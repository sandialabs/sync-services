import React, { useState, useEffect, useRef, useCallback } from 'react';
import { WorkbenchToolBar } from './components/WorkbenchToolBar';
import { WorkbenchHelpModal } from './components/WorkbenchHelpModal';
import { WorkbenchLeftPane } from './components/WorkbenchLeftPane';
import { QueryPane } from './components/QueryPane';
import { OutputPane } from './components/OutputPane';
import { HistoryPane } from './components/HistoryPane';
import { QueryTab, HistoryEntry, Theme } from './types/workbench';
import { getInitialTheme, getInitialVimMode, applyTheme } from './utils/theme';
import { executeQuery } from './utils/api';
import { prettifyScheme } from './utils/scheme';
import './App.css';

// Get environment variable from runtime config or build-time env
const getEnvVar = (key: string): string => {
  // @ts-ignore - window._env_ is injected at runtime
  if (window._env_?.[key]) {
    // @ts-ignore
    return window._env_[key];
  }
  return process.env[`REACT_APP_${key}`] || '';
};

const JOURNAL_ENDPOINT = getEnvVar('SYNC_WORKBENCH_ENDPOINT') || 'http://localhost:4096/interface';

const App: React.FC = () => {
  const [theme, setTheme] = useState<Theme>(getInitialTheme);
  const [vimMode, setVimMode] = useState<boolean>(getInitialVimMode);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [showHelp, setShowHelp] = useState(false);

  // Query tabs state
  const [queryTabs, setQueryTabs] = useState<QueryTab[]>([
    { id: '1', name: 'Query 1', content: '' },
  ]);
  const [activeQueryTab, setActiveQueryTab] = useState('1');
  const [nextTabNumber, setNextTabNumber] = useState(2);

  // History state
  const [history, setHistory] = useState<HistoryEntry[]>([]);
  const [selectedHistoryEntry, setSelectedHistoryEntry] = useState<HistoryEntry | null>(null);

  // Pane dimensions
  const [leftPaneWidth, setLeftPaneWidth] = useState(250);
  const [rightPaneWidth, setRightPaneWidth] = useState(280);
  const [topPaneFlex, setTopPaneFlex] = useState(1);
  const [bottomPaneFlex, setBottomPaneFlex] = useState(1);
  const [isResizing, setIsResizing] = useState<'left' | 'right' | 'horizontal' | null>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  const middleRef = useRef<HTMLDivElement>(null);

  // Apply theme on change
  useEffect(() => {
    applyTheme(theme);
  }, [theme]);

  // Persist vim mode to localStorage
  useEffect(() => {
    localStorage.setItem('workbench-vim-mode', String(vimMode));
  }, [vimMode]);

  const toggleTheme = () => {
    setTheme((prev) => (prev === 'light' ? 'dark' : 'light'));
  };

  const toggleVimMode = () => {
    setVimMode((prev) => !prev);
  };

  // Handle resizing
  useEffect(() => {
    if (!isResizing) return;

    const handleMouseMove = (e: MouseEvent) => {
      if (!containerRef.current) return;

      const containerRect = containerRef.current.getBoundingClientRect();

      if (isResizing === 'left') {
        setLeftPaneWidth(Math.max(150, Math.min(400, e.clientX - containerRect.left)));
      } else if (isResizing === 'right') {
        setRightPaneWidth(Math.max(150, Math.min(400, containerRect.right - e.clientX)));
      } else if (isResizing === 'horizontal' && middleRef.current) {
        const middleRect = middleRef.current.getBoundingClientRect();
        const totalHeight = middleRect.height - 4; // subtract resize handle height
        const topHeight = e.clientY - middleRect.top;
        const bottomHeight = totalHeight - topHeight;

        if (topHeight > 100 && bottomHeight > 100) {
          const newTopFlex = topHeight / totalHeight;
          const newBottomFlex = bottomHeight / totalHeight;
          setTopPaneFlex(newTopFlex);
          setBottomPaneFlex(newBottomFlex);
        }
      }
    };

    const handleMouseUp = () => setIsResizing(null);

    document.addEventListener('mousemove', handleMouseMove);
    document.addEventListener('mouseup', handleMouseUp);
    document.body.style.cursor = isResizing === 'horizontal' ? 'row-resize' : 'col-resize';
    document.body.style.userSelect = 'none';

    return () => {
      document.removeEventListener('mousemove', handleMouseMove);
      document.removeEventListener('mouseup', handleMouseUp);
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    };
  }, [isResizing]);

  const handleRunQuery = useCallback(async () => {
    const currentTab = queryTabs.find((t) => t.id === activeQueryTab);
    if (!currentTab || !currentTab.content.trim()) return;

    setIsLoading(true);
    setError(null);

    try {
      const result = await executeQuery(JOURNAL_ENDPOINT, currentTab.content);

      const historyEntry: HistoryEntry = {
        id: crypto.randomUUID(),
        timestamp: new Date(),
        query: currentTab.content,
        request: result.request,
        response: result.response,
        result: result.result,
        error: result.error,
      };

      setHistory((prev) => [historyEntry, ...prev]);
      setSelectedHistoryEntry(historyEntry);

      if (result.error) {
        setError(result.error);
      }
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Unknown error';
      setError(errorMessage);
    } finally {
      setIsLoading(false);
    }
  }, [queryTabs, activeQueryTab]);

  const handleTabChange = (tabId: string) => {
    setActiveQueryTab(tabId);
  };

  const handleTabContentChange = (tabId: string, content: string) => {
    setQueryTabs((prev) => prev.map((t) => (t.id === tabId ? { ...t, content } : t)));
  };

  const handleAddTab = () => {
    const newId = String(Date.now());
    const newTab: QueryTab = {
      id: newId,
      name: `Query ${nextTabNumber}`,
      content: '',
    };
    setQueryTabs((prev) => [...prev, newTab]);
    setActiveQueryTab(newId);
    setNextTabNumber((prev) => prev + 1);
  };

  const handleCloseTab = (tabId: string) => {
    if (queryTabs.length === 1) return;

    const index = queryTabs.findIndex((t) => t.id === tabId);
    const newTabs = queryTabs.filter((t) => t.id !== tabId);
    setQueryTabs(newTabs);

    if (activeQueryTab === tabId) {
      const newIndex = Math.min(index, newTabs.length - 1);
      setActiveQueryTab(newTabs[newIndex].id);
    }
  };

  const handleUseExample = (code: string) => {
    // Prettify the code before inserting it into the editor
    try {
      const prettified = prettifyScheme(code);
      handleTabContentChange(activeQueryTab, prettified);
    } catch (err) {
      // If prettification fails, use the original code
      console.error('Failed to prettify example:', err);
      handleTabContentChange(activeQueryTab, code);
    }
  };

  const handleLogoClick = () => {
    window.location.href = window.location.pathname + window.location.search;
  };

  const handleHistorySelect = (entry: HistoryEntry) => {
    setSelectedHistoryEntry(entry);
  };

  return (
    <div className="app">
      {/* Toolbar */}
      <WorkbenchToolBar
        theme={theme}
        vimMode={vimMode}
        isLoading={isLoading}
        error={error}
        onThemeToggle={toggleTheme}
        onVimModeToggle={toggleVimMode}
        onHelpClick={() => setShowHelp(true)}
        onLogoClick={handleLogoClick}
      />

      {/* Main content */}
      <div className="main-content" ref={containerRef}>
        {/* Left pane */}
        <div className="left-pane pane" style={{ width: `${leftPaneWidth}px` }}>
          <WorkbenchLeftPane onUseExample={handleUseExample} />
        </div>

        <div className="resize-handle resize-handle-vertical" onMouseDown={() => setIsResizing('left')} />

        {/* Middle section */}
        <div className="middle-section" ref={middleRef}>
          {/* Top pane (Input) */}
          <div className="top-pane pane" style={{ flex: topPaneFlex }}>
            <QueryPane
              tabs={queryTabs}
              activeTabId={activeQueryTab}
              theme={theme}
              vimMode={vimMode}
              isLoading={isLoading}
              onTabChange={handleTabChange}
              onTabContentChange={handleTabContentChange}
              onAddTab={handleAddTab}
              onCloseTab={handleCloseTab}
              onRunQuery={handleRunQuery}
            />
          </div>

          <div className="resize-handle resize-handle-horizontal" onMouseDown={() => setIsResizing('horizontal')} />

          {/* Bottom pane (Output) */}
          <div className="bottom-pane pane" style={{ flex: bottomPaneFlex }}>
            <OutputPane
              selectedEntry={selectedHistoryEntry}
              theme={theme}
              vimMode={vimMode}
            />
          </div>
        </div>

        <div className="resize-handle resize-handle-vertical" onMouseDown={() => setIsResizing('right')} />

        {/* Right pane (History) */}
        <div className="right-pane pane" style={{ width: `${rightPaneWidth}px` }}>
          <HistoryPane
            history={history}
            selectedEntry={selectedHistoryEntry}
            onSelectEntry={handleHistorySelect}
          />
        </div>
      </div>

      {/* Help Modal */}
      {showHelp && <WorkbenchHelpModal onClose={() => setShowHelp(false)} />}
    </div>
  );
};

export default App;
