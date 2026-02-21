import React, { useState, useEffect, useRef, useCallback } from 'react';
import './App.css';
import ToolBar from './components/ToolBar';
import LeftPane from './components/LeftPane';
import MiddlePane from './components/MiddlePane';
import RightPane from './components/RightPane';
import { JournalService } from './services/JournalService';
import { AppState, JournalPath } from './types';

// Get environment variable from runtime config or build-time env
const getEnvVar = (key: string): string => {
  // @ts-ignore - window._env_ is injected at runtime
  if (window._env_?.[key]) {
    // @ts-ignore
    return window._env_[key];
  }
  return process.env[`REACT_APP_${key}`] || '';
};

// Encode a JournalPath to a URL-safe string
const encodePathToHash = (path: JournalPath): string => {
  return encodeURIComponent(JSON.stringify(path));
};

// Decode a URL hash to a JournalPath
const decodeHashToPath = (hash: string): JournalPath | null => {
  if (!hash || hash === '#') return null;
  try {
    const decoded = decodeURIComponent(hash.startsWith('#') ? hash.slice(1) : hash);
    if (!decoded) return null;
    const parsed = JSON.parse(decoded);
    if (Array.isArray(parsed)) {
      return parsed as JournalPath;
    }
    return null;
  } catch {
    return null;
  }
};

// Generate expanded nodes set from a path
// This ensures all parent nodes are expanded so the selected path is visible
const generateExpandedNodesFromPath = (path: JournalPath): Set<string> => {
  const expanded = new Set<string>();
  
  // Build up partial paths and add them to expanded set
  for (let i = 1; i <= path.length; i++) {
    const partialPath = path.slice(0, i);
    expanded.add(JSON.stringify(partialPath));
  }
  
  return expanded;
};

// Get initial theme from localStorage or system preference
const getInitialTheme = (): 'light' | 'dark' => {
  const stored = localStorage.getItem('theme');
  if (stored === 'light' || stored === 'dark') {
    return stored;
  }
  // Check system preference
  if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
    return 'dark';
  }
  return 'light';
};

const App: React.FC = () => {
  // Initialize state, checking URL hash for initial path
  const getInitialState = (): AppState => {
    const initialPath = decodeHashToPath(window.location.hash);
    const initialExpanded = initialPath ? generateExpandedNodesFromPath(initialPath) : new Set<string>();
    
    return {
      endpoint: getEnvVar('SYNC_EXPLORER_ENDPOINT'),
      authentication: getEnvVar('SYNC_EXPLORER_PASSWORD'),
      rootIndex: 0,
      selectedPath: initialPath,
      expandedNodes: initialExpanded,
      isLoading: false,
      error: null,
    };
  };

  const [appState, setAppState] = useState<AppState>(getInitialState);
  const [theme, setTheme] = useState<'light' | 'dark'>(getInitialTheme);

  const [journalService, setJournalService] = useState<JournalService | null>(null);
  const [leftPaneWidth, setLeftPaneWidth] = useState(300);
  const [rightPaneWidth, setRightPaneWidth] = useState(350);
  const [isResizing, setIsResizing] = useState<'left' | 'right' | null>(null);
  const containerRef = useRef<HTMLDivElement>(null);
  
  // Track if we're updating the hash programmatically to avoid loops
  const isUpdatingHash = useRef(false);
  
  // Track if initial sync has been done
  const hasInitialSync = useRef(false);

  // Apply theme to document
  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme);
    localStorage.setItem('theme', theme);
  }, [theme]);

  const toggleTheme = () => {
    setTheme(prev => prev === 'light' ? 'dark' : 'light');
  };

  useEffect(() => {
    if (appState.endpoint && appState.authentication) {
      const service = new JournalService(appState.endpoint, appState.authentication);
      setJournalService(service);
    }
  }, [appState.endpoint, appState.authentication]);

  // Auto-synchronize on initial load when journalService is ready
  useEffect(() => {
    if (journalService && !hasInitialSync.current) {
      hasInitialSync.current = true;
      performSync(journalService);
    }
  }, [journalService]);

  const performSync = async (service: JournalService) => {
    setAppState(prev => ({ ...prev, isLoading: true, error: null }));
    
    try {
      const size = await service.getSize();
      setAppState(prev => ({ 
        ...prev,
        rootIndex: size - 1,
        isLoading: false,
        error: null,
      }));
    } catch (error) {
      console.error('Synchronization failed:', error);
      setAppState(prev => ({ 
        ...prev,
        isLoading: false, 
        error: `Synchronization failed: ${error instanceof Error ? error.message : 'Unknown error'}` 
      }));
    }
  };

  // Update URL hash when selectedPath changes
  useEffect(() => {
    const newHash = appState.selectedPath ? '#' + encodePathToHash(appState.selectedPath) : '';
    if (window.location.hash !== newHash) {
      isUpdatingHash.current = true;
      if (newHash) {
        window.history.pushState(null, '', newHash);
      } else {
        window.history.pushState(null, '', window.location.pathname + window.location.search);
      }
      isUpdatingHash.current = false;
    }
  }, [appState.selectedPath]);

  // Handle browser back/forward navigation
  const handleHashChange = useCallback(() => {
    if (isUpdatingHash.current) return;
    
    const path = decodeHashToPath(window.location.hash);
    const expanded = path ? generateExpandedNodesFromPath(path) : new Set<string>();
    
    setAppState(prev => {
      if (path) {
        const mergedExpanded = new Set(Array.from(prev.expandedNodes).concat(Array.from(expanded)));
        return {
          ...prev,
          selectedPath: path,
          expandedNodes: mergedExpanded,
        };
      } else {
        return {
          ...prev,
          selectedPath: path,
        };
      }
    });
  }, []);

  useEffect(() => {
    window.addEventListener('hashchange', handleHashChange);
    window.addEventListener('popstate', handleHashChange);
    return () => {
      window.removeEventListener('hashchange', handleHashChange);
      window.removeEventListener('popstate', handleHashChange);
    };
  }, [handleHashChange]);

  useEffect(() => {
    if (!isResizing) return;

    const handleMouseMove = (e: MouseEvent) => {
      if (!containerRef.current) return;
      
      const containerRect = containerRef.current.getBoundingClientRect();
      
      if (isResizing === 'left') {
        setLeftPaneWidth(Math.max(100, e.clientX - containerRect.left));
      } else {
        setRightPaneWidth(Math.max(100, containerRect.right - e.clientX));
      }
    };

    const handleMouseUp = () => setIsResizing(null);

    document.addEventListener('mousemove', handleMouseMove);
    document.addEventListener('mouseup', handleMouseUp);
    document.body.style.cursor = 'col-resize';
    document.body.style.userSelect = 'none';

    return () => {
      document.removeEventListener('mousemove', handleMouseMove);
      document.removeEventListener('mouseup', handleMouseUp);
      document.body.style.cursor = '';
      document.body.style.userSelect = '';
    };
  }, [isResizing]);

  const updateAppState = (updates: Partial<AppState>) => {
    setAppState(prev => ({ ...prev, ...updates }));
  };

  const handleSynchronize = async () => {
    if (!journalService) return;
    await performSync(journalService);
  };

  const handlePathSelect = (path: JournalPath) => {
    updateAppState({ selectedPath: path });
  };

  return (
    <div className="app">
      <ToolBar
        endpoint={appState.endpoint}
        authentication={appState.authentication}
        rootIndex={appState.rootIndex}
        isLoading={appState.isLoading}
        error={appState.error}
        theme={theme}
        onEndpointChange={(endpoint) => updateAppState({ endpoint })}
        onAuthenticationChange={(authentication) => updateAppState({ authentication })}
        onSynchronize={handleSynchronize}
        onThemeToggle={toggleTheme}
      />
      <div className="main-content" ref={containerRef}>
        <div className="left-pane pane" style={{ width: `${leftPaneWidth}px` }}>
          <LeftPane
            appState={appState}
            journalService={journalService}
            onPathSelect={handlePathSelect}
            onExpandedNodesChange={(expandedNodes) => updateAppState({ expandedNodes })}
          />
        </div>
        <div 
          className="resize-handle resize-handle-left"
          onMouseDown={() => setIsResizing('left')}
        />
        <div className="middle-pane pane" style={{ flex: 1 }}>
          <MiddlePane
            appState={appState}
            journalService={journalService}
            onContentUpdate={handleSynchronize}
          />
        </div>
        <div 
          className="resize-handle resize-handle-right"
          onMouseDown={() => setIsResizing('right')}
        />
        <div className="right-pane pane" style={{ width: `${rightPaneWidth}px` }}>
          <RightPane
            appState={appState}
            journalService={journalService}
            onPathUpdate={handlePathSelect}
          />
        </div>
      </div>
    </div>
  );
};

export default App;
