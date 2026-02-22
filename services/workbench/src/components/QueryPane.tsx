import React, { useState } from 'react';
import { SchemeEditor } from './SchemeEditor';
import { QueryTab, Theme } from '../types/workbench';
import { modKey } from '../utils/theme';
import { prettifyScheme } from '../utils/scheme';

interface QueryPaneProps {
  tabs: QueryTab[];
  activeTabId: string;
  theme: Theme;
  vimMode: boolean;
  isLoading: boolean;
  onTabChange: (tabId: string) => void;
  onTabContentChange: (tabId: string, content: string) => void;
  onAddTab: () => void;
  onCloseTab: (tabId: string) => void;
  onRunQuery: () => void;
}

export const QueryPane: React.FC<QueryPaneProps> = ({
  tabs,
  activeTabId,
  theme,
  vimMode,
  isLoading,
  onTabChange,
  onTabContentChange,
  onAddTab,
  onCloseTab,
  onRunQuery,
}) => {
  const [copiedQuery, setCopiedQuery] = useState(false);

  const currentTab = tabs.find((t) => t.id === activeTabId);

  const handleCopyQuery = async () => {
    if (!currentTab) return;

    try {
      await navigator.clipboard.writeText(currentTab.content);
      setCopiedQuery(true);
      setTimeout(() => setCopiedQuery(false), 2000);
    } catch (err) {
      console.error('Failed to copy:', err);
    }
  };

  const handleDownloadQuery = () => {
    if (!currentTab) return;

    const blob = new Blob([currentTab.content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `${currentTab.name}.scm`;
    a.click();
    URL.revokeObjectURL(url);
  };

  const handlePrettifyQuery = () => {
    if (!currentTab || !currentTab.content.trim()) return;

    try {
      const prettified = prettifyScheme(currentTab.content);
      onTabContentChange(activeTabId, prettified);
    } catch (err) {
      console.error('Failed to prettify:',  err);
    }
  };

  return (
    <>
      <div className="tabs query-tabs">
        {tabs.map((tab) => (
          <div key={tab.id} className={`query-tab ${activeTabId === tab.id ? 'active' : ''}`}>
            <span className="query-tab-label" onClick={() => onTabChange(tab.id)}>
              {tab.name}
            </span>
            {tabs.length > 1 && (
              <button
                className="query-tab-close"
                onClick={(e) => {
                  e.stopPropagation();
                  onCloseTab(tab.id);
                }}
              >
                ×
              </button>
            )}
          </div>
        ))}
        <button className="button button-icon add-tab-button" onClick={onAddTab} title="New tab">
          +
        </button>
        <div className="query-actions">
          <span className="keyboard-hint">{modKey}+Enter to run</span>
          <button
            className="button button-icon"
            onClick={handlePrettifyQuery}
            title="Prettify code"
            disabled={!currentTab?.content.trim()}
          >
            ✨
          </button>
          <button
            className="button button-icon"
            onClick={handleCopyQuery}
            title={copiedQuery ? 'Copied!' : 'Copy query'}
            disabled={!currentTab?.content.trim()}
          >
            {copiedQuery ? '✓' : '📋'}
          </button>
          <button className="button button-icon" onClick={handleDownloadQuery} title="Download query">
            ⬇
          </button>
          <button
            className="button button-primary run-button"
            onClick={onRunQuery}
            disabled={isLoading || !currentTab?.content.trim()}
          >
            {isLoading ? <span className="loading-spinner" /> : '▶ Run'}
          </button>
        </div>
      </div>
      <div className="query-editor">
        <SchemeEditor
          value={currentTab?.content || ''}
          onChange={(value) => onTabContentChange(activeTabId, value)}
          theme={theme}
          placeholder="Enter your Scheme query here..."
          language="scheme"
          onRun={onRunQuery}
          vimMode={vimMode}
        />
      </div>
    </>
  );
};
