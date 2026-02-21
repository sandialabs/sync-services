import React, { useState, useEffect, useRef } from 'react';
import './RightPane.css';
import { AppState, JournalPath, HistoryEntry } from '../types';
import { JournalService } from '../services/JournalService';

interface RightPaneProps {
  appState: AppState;
  journalService: JournalService | null;
  onPathUpdate: (path: JournalPath) => void;
}

/**
 * Build a path with a specific version offset for a given tab index
 */
const buildVersionPath = (
  basePath: JournalPath,
  tabIndex: number,
  versionOffset: number
): JournalPath => {
  if (tabIndex === 0) {
    // For the local journal
    if (Array.isArray(basePath[0])) {
      // Path starts with a list (staged) - prepend the version offset
      return [versionOffset, ...basePath];
    }
    // Path already starts with a number - replace it
    return [versionOffset, ...basePath.slice(1)];
  }

  // For peer journals - update the appropriate index in the path
  const modifiedPath = [...basePath];
  let peerCount = 0;

  for (let i = 0; i < modifiedPath.length; i++) {
    if (typeof modifiedPath[i] === 'number') {
      peerCount++;
      if (peerCount === tabIndex + 1) {
        modifiedPath[i] = versionOffset;
        break;
      }
    }
  }

  return modifiedPath;
};

/**
 * Get the version index at a specific tab position in the path
 */
const getVersionAtTab = (path: JournalPath, tabIndex: number): number | null => {
  if (tabIndex === 0) {
    const firstElement = path[0];
    return typeof firstElement === 'number' ? firstElement : null;
  }

  let peerCount = 0;
  for (const segment of path) {
    if (typeof segment === 'number') {
      peerCount++;
      if (peerCount === tabIndex + 1) {
        return segment;
      }
    }
  }
  return null;
};

/**
 * Extract the "base" path by removing version numbers, for comparison purposes.
 * This helps determine if we're looking at the same document or a different one.
 */
const getBasePath = (path: JournalPath): string => {
  // Filter out numbers and stringify for comparison
  const filtered = path.filter(segment => Array.isArray(segment));
  return JSON.stringify(filtered);
};

const RightPane: React.FC<RightPaneProps> = ({
  appState,
  journalService,
  onPathUpdate,
}) => {
  const [historyTabs, setHistoryTabs] = useState<string[]>([]);
  const [activeTab, setActiveTab] = useState(0);
  const [histories, setHistories] = useState<Map<string, HistoryEntry[]>>(new Map());
  
  // Track the base path to detect when we switch to a different document
  const previousBasePath = useRef<string | null>(null);

  const { selectedPath } = appState;

  useEffect(() => {
    if (!selectedPath) return;

    const currentBasePath = getBasePath(selectedPath);
    
    // Only reset histories if we're looking at a different document
    const shouldResetHistories = previousBasePath.current !== null && 
                                  previousBasePath.current !== currentBasePath;
    
    previousBasePath.current = currentBasePath;

    // Extract journal names from the path
    const tabs: string[] = ['Self'];
    
    for (let i = 1; i < selectedPath.length - 1; i += 2) {
      const segment = selectedPath[i];
      if (Array.isArray(segment) && segment[0] === '*peer*' && segment[2] === 'chain') {
        tabs.push(segment[1]);
      }
    }

    setHistoryTabs(tabs);
    
    // Only reset if switching to a different document
    if (shouldResetHistories) {
      setActiveTab(0);
      setHistories(new Map());
    }
  }, [selectedPath]);

  const getNextVersionOffset = (currentHistory: HistoryEntry[]): number => {
    if (currentHistory.length === 0) return -1;
    
    const minIndex = Math.min(...currentHistory.map(h => h.index));
    return minIndex === -1 ? -2 : -(Math.abs(minIndex) * 2);
  };

  const loadHistoryVersion = async (tabIndex: number, versionOffset: number) => {
    if (!journalService || !selectedPath) return;

    const tabKey = historyTabs[tabIndex];
    const currentHistory = histories.get(tabKey) || [];
    const modifiedPath = buildVersionPath(selectedPath, tabIndex, versionOffset);

    try {
      const response = await journalService.get(modifiedPath);
      
      const newEntry: HistoryEntry = {
        index: versionOffset,
        content: response.content,
        path: modifiedPath,
      };

      const updatedHistory = [...currentHistory, newEntry].sort((a, b) => b.index - a.index);
      setHistories(new Map(histories.set(tabKey, updatedHistory)));
    } catch (error) {
      const errorEntry: HistoryEntry = {
        index: versionOffset,
        content: `Error: ${error instanceof Error ? error.message : 'Failed to load'}`,
        path: modifiedPath,
      };
      
      const updatedHistory = [...currentHistory, errorEntry].sort((a, b) => b.index - a.index);
      setHistories(new Map(histories.set(tabKey, updatedHistory)));
    }
  };

  const handleEntryClick = (entry: HistoryEntry) => {
    if (!selectedPath) return;
    const historicalPath = buildVersionPath(selectedPath, activeTab, entry.index);
    onPathUpdate(historicalPath);
  };

  const handleStagedClick = () => {
    if (!selectedPath) return;
    // Build a staged path by removing the leading number if present
    const firstElement = selectedPath[0];
    if (typeof firstElement === 'number') {
      // Remove the version number to get the staged path
      onPathUpdate(selectedPath.slice(1));
    }
    // If already staged, do nothing
  };

  const isCurrentVersion = (entry: HistoryEntry): boolean => {
    if (!selectedPath) return false;
    const currentVersion = getVersionAtTab(selectedPath, activeTab);
    return currentVersion === entry.index;
  };

  const isViewingStaged = selectedPath && Array.isArray(selectedPath[0]);
  const currentHistory = histories.get(historyTabs[activeTab]) || [];

  const formatEntryPreview = (content: any): string => {
    const { value } = JournalService.extractSchemeValue(content);
    return JSON.stringify(value).substring(0, 100) + '...';
  };

  return (
    <div className="pane right-pane">
      <div className="tabs history-tabs">
        {historyTabs.map((tab, index) => (
          <button
            key={index}
            className={`tab ${activeTab === index ? 'active' : ''}`}
            onClick={() => setActiveTab(index)}
          >
            {tab}
          </button>
        ))}
      </div>
      
      <div className="tab-content">
        {historyTabs.length > 0 ? (
          <div className="history-list">
            {activeTab === 0 && (
              <div 
                className={`history-entry ${isViewingStaged ? 'active' : ''}`} 
                style={{ borderColor: 'var(--green)' }}
                onClick={handleStagedClick}
              >
                <div className="history-index">Version: Staged</div>
                <div className="history-preview">
                  {isViewingStaged ? 'Currently viewing staged version' : 'Click to view staged version'}
                </div>
              </div>
            )}
            
            {currentHistory.map((entry, index) => (
              <div 
                key={index} 
                className={`history-entry ${isCurrentVersion(entry) ? 'active' : ''}`}
                onClick={() => handleEntryClick(entry)}
              >
                <div className="history-index">
                  Version {entry.index === -1 ? 'Latest' : entry.index}
                </div>
                <div className="history-preview">
                  {formatEntryPreview(entry.content)}
                </div>
              </div>
            ))}
            
            <button
              className="button button-secondary load-more"
              onClick={() => loadHistoryVersion(activeTab, getNextVersionOffset(currentHistory))}
            >
              Load Older Version ({getNextVersionOffset(currentHistory)})
            </button>
          </div>
        ) : (
          <div className="empty-state">
            Select a document to view its history
          </div>
        )}
      </div>
    </div>
  );
};

export default RightPane;
