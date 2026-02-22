import React from 'react';
import { HistoryEntry } from '../types/workbench';

interface HistoryPaneProps {
  history: HistoryEntry[];
  selectedEntry: HistoryEntry | null;
  onSelectEntry: (entry: HistoryEntry) => void;
}

export const HistoryPane: React.FC<HistoryPaneProps> = ({
  history,
  selectedEntry,
  onSelectEntry,
}) => {
  const formatTimestamp = (date: Date): string => {
    return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' });
  };

  const getQueryPreview = (query: string): string => {
    const firstLine = query.trim().split('\n')[0];
    return firstLine.length > 40 ? firstLine.substring(0, 40) + '...' : firstLine;
  };

  return (
    <>
      <div className="history-header">
        <h3>History</h3>
      </div>
      <div className="history-list">
        {history.length === 0 ? (
          <div className="empty-state">No queries executed yet</div>
        ) : (
          history.map((entry) => (
            <div
              key={entry.id}
              className={`history-entry ${selectedEntry?.id === entry.id ? 'selected' : ''} ${entry.error ? 'error' : ''}`}
              onClick={() => onSelectEntry(entry)}
            >
              <div className="history-entry-time">{formatTimestamp(entry.timestamp)}</div>
              <div className="history-entry-preview">{getQueryPreview(entry.query)}</div>
              {entry.error && <div className="history-entry-status">✗</div>}
              {!entry.error && <div className="history-entry-status success">✓</div>}
            </div>
          ))
        )}
      </div>
    </>
  );
};
