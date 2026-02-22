import React, { useState } from 'react';
import { SchemeEditor } from './SchemeEditor';
import { HistoryEntry, BottomPaneTab, Theme } from '../types/workbench';

interface OutputPaneProps {
  selectedEntry: HistoryEntry | null;
  theme: Theme;
  vimMode: boolean;
}

export const OutputPane: React.FC<OutputPaneProps> = ({
  selectedEntry,
  theme,
  vimMode,
}) => {
  const [activeTab, setActiveTab] = useState<BottomPaneTab>('result');
  const [copiedOutput, setCopiedOutput] = useState(false);

  const getOutputContent = (): string => {
    if (!selectedEntry) return '';

    switch (activeTab) {
      case 'query':
        return selectedEntry.query;
      case 'result':
        if (selectedEntry.error) {
          return selectedEntry.error;
        }
        return typeof selectedEntry.result === 'string'
          ? selectedEntry.result
          : JSON.stringify(selectedEntry.result, null, 2);
      case 'request':
        return selectedEntry.request;
      case 'response':
        return selectedEntry.response;
      default:
        return '';
    }
  };

  const handleCopyOutput = async () => {
    const content = getOutputContent();
    if (!content) return;

    try {
      await navigator.clipboard.writeText(content);
      setCopiedOutput(true);
      setTimeout(() => setCopiedOutput(false), 2000);
    } catch (err) {
      console.error('Failed to copy:', err);
    }
  };

  const handleDownloadOutput = () => {
    if (!selectedEntry) return;

    let content: string;
    let filename: string;

    switch (activeTab) {
      case 'query':
        content = selectedEntry.query;
        filename = 'query.scm';
        break;
      case 'result':
        content =
          typeof selectedEntry.result === 'string'
            ? selectedEntry.result
            : JSON.stringify(selectedEntry.result, null, 2);
        filename = 'result.json';
        break;
      case 'request':
        content = selectedEntry.request;
        filename = 'request.txt';
        break;
      case 'response':
        content = selectedEntry.response;
        filename = 'response.txt';
        break;
      default:
        return;
    }

    const blob = new Blob([content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    URL.revokeObjectURL(url);
  };

  const renderContent = () => {
    if (!selectedEntry) {
      return <div className="empty-state">Run a query to see results</div>;
    }

    switch (activeTab) {
      case 'query':
        return (
          <SchemeEditor
            value={selectedEntry.query}
            theme={theme}
            readOnly={true}
            language="scheme"
            vimMode={vimMode}
          />
        );
      case 'result':
        if (selectedEntry.error) {
          return (
            <SchemeEditor
              value={selectedEntry.error}
              theme={theme}
              readOnly={true}
              language="text"
              vimMode={vimMode}
            />
          );
        }
        const resultContent =
          typeof selectedEntry.result === 'string'
            ? selectedEntry.result
            : JSON.stringify(selectedEntry.result, null, 2);
        return (
          <SchemeEditor
            value={resultContent}
            theme={theme}
            readOnly={true}
            language="json"
            vimMode={vimMode}
          />
        );
      case 'request':
        return (
          <SchemeEditor
            value={selectedEntry.request}
            theme={theme}
            readOnly={true}
            language="text"
            vimMode={vimMode}
          />
        );
      case 'response':
        return (
          <SchemeEditor
            value={selectedEntry.response}
            theme={theme}
            readOnly={true}
            language="text"
            vimMode={vimMode}
          />
        );
      default:
        return null;
    }
  };

  return (
    <>
      <div className="tabs output-tabs">
        <button
          className={`tab ${activeTab === 'query' ? 'active' : ''}`}
          onClick={() => setActiveTab('query')}
        >
          Query
        </button>
        <button
          className={`tab ${activeTab === 'result' ? 'active' : ''}`}
          onClick={() => setActiveTab('result')}
        >
          Result
        </button>
        <button
          className={`tab ${activeTab === 'request' ? 'active' : ''}`}
          onClick={() => setActiveTab('request')}
        >
          Request
        </button>
        <button
          className={`tab ${activeTab === 'response' ? 'active' : ''}`}
          onClick={() => setActiveTab('response')}
        >
          Response
        </button>
        <div className="output-actions">
          <button
            className="button button-icon"
            onClick={handleCopyOutput}
            disabled={!selectedEntry}
            title={copiedOutput ? 'Copied!' : 'Copy output'}
          >
            {copiedOutput ? '✓' : '📋'}
          </button>
          <button
            className="button button-icon"
            onClick={handleDownloadOutput}
            disabled={!selectedEntry}
            title="Download output"
          >
            ⬇
          </button>
        </div>
      </div>
      <div className="output-viewer">{renderContent()}</div>
    </>
  );
};
