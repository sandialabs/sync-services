import React, { useState, useEffect } from 'react';
import { AppState } from '../types';
import { JournalService } from '../services/JournalService';

interface VerificationTabProps {
  appState: AppState;
  journalService: JournalService | null;
}

const VerificationTab: React.FC<VerificationTabProps> = ({
  appState,
  journalService,
}) => {
  const [proof, setProof] = useState<any>(null);
  const [isLoading, setIsLoading] = useState(false);

  useEffect(() => {
    if (journalService && appState.selectedPath) {
      loadProof();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [journalService, appState.selectedPath]);

  const loadProof = async () => {
    if (!journalService || !appState.selectedPath) return;

    setIsLoading(true);
    try {
      const response = await journalService.get(appState.selectedPath);
      setProof(response.proof);
    } catch (error) {
      console.error('Failed to load proof:', error);
      setProof({ error: `Failed to load proof: ${error instanceof Error ? error.message : 'Unknown error'}` });
    } finally {
      setIsLoading(false);
    }
  };

  const syntaxHighlight = (json: string): string => {
    return json.replace(
      /("(\\u[a-zA-Z0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\b(true|false|null)\b|-?\d+(?:\.\d*)?(?:[eE][+-]?\d+)?)/g,
      (match) => {
        let cls = 'json-number';
        if (/^"/.test(match)) {
          if (/:$/.test(match)) {
            cls = 'json-key';
          } else {
            cls = 'json-string';
          }
        } else if (/true|false/.test(match)) {
          cls = 'json-boolean';
        } else if (/null/.test(match)) {
          cls = 'json-null';
        }
        return `<span class="${cls}">${match}</span>`;
      }
    );
  };

  return (
    <div className="proof-viewer">
      {isLoading ? (
        <div className="loading-spinner" />
      ) : proof ? (
        <div 
          className="proof-content"
          dangerouslySetInnerHTML={{
            __html: syntaxHighlight(JSON.stringify(proof, null, 2))
          }}
        />
      ) : (
        <div className="empty-state">
          Select a document to view its cryptographic proof
        </div>
      )}
    </div>
  );
};

export default VerificationTab;
