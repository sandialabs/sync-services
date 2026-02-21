import React, { useState, useEffect } from 'react';
import { AppState, DocumentContent, JournalPath } from '../types';
import { JournalService } from '../services/JournalService';

interface ContentTabProps {
  appState: AppState;
  journalService: JournalService | null;
  onContentUpdate: () => void;
}

const ContentTab: React.FC<ContentTabProps> = ({
  appState,
  journalService,
  onContentUpdate,
}) => {
  const [content, setContent] = useState<DocumentContent | null>(null);
  const [isEditing, setIsEditing] = useState(false);
  const [editValue, setEditValue] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  const { selectedPath } = appState;

  useEffect(() => {
    if (journalService && selectedPath) {
      loadContent();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [journalService, selectedPath]);

  const loadContent = async () => {
    if (!journalService || !selectedPath) return;

    setIsLoading(true);
    setContent(null);
    
    try {
      const response = await journalService.get(selectedPath);
      
      // Determine if pinned - the field contains a path array when pinned,
      // or null/false/undefined when not pinned
      const pinnedValue = response['pinned?'];
      const isPinned = Array.isArray(pinnedValue) && pinnedValue.length > 0;
      
      setContent({
        path: selectedPath,
        content: response.content,
        isPinned,
        proof: response.proof,
      });
      
      const { value } = JournalService.extractSchemeValue(response.content);
      setEditValue(typeof value === 'string' ? value : JSON.stringify(value, null, 2));
    } catch (error) {
      setContent({
        path: selectedPath,
        content: `Error loading content: ${error instanceof Error ? error.message : 'Unknown error'}`,
        isPinned: false,
        proof: null,
      });
    } finally {
      setIsLoading(false);
    }
  };

  const handleSave = async () => {
    if (!journalService || !selectedPath) return;

    setIsLoading(true);
    try {
      let valueToSave: any;
      try {
        valueToSave = JSON.parse(editValue);
      } catch {
        valueToSave = { '*type/string*': editValue };
      }

      await journalService.set(selectedPath, valueToSave);
      setIsEditing(false);
      // Reload content to show the saved value without resetting the tree
      await loadContent();
    } catch (error) {
      alert(`Failed to save: ${error instanceof Error ? error.message : 'Unknown error'}`);
    } finally {
      setIsLoading(false);
    }
  };

  const handlePin = async () => {
    if (!journalService || !selectedPath || !content) return;

    const wasPinned = content.isPinned;
    setIsLoading(true);
    
    try {
      let success: boolean;
      if (wasPinned) {
        success = await journalService.unpin(selectedPath);
      } else {
        success = await journalService.pin(selectedPath);
      }
      
      if (success) {
        // Optimistically update the pinned state
        setContent(prev => prev ? { ...prev, isPinned: !wasPinned } : null);
      } else {
        alert(`Failed to ${wasPinned ? 'unpin' : 'pin'}: Operation returned false`);
      }
    } catch (error) {
      alert(`Failed to ${wasPinned ? 'unpin' : 'pin'}: ${error instanceof Error ? error.message : 'Unknown error'}`);
    } finally {
      setIsLoading(false);
    }
  };

  const getPathInfo = (path: JournalPath | null) => {
    if (!path || path.length === 0) {
      return { isLocal: false, isHistorical: false, isNonStaging: false };
    }
    
    const firstElement = path[0];
    const isLocal = Array.isArray(firstElement);
    const isNonStaging = typeof firstElement === 'number';
    const isHistorical = isNonStaging && firstElement < 0;
    
    return { isLocal, isHistorical, isNonStaging };
  };

  const { isLocal, isHistorical, isNonStaging } = getPathInfo(selectedPath);

  const renderDirectoryContent = (items: string[], isComplete: boolean) => {
    const sortedItems = [...items].sort((a, b) => a.localeCompare(b));
    
    return (
      <div className="directory-list">
        <h3>Directory Contents:</h3>
        {sortedItems.map((item, index) => (
          <div key={index} className="directory-item">{item}</div>
        ))}
        {!isComplete && (
          <div className="directory-item" style={{ fontStyle: 'italic', color: 'var(--blue-gray)' }}>
            (List may be incomplete)
          </div>
        )}
      </div>
    );
  };

  const renderContent = () => {
    if (!content) return null;

    const { value: displayContent } = JournalService.extractSchemeValue(content.content);

    // Check for directory content
    const directory = JournalService.parseDirectoryResponse(content.content);
    if (directory) {
      return renderDirectoryContent(directory.items, directory.isComplete);
    }

    // Check for special content types
    if (Array.isArray(displayContent)) {
      if (displayContent[0] === 'nothing') {
        return <div className="empty-state">Empty document</div>;
      }
      if (displayContent[0] === 'unknown') {
        return <div className="empty-state">Document has been pruned</div>;
      }
    }

    // Regular document content
    if (isEditing) {
      return (
        <textarea
          className="content-editor"
          value={editValue}
          onChange={(e) => setEditValue(e.target.value)}
        />
      );
    }

    return (
      <pre className="content-text">
        {typeof displayContent === 'string' 
          ? displayContent 
          : JSON.stringify(displayContent, null, 2)}
      </pre>
    );
  };

  const canEdit = isLocal && !Array.isArray(content?.content) && !isHistorical;
  const canPin = isNonStaging && !isLocal && content && !Array.isArray(content.content);

  return (
    <div className="content-viewer">
      {selectedPath && (
        <div className="content-header">
          <div className="content-path-container">
            <div className="content-path">
              Path: {JSON.stringify(selectedPath)}
            </div>
            {content && (() => {
              const { schemeType } = JournalService.extractSchemeValue(content.content);
              return schemeType && (
                <div className="content-type">Type: {schemeType}</div>
              );
            })()}
          </div>
          <div className="content-actions">
            {canEdit && (
              <button
                className="button button-primary"
                onClick={() => isEditing ? handleSave() : setIsEditing(true)}
                disabled={isLoading}
              >
                {isEditing ? 'Save' : 'Edit'}
              </button>
            )}
            {canPin && (
              <button
                className={`button ${content!.isPinned ? 'button-secondary' : 'button-primary'}`}
                onClick={handlePin}
                disabled={isLoading}
                title={content!.isPinned ? 'Unpin this document' : 'Pin this document'}
              >
                {content!.isPinned ? '📌 Unpin' : '📌 Pin'}
              </button>
            )}
          </div>
        </div>
      )}
      
      <div className="content-body">
        {isLoading ? (
          <div className="loading-spinner" />
        ) : content ? (
          renderContent()
        ) : (
          <div className="empty-state">
            Select a document or directory to view its contents
          </div>
        )}
      </div>
    </div>
  );
};

export default ContentTab;
