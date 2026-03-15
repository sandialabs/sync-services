import React, { useEffect, useMemo, useRef, useState } from 'react';
import { DirectoryResult, ExplorerMode, ExplorerSelection, JournalPath, JournalResponse } from '../types';
import { JournalService } from '../services/JournalService';
import { compareSegmentedNames } from '../utils/sortKeys';

interface ExplorerContentProps {
  mode: ExplorerMode;
  selection: ExplorerSelection | null;
  journalService: JournalService | null;
  refreshKey: number;
  ledgerView: 'content' | 'proof';
  onLedgerViewToggle: () => void;
  onStageCreateFile: (path: JournalPath) => Promise<void>;
  onStageCreateDirectory: (path: JournalPath) => Promise<void>;
  onStageUploadFile: (path: JournalPath, file: File) => Promise<void>;
  onSelectPath: (selection: ExplorerSelection) => void;
}

const ExplorerContent: React.FC<ExplorerContentProps> = ({
  mode,
  selection,
  journalService,
  refreshKey,
  ledgerView,
  onLedgerViewToggle,
  onStageCreateFile,
  onStageCreateDirectory,
  onStageUploadFile,
  onSelectPath,
}) => {
  const [response, setResponse] = useState<JournalResponse | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [isEditing, setIsEditing] = useState(false);
  const [editValue, setEditValue] = useState('');
  const [actionNotice, setActionNotice] = useState<string | null>(null);
  const uploadInputRef = useRef<HTMLInputElement | null>(null);

  const buildChildPath = (parentPath: JournalPath, childName: string): JournalPath => {
    const last = parentPath[parentPath.length - 1];
    if (!Array.isArray(last)) {
      return parentPath;
    }

    return [...parentPath.slice(0, -1), [last[0], ...last.slice(1), childName]];
  };

  useEffect(() => {
    if (!journalService || !selection) {
      setResponse(null);
      setIsEditing(false);
      setEditValue('');
      setActionNotice(null);
      return;
    }

    let active = true;
    const load = async () => {
      setIsLoading(true);
      try {
        const nextResponse = await journalService.get(selection.path);
        if (!active) {
          return;
        }
        setResponse(nextResponse);
        const { value } = JournalService.extractSchemeValue(nextResponse.content);
        setEditValue(typeof value === 'string' ? value : JSON.stringify(value, null, 2));
        setIsEditing(false);
        setActionNotice(null);
      } catch (error) {
        if (active) {
          setResponse({
            content: `Error loading content: ${error instanceof Error ? error.message : 'Unknown error'}`,
            'pinned?': null,
            proof: { error: String(error) },
          });
          setIsEditing(false);
        }
      } finally {
        if (active) {
          setIsLoading(false);
        }
      }
    };

    load();
    return () => {
      active = false;
    };
  }, [journalService, selection, refreshKey]);

  const directory = useMemo<DirectoryResult | null>(() => {
    if (!response) {
      return null;
    }
    return JournalService.parseDirectoryResponse(response.content);
  }, [response]);

  const isPinnedValue = (value: JournalResponse['pinned?'] | null | undefined): boolean => {
    if (value == null) {
      return false;
    }
    if (typeof value === 'boolean') {
      return value;
    }
    if (Array.isArray(value)) {
      return value.length > 0;
    }
    return true;
  };

  const isPinned = useMemo(() => {
    return isPinnedValue(response?.['pinned?']);
  }, [response]);

  const directoryEntries = useMemo(() => {
    if (!response) {
      return null;
    }

    return JournalService.parseDirectoryEntries(response.content);
  }, [response]);

  const handleSave = async () => {
    if (!journalService || !selection) {
      return;
    }

    setIsLoading(true);
    try {
      let valueToSave: any;
      try {
        valueToSave = JSON.parse(editValue);
      } catch {
        valueToSave = { '*type/string*': editValue };
      }
      await journalService.set(selection.path, valueToSave);
      setIsEditing(false);
      const nextResponse = await journalService.get(selection.path);
      setResponse(nextResponse);
      setActionNotice('Saved');
    } catch (error) {
      alert(`Failed to save: ${error instanceof Error ? error.message : 'Unknown error'}`);
    } finally {
      setIsLoading(false);
    }
  };

  const handleDownload = async () => {
    if (!journalService || !selection) {
      return;
    }

    try {
      const { blob, filename } = await journalService.download(selection.path);
      const url = URL.createObjectURL(blob);
      const anchor = document.createElement('a');
      anchor.href = url;
      anchor.download = filename;
      anchor.click();
      URL.revokeObjectURL(url);
      setActionNotice(`Downloaded ${filename}`);
    } catch (error) {
      alert(`Failed to download: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  };

  const handlePinToggle = async () => {
    if (!journalService || !selection) {
      return;
    }
    setIsLoading(true);
    try {
      if (isPinned) {
        await journalService.unpin(selection.path);
      } else {
        await journalService.pin(selection.path);
      }

      const expectedPinned = !isPinned;
      setResponse((prev) =>
        prev
          ? {
              ...prev,
              'pinned?': expectedPinned,
            }
          : prev,
      );
      setActionNotice(expectedPinned ? 'Pinned' : 'Unpinned');

      try {
        const nextResponse = await journalService.get(selection.path);
        setResponse(nextResponse);
      } catch {
        // Keep the optimistic pinned state if the immediate refresh fails.
      }
    } catch (error) {
      alert(`Failed to update pin: ${error instanceof Error ? error.message : 'Unknown error'}`);
    } finally {
      setIsLoading(false);
    }
  };

  const openUploadDialog = () => uploadInputRef.current?.click();

  if (!selection) {
    return (
      <div className="empty-state">
        {mode === 'stage'
          ? 'Select a local document or directory to view its contents.'
          : 'Select a ledger document or directory to browse this route.'}
      </div>
    );
  }

  const { value: extractedContent } = JournalService.extractSchemeValue(response?.content);
  const title = Array.isArray(selection.path[selection.path.length - 1])
    ? String((selection.path[selection.path.length - 1] as string[]).slice(-1)[0] ?? '')
    : 'item';

  return (
    <div className="content-viewer">
      <div className="content-header">
        <div className="content-path-container">
          <div className="content-path">{title}</div>
          {actionNotice && <div className="content-meta-note">{actionNotice}</div>}
        </div>
        <div className="content-actions">
          {mode === 'stage' && selection.type === 'directory' && (
            <>
              <button className="button button-secondary" onClick={() => void onStageCreateFile(selection.path)}>+ Document</button>
              <button className="button button-secondary" onClick={() => void onStageCreateDirectory(selection.path)}>+ Directory</button>
              <button className="button button-secondary" onClick={openUploadDialog}>Upload File</button>
              <input
                ref={uploadInputRef}
                type="file"
                style={{ display: 'none' }}
                onChange={(event) => {
                  const file = event.target.files?.[0];
                  if (file) {
                    void onStageUploadFile(selection.path, file);
                    event.target.value = '';
                  }
                }}
              />
            </>
          )}
          {mode === 'stage' && selection.type === 'file' && (
            <>
              <button className="button button-secondary" onClick={() => {
                if (isEditing) {
                  void handleSave();
                } else {
                  setIsEditing(true);
                }
              }}>
                {isEditing ? 'Save' : 'Edit'}
              </button>
              <button className="button button-secondary" onClick={handleDownload}>Download</button>
            </>
          )}
          {mode === 'ledger' && selection.type === 'file' && (
            <>
              <button className="button button-secondary" onClick={onLedgerViewToggle}>
                {ledgerView === 'content' ? 'Proof' : 'Content'}
              </button>
              <button
                className={isPinned ? 'button button-secondary' : 'button button-primary'}
                onClick={handlePinToggle}
              >
                {isPinned ? 'Unpin' : 'Pin'}
              </button>
            </>
          )}
        </div>
      </div>

      <div className="content-body">
        {isLoading ? (
          <div className="loading-spinner" />
        ) : mode === 'ledger' && selection.type === 'file' && ledgerView === 'proof' ? (
          <pre className="content-text">{JSON.stringify(response?.proof, null, 2)}</pre>
        ) : directory ? (
          <div className="directory-list">
            {(directoryEntries ?? [])
              .filter((item) => item.name !== '*directory*')
              .sort((a, b) => {
                const leftRank = a.type === 'directory' ? 0 : 1;
                const rightRank = b.type === 'directory' ? 0 : 1;
                if (leftRank !== rightRank) {
                  return leftRank - rightRank;
                }
                return compareSegmentedNames(a.name, b.name);
              })
              .map((item) => (
                <button
                  key={item.name}
                  className="directory-item directory-item-button"
                  onClick={() => onSelectPath({
                    path: buildChildPath(selection.path, item.name),
                    type: item.type === 'directory' ? 'directory' : 'file',
                  })}
                >
                  <span className="directory-item-kind" aria-hidden="true">
                    {item.type === 'directory' ? '▣' : '▤'}
                  </span>
                  <span>{item.name}</span>
                </button>
              ))}
          </div>
        ) : mode === 'stage' && selection.type === 'file' && isEditing ? (
          <textarea
            className="content-editor"
            value={editValue}
            onChange={(event) => setEditValue(event.target.value)}
          />
        ) : (
          <pre className="content-text">
            {typeof extractedContent === 'string'
              ? extractedContent
              : JSON.stringify(extractedContent, null, 2)}
          </pre>
        )}
      </div>
    </div>
  );
};

export default ExplorerContent;
