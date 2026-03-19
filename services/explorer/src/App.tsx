import React, { useEffect, useMemo, useRef, useState } from 'react';
import './App.css';
import ToolBar from './components/ToolBar';
import ExplorerTree from './components/ExplorerTree';
import ExplorerContent from './components/ExplorerContent';
import LedgerRouteBar from './components/LedgerRouteBar';
import { JournalService } from './services/JournalService';
import { AppState, ExplorerMode, ExplorerSelection, JournalPath, LedgerHop, TreeNode } from './types';
import {
  LEDGER_LATEST,
  buildLedgerBridgesPath,
  buildLedgerStateRootPath,
  normalizeSnapshotInput,
  stepSnapshotValue,
} from './utils/ledgerRoute';
import {
  buildFragmentHash,
  getInitialLedgerHops,
  parseFragmentHash,
} from './utils/projectedFragments';

const getEnvVar = (key: string): string => {
  // @ts-ignore
  if (window._env_?.[key]) {
    // @ts-ignore
    return window._env_[key];
  }
  return process.env[`REACT_APP_${key}`] || '';
};

const JOURNAL_ENDPOINT = getEnvVar('SYNC_EXPLORER_ENDPOINT');

const getInitialTheme = (): 'light' | 'dark' => {
  const stored = localStorage.getItem('theme');
  if (stored === 'light' || stored === 'dark') {
    return stored;
  }
  if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
    return 'dark';
  }
  return 'light';
};

const createInitialAppState = (): AppState => ({
  endpoint: JOURNAL_ENDPOINT,
  authentication: getEnvVar('SYNC_EXPLORER_PASSWORD'),
  rootIndex: -1,
  selectedPath: null,
  expandedNodes: new Set(),
  isLoading: false,
  error: null,
});

const STAGE_ROOT_PATH: JournalPath = [['*state*']];

const isPathWithin = (candidate: JournalPath, ancestor: JournalPath): boolean => {
  if (ancestor.length > candidate.length) {
    return false;
  }

  return ancestor.every((segment, index) => JSON.stringify(segment) === JSON.stringify(candidate[index]));
};

const buildStageChildPath = (parentPath: JournalPath, childName: string): JournalPath => {
  const last = parentPath[parentPath.length - 1];
  if (!Array.isArray(last) || last[0] !== '*state*') {
    throw new Error('Expected a stage state path');
  }

  return [
    ...parentPath.slice(0, -1),
    ['*state*', ...last.slice(1), childName],
  ];
};

const buildStageSiblingPath = (path: JournalPath, siblingName: string): JournalPath => {
  const last = path[path.length - 1];
  if (!Array.isArray(last) || last[0] !== '*state*') {
    throw new Error('Expected a stage state path');
  }

  return [
    ...path.slice(0, -1),
    ['*state*', ...last.slice(1, -1), siblingName],
  ];
};

const stagePathToTreeNodeId = (path: JournalPath): string => {
  const last = path[path.length - 1];
  if (!Array.isArray(last) || last[0] !== '*state*') {
    throw new Error('Expected a stage state path');
  }

  const suffix = last.slice(1);
  return suffix.length > 0 ? `stage/${suffix.join('/')}` : 'stage';
};

const replaceStagePathPrefix = (
  candidate: JournalPath,
  sourcePrefix: JournalPath,
  targetPrefix: JournalPath,
): JournalPath => {
  if (!isPathWithin(candidate, sourcePrefix)) {
    return candidate;
  }

  const candidateLast = candidate[candidate.length - 1];
  const sourceLast = sourcePrefix[sourcePrefix.length - 1];
  const targetLast = targetPrefix[targetPrefix.length - 1];

  if (!Array.isArray(candidateLast) || !Array.isArray(sourceLast) || !Array.isArray(targetLast)) {
    return candidate;
  }

  return [
    ...candidate.slice(0, -1),
    [...targetLast, ...candidateLast.slice(sourceLast.length)],
  ];
};

const App: React.FC = () => {
  const [appState, setAppState] = useState<AppState>(createInitialAppState);
  const [theme, setTheme] = useState<'light' | 'dark'>(getInitialTheme);
  const [mode, setMode] = useState<ExplorerMode>('ledger');
  const [journalService, setJournalService] = useState<JournalService | null>(null);
  const [stageSelection, setStageSelection] = useState<ExplorerSelection | null>(null);
  const [ledgerSelection, setLedgerSelection] = useState<ExplorerSelection | null>(null);
  const [stageExpandedNodes, setStageExpandedNodes] = useState<Set<string>>(new Set());
  const [ledgerExpandedNodes, setLedgerExpandedNodes] = useState<Set<string>>(new Set());
  const [ledgerHops, setLedgerHops] = useState<LedgerHop[]>(getInitialLedgerHops(-1));
  const [ledgerPeerChoices, setLedgerPeerChoices] = useState<string[] | null>(null);
  const [ledgerView, setLedgerView] = useState<'content' | 'proof'>('content');
  const [stageRefreshKey, setStageRefreshKey] = useState(0);
  const [ledgerRefreshKey, setLedgerRefreshKey] = useState(0);
  const isApplyingHashRef = useRef(false);

  useEffect(() => {
    document.documentElement.setAttribute('data-theme', theme);
    localStorage.setItem('theme', theme);
  }, [theme]);

  useEffect(() => {
    if (JOURNAL_ENDPOINT && appState.authentication) {
      setJournalService(new JournalService(JOURNAL_ENDPOINT, appState.authentication));
    } else {
      setJournalService(null);
    }
  }, [appState.authentication]);

  const setLoadingState = (isLoading: boolean, error: string | null = null) => {
    setAppState((prev) => ({ ...prev, isLoading, error }));
  };

  const synchronizeLedger = async () => {
    if (!journalService) {
      return;
    }

    setLoadingState(true, null);
    try {
      const size = await journalService.getSize();
      const latestIndex = Math.max(0, size - 1);
      setAppState((prev) => ({
        ...prev,
        rootIndex: latestIndex,
        isLoading: false,
        error: null,
      }));
      setLedgerHops((prev) =>
        prev.map((hop, index) =>
          index === 0 ? { ...hop, snapshot: String(latestIndex) } : hop,
        ),
      );
      setLedgerRefreshKey((prev) => prev + 1);
    } catch (error) {
      setAppState((prev) => ({
        ...prev,
        isLoading: false,
        error: `Synchronization failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }));
    }
  };

  useEffect(() => {
    if (journalService && appState.rootIndex < 0) {
      void synchronizeLedger();
    }
  }, [journalService, appState.rootIndex]);

  const ledgerRootPath = useMemo(
    () => buildLedgerStateRootPath(ledgerHops, appState.rootIndex >= 0 ? appState.rootIndex : 0),
    [ledgerHops, appState.rootIndex],
  );
  const stageRootPath = useMemo(() => STAGE_ROOT_PATH, []);
  useEffect(() => {
    const applyHash = () => {
      const parsed = parseFragmentHash(window.location.hash);
      if (!parsed) {
        return;
      }

      isApplyingHashRef.current = true;
      setMode(parsed.mode);
      setAppState((prev) => ({ ...prev, error: null }));

      if (parsed.mode === 'stage') {
        setStageSelection(parsed.selection);
      } else {
        setLedgerHops(parsed.ledgerHops ?? getInitialLedgerHops(appState.rootIndex));
        setLedgerSelection(parsed.selection);
        setLedgerView('content');
      }

      window.setTimeout(() => {
        isApplyingHashRef.current = false;
      }, 0);
    };

    applyHash();
    window.addEventListener('hashchange', applyHash);
    return () => window.removeEventListener('hashchange', applyHash);
  }, [appState.rootIndex]);

  const handleModeChange = (nextMode: ExplorerMode) => {
    setMode(nextMode);
    if (nextMode === 'ledger') {
      setLedgerView('content');
    }
    setAppState((prev) => ({ ...prev, error: null }));
  };

  const handleRenameStageNode = async (node: TreeNode) => {
    if (!journalService) {
      return;
    }
    const nextName = window.prompt('Rename to:', node.label);
    if (!nextName || nextName.trim() === '' || nextName === node.label) {
      return;
    }

    try {
      const renamedPath = buildStageSiblingPath(node.path, nextName.trim());
      await journalService.renameStagePath(node.path, nextName.trim());
      setStageSelection((prev) => {
        if (!prev || !isPathWithin(prev.path, node.path)) {
          return prev;
        }
        return {
          ...prev,
          path: replaceStagePathPrefix(prev.path, node.path, renamedPath),
        };
      });
      setStageExpandedNodes((prev) => {
        const sourceId = stagePathToTreeNodeId(node.path);
        const targetId = stagePathToTreeNodeId(renamedPath);
        const next = new Set<string>();
        prev.forEach((id) => {
          if (id === sourceId || id.startsWith(`${sourceId}/`)) {
            next.add(targetId + id.slice(sourceId.length));
          } else {
            next.add(id);
          }
        });
        return next;
      });
      setStageRefreshKey((prev) => prev + 1);
      setAppState((prev) => ({ ...prev, error: null }));
    } catch (error) {
      setAppState((prev) => ({
        ...prev,
        error: `Rename failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }));
    }
  };

  const handleDeleteStageNode = async (node: TreeNode) => {
    if (!journalService) {
      return;
    }
    const confirmed = window.confirm(`Delete ${node.label}?`);
    if (!confirmed) {
      return;
    }

    try {
      await journalService.deleteStagePath(node.path);
      setStageSelection((prev) =>
        prev && isPathWithin(prev.path, node.path) ? null : prev,
      );
      setStageExpandedNodes((prev) => {
        const sourceId = stagePathToTreeNodeId(node.path);
        const next = new Set<string>();
        prev.forEach((id) => {
          if (id !== sourceId && !id.startsWith(`${sourceId}/`)) {
            next.add(id);
          }
        });
        return next;
      });
      setStageRefreshKey((prev) => prev + 1);
      setAppState((prev) => ({ ...prev, error: null }));
    } catch (error) {
      setAppState((prev) => ({
        ...prev,
        error: `Delete failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }));
    }
  };

  const promptForName = (message: string): string | null => {
    const value = window.prompt(message);
    if (!value || value.trim() === '') {
      return null;
    }
    return value.trim();
  };

  const handleStageCreateFile = async (path: JournalPath) => {
    if (!journalService) {
      return;
    }
    const name = promptForName('Enter file name:');
    if (!name) {
      return;
    }
    try {
      const createdPath = buildStageChildPath(path, name);
      await journalService.createFile(path, name);
      setStageSelection({
        path: createdPath,
        type: 'file',
      });
      setStageExpandedNodes((prev) => {
        const next = new Set(prev);
        next.add(stagePathToTreeNodeId(path));
        next.add(stagePathToTreeNodeId(createdPath));
        return next;
      });
      setStageRefreshKey((prev) => prev + 1);
      setAppState((prev) => ({ ...prev, error: null }));
    } catch (error) {
      setAppState((prev) => ({
        ...prev,
        error: `Create file failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }));
    }
  };

  const handleStageCreateDirectory = async (path: JournalPath) => {
    if (!journalService) {
      return;
    }
    const name = promptForName('Enter folder name:');
    if (!name) {
      return;
    }
    try {
      const createdPath = buildStageChildPath(path, name);
      await journalService.createDirectory(path, name);
      setStageSelection({
        path: createdPath,
        type: 'directory',
      });
      setStageExpandedNodes((prev) => {
        const next = new Set(prev);
        next.add(stagePathToTreeNodeId(path));
        next.add(stagePathToTreeNodeId(createdPath));
        return next;
      });
      setStageRefreshKey((prev) => prev + 1);
      setAppState((prev) => ({ ...prev, error: null }));
    } catch (error) {
      setAppState((prev) => ({
        ...prev,
        error: `Create folder failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }));
    }
  };

  const handleStageUploadFile = async (path: JournalPath, file: File) => {
    if (!journalService) {
      return;
    }
    try {
      await journalService.uploadFile(path, file);
      const uploadedPath = buildStageChildPath(path, file.name);
      setStageSelection({
        path: uploadedPath,
        type: 'file',
      });
      setStageExpandedNodes((prev) => {
        const next = new Set(prev);
        next.add(stagePathToTreeNodeId(path));
        next.add(stagePathToTreeNodeId(uploadedPath));
        return next;
      });
      setStageRefreshKey((prev) => prev + 1);
      setAppState((prev) => ({ ...prev, error: null }));
    } catch (error) {
      setAppState((prev) => ({
        ...prev,
        error: `Upload failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }));
    }
  };

  const handleLedgerSnapshotChange = (index: number, value: string) => {
    setLedgerHops((prev) =>
      prev.map((hop, hopIndex) =>
        hopIndex === index ? { ...hop, snapshot: value } : hop,
      ),
    );
    setLedgerRefreshKey((prev) => prev + 1);
    setLedgerSelection(null);
  };

  const handleLedgerStepSnapshot = (index: number, direction: 'older' | 'newer') => {
    if (index === 0) {
      setLedgerHops((prev) =>
        prev.map((hop, hopIndex) => {
          if (hopIndex !== index) {
            return hop;
          }

          const current =
            hop.snapshot.trim().toLowerCase() === LEDGER_LATEST
              ? appState.rootIndex
              : Number.parseInt(hop.snapshot, 10);
          const safeCurrent = Number.isNaN(current) ? appState.rootIndex : current;

          if (direction === 'older') {
            return { ...hop, snapshot: String(Math.max(0, safeCurrent - 1)) };
          }

          if (safeCurrent + 1 >= appState.rootIndex) {
            return { ...hop, snapshot: LEDGER_LATEST };
          }

          return { ...hop, snapshot: String(safeCurrent + 1) };
        }),
      );
      setLedgerRefreshKey((prev) => prev + 1);
      setLedgerSelection(null);
      return;
    }

    setLedgerHops((prev) =>
      prev.map((hop, hopIndex) =>
        hopIndex === index
          ? { ...hop, snapshot: stepSnapshotValue(hop.snapshot, direction) }
          : hop,
      ),
    );
    setLedgerRefreshKey((prev) => prev + 1);
    setLedgerSelection(null);
  };

  const handleOpenLedgerPeerPicker = async () => {
    if (!journalService || appState.rootIndex < 0) {
      return;
    }
    try {
      const bridgesPath = buildLedgerBridgesPath(ledgerHops, appState.rootIndex);
      const entries = await journalService.getDirectoryEntries(bridgesPath);
      setLedgerPeerChoices(
        entries
          .filter((entry) => entry.name !== '*directory*')
          .map((entry) => entry.name)
          .sort((a, b) => a.localeCompare(b)),
      );
    } catch (error) {
      setAppState((prev) => ({
        ...prev,
        error: `Bridge lookup failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
      }));
    }
  };

  const handleChooseLedgerPeer = (peerName: string) => {
    setLedgerHops((prev) => [
      ...prev,
      {
        key: `${peerName}-${prev.length}`,
        kind: 'bridge',
        name: peerName,
        snapshot: LEDGER_LATEST,
      },
    ]);
    setLedgerPeerChoices(null);
    setLedgerRefreshKey((prev) => prev + 1);
    setLedgerExpandedNodes(new Set());
    setLedgerSelection(null);
  };

  const handleRemoveLedgerHop = () => {
    setLedgerPeerChoices(null);
    setLedgerHops((prev) => (prev.length > 1 ? prev.slice(0, -1) : prev));
    setLedgerRefreshKey((prev) => prev + 1);
    setLedgerExpandedNodes(new Set());
    setLedgerSelection(null);
  };

  useEffect(() => {
    setLedgerHops((prev) =>
      prev.map((hop, index) => {
        if (index === 0) {
          const trimmed = hop.snapshot.trim().toLowerCase();
          if (trimmed === '' || trimmed === LEDGER_LATEST) {
            return { ...hop, snapshot: appState.rootIndex >= 0 ? String(appState.rootIndex) : '0' };
          }

          const parsed = Number.parseInt(trimmed, 10);
          if (Number.isNaN(parsed) || parsed < 0) {
            return { ...hop, snapshot: appState.rootIndex >= 0 ? String(appState.rootIndex) : '0' };
          }

          return { ...hop, snapshot: String(parsed) };
        }

        return { ...hop, snapshot: normalizeSnapshotInput(hop.snapshot) };
      }),
    );
  }, [appState.rootIndex]);

  useEffect(() => {
    setLedgerView('content');
  }, [ledgerSelection]);

  useEffect(() => {
    if (isApplyingHashRef.current) {
      return;
    }

    const nextHash = buildFragmentHash({
      mode,
      stageSelection,
      ledgerSelection,
      ledgerRootPath,
      ledgerHops,
      rootIndex: appState.rootIndex,
    });

    if (window.location.hash !== nextHash) {
      window.history.replaceState(
        null,
        '',
        `${window.location.pathname}${window.location.search}${nextHash}`,
      );
    }
  }, [mode, stageSelection, ledgerSelection, ledgerRootPath, ledgerHops, appState.rootIndex]);

  return (
    <div className="app">
      <ToolBar
        authentication={appState.authentication}
        error={appState.error}
        isLoading={appState.isLoading}
        mode={mode}
        theme={theme}
        onAuthenticationChange={(authentication) =>
          setAppState((prev) => ({ ...prev, authentication }))
        }
        onModeChange={handleModeChange}
        onThemeToggle={() => setTheme((prev) => (prev === 'light' ? 'dark' : 'light'))}
      />

      {mode === 'ledger' && (
        <LedgerRouteBar
          hops={ledgerHops}
          peerChoices={ledgerPeerChoices}
          rootIndex={appState.rootIndex}
          onSynchronize={synchronizeLedger}
          onSnapshotChange={handleLedgerSnapshotChange}
          onStepSnapshot={handleLedgerStepSnapshot}
          onRemoveHop={handleRemoveLedgerHop}
          onOpenPeerPicker={handleOpenLedgerPeerPicker}
          onClosePeerPicker={() => setLedgerPeerChoices(null)}
          onChoosePeer={handleChooseLedgerPeer}
        />
      )}

      <div className="main-content two-pane">
        <div className="left-pane pane">
          <ExplorerTree
            mode={mode}
            rootPath={mode === 'stage' ? stageRootPath : ledgerRootPath}
            selected={mode === 'stage' ? stageSelection : ledgerSelection}
            expandedNodes={mode === 'stage' ? stageExpandedNodes : ledgerExpandedNodes}
            journalService={journalService}
            refreshKey={mode === 'stage' ? stageRefreshKey : ledgerRefreshKey}
            onExpandedNodesChange={mode === 'stage' ? setStageExpandedNodes : setLedgerExpandedNodes}
            onSelect={(selection) => {
              if (mode === 'stage') {
                setStageSelection(selection);
              } else {
                setLedgerSelection(selection);
              }
            }}
            onRename={mode === 'stage' ? handleRenameStageNode : undefined}
            onDelete={mode === 'stage' ? handleDeleteStageNode : undefined}
          />
        </div>

        <div className="middle-pane pane">
          <ExplorerContent
            mode={mode}
            selection={mode === 'stage' ? stageSelection : ledgerSelection}
            journalService={journalService}
            refreshKey={mode === 'stage' ? stageRefreshKey : ledgerRefreshKey}
            ledgerView={ledgerView}
            onLedgerViewToggle={() =>
              setLedgerView((prev) => (prev === 'content' ? 'proof' : 'content'))
            }
            onStageCreateFile={handleStageCreateFile}
            onStageCreateDirectory={handleStageCreateDirectory}
            onStageUploadFile={handleStageUploadFile}
            onSelectPath={(selection) => {
              if (mode === 'stage') {
                setStageSelection(selection);
              } else {
                setLedgerSelection(selection);
              }
            }}
          />
        </div>
      </div>
    </div>
  );
};

export default App;
