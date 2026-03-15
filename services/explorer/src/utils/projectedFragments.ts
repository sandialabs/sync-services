import { ExplorerMode, ExplorerSelection, JournalPath, LedgerHop } from '../types';
import { buildLedgerStateRootPath } from './ledgerRoute';

const encodeSegments = (segments: string[], isDirectory: boolean): string => {
  const encoded = segments.map((segment) => encodeURIComponent(segment)).join('/');
  if (isDirectory) {
    return `#${encoded}/`;
  }
  return `#${encoded}`;
};

const decodeHashSegments = (hash: string): { segments: string[]; isDirectory: boolean } => {
  const raw = hash.startsWith('#') ? hash.slice(1) : hash;
  const isDirectory = raw.endsWith('/');
  const trimmed = raw.replace(/^\/+/, '').replace(/\/+$/, '');
  if (!trimmed) {
    return { segments: [], isDirectory };
  }

  return {
    segments: trimmed.split('/').map((segment) => decodeURIComponent(segment)),
    isDirectory,
  };
};

const buildStagePath = (selection: ExplorerSelection | null): JournalPath => {
  if (!selection) {
    return [['*state*']];
  }
  return selection.path;
};

const buildStageFragment = (selection: ExplorerSelection | null): string => {
  const path = buildStagePath(selection);
  const block = path[path.length - 1];
  const suffix = Array.isArray(block) ? block.slice(1) : [];
  return encodeSegments(['stage', ...suffix], selection?.type !== 'file');
};

const getLedgerRootSnapshot = (hop: LedgerHop, rootIndex: number): string => {
  const trimmed = hop.snapshot.trim().toLowerCase();
  if (trimmed === '' || trimmed === 'latest') {
    return rootIndex >= 0 ? String(rootIndex) : '0';
  }
  return hop.snapshot;
};

const buildLedgerStateSuffix = (selection: ExplorerSelection | null, ledgerRootPath: JournalPath): string[] => {
  const targetPath = selection?.path ?? ledgerRootPath;
  const targetBlock = targetPath[targetPath.length - 1];
  const rootBlock = ledgerRootPath[ledgerRootPath.length - 1];

  if (!Array.isArray(targetBlock) || !Array.isArray(rootBlock)) {
    return [];
  }

  return targetBlock.slice(rootBlock.length);
};

const buildLedgerFragment = (
  selection: ExplorerSelection | null,
  ledgerRootPath: JournalPath,
  ledgerHops: LedgerHop[],
  rootIndex: number,
): string => {
  const segments = ['ledger', 'previous', getLedgerRootSnapshot(ledgerHops[0], rootIndex)];

  for (const hop of ledgerHops.slice(1)) {
    segments.push('peer', hop.name);
    const trimmed = hop.snapshot.trim().toLowerCase();
    if (trimmed !== '' && trimmed !== 'latest') {
      segments.push('previous', hop.snapshot);
    }
  }

  segments.push('state', ...buildLedgerStateSuffix(selection, ledgerRootPath));
  return encodeSegments(segments, selection?.type !== 'file');
};

export const buildFragmentHash = (input: {
  mode: ExplorerMode;
  stageSelection: ExplorerSelection | null;
  ledgerSelection: ExplorerSelection | null;
  ledgerRootPath: JournalPath;
  ledgerHops: LedgerHop[];
  rootIndex: number;
}): string => {
  if (input.mode === 'stage') {
    return buildStageFragment(input.stageSelection);
  }

  return buildLedgerFragment(
    input.ledgerSelection,
    input.ledgerRootPath,
    input.ledgerHops,
    input.rootIndex,
  );
};

export const buildProjectedPathDisplay = (input: {
  mode: ExplorerMode;
  stageSelection: ExplorerSelection | null;
  ledgerSelection: ExplorerSelection | null;
  ledgerRootPath: JournalPath;
  ledgerHops: LedgerHop[];
  rootIndex: number;
}): string => {
  const hash = buildFragmentHash(input);
  const raw = hash.startsWith('#') ? hash.slice(1) : hash;
  return raw ? `/${raw}` : '/';
};

const parseStageFragment = (segments: string[], isDirectory: boolean) => ({
  mode: 'stage' as const,
  selection: {
    path: [['*state*', ...segments.slice(1)]] as JournalPath,
    type: isDirectory || segments.length === 1 ? 'directory' as const : 'file' as const,
  },
});

const parseLedgerFragment = (segments: string[], isDirectory: boolean) => {
  let cursor = 1;
  let rootSnapshot = '0';
  const hops: LedgerHop[] = [];

  if (segments[cursor] === 'previous' && cursor + 1 < segments.length) {
    rootSnapshot = segments[cursor + 1];
    cursor += 2;
  }

  hops.push({
    key: 'local',
    kind: 'local',
    name: 'Self',
    snapshot: rootSnapshot,
  });

  const path: JournalPath = [Number.parseInt(rootSnapshot, 10)];

  while (cursor < segments.length) {
    const segment = segments[cursor];
    if (segment === 'state') {
      const suffix = segments.slice(cursor + 1);
      path.push(['*state*', ...suffix]);
      return {
        mode: 'ledger' as const,
        hops,
        selection: {
          path,
          type: isDirectory || suffix.length === 0 ? 'directory' as const : 'file' as const,
        },
      };
    }

    if (segment !== 'peer' || cursor + 1 >= segments.length) {
      return null;
    }

    const peerName = segments[cursor + 1];
    cursor += 2;
    let snapshot = 'latest';
    if (segments[cursor] === 'previous' && cursor + 1 < segments.length) {
      snapshot = segments[cursor + 1];
      cursor += 2;
    }

    hops.push({
      key: `${peerName}-${hops.length}`,
      kind: 'peer',
      name: peerName,
      snapshot,
    });
    path.push(['*peer*', peerName, 'chain']);
    path.push(snapshot === 'latest' ? -1 : Number.parseInt(snapshot, 10));
  }

  return null;
};

export const parseFragmentHash = (hash: string): {
  mode: ExplorerMode;
  selection: ExplorerSelection;
  ledgerHops?: LedgerHop[];
} | null => {
  const { segments, isDirectory } = decodeHashSegments(hash);
  if (segments.length === 0) {
    return null;
  }

  if (segments[0] === 'stage') {
    return parseStageFragment(segments, isDirectory);
  }

  if (segments[0] === 'ledger') {
    const parsed = parseLedgerFragment(segments, isDirectory);
    if (!parsed) {
      return null;
    }

    return {
      mode: parsed.mode,
      selection: parsed.selection,
      ledgerHops: parsed.hops,
    };
  }

  return null;
};

export const getInitialLedgerHops = (rootIndex: number): LedgerHop[] => [
  {
    key: 'local',
    kind: 'local',
    name: 'Self',
    snapshot: rootIndex >= 0 ? String(rootIndex) : '0',
  },
];

export const buildLedgerRootSelection = (hops: LedgerHop[], rootIndex: number): ExplorerSelection => ({
  path: buildLedgerStateRootPath(hops, rootIndex >= 0 ? rootIndex : 0),
  type: 'directory',
});
