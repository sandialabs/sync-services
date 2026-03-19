import { JournalPath, LedgerHop } from '../types';

export const LEDGER_LATEST = 'latest';

const normalizeSnapshotValue = (value: string): string => {
  const trimmed = value.trim().toLowerCase();
  if (trimmed === '' || trimmed === LEDGER_LATEST) {
    return LEDGER_LATEST;
  }

  const parsed = Number.parseInt(trimmed, 10);
  if (Number.isNaN(parsed) || parsed >= 0) {
    return LEDGER_LATEST;
  }
  return String(parsed);
};

export const normalizeSnapshotInput = (value: string): string => normalizeSnapshotValue(value);

export const stepSnapshotValue = (value: string, direction: 'older' | 'newer'): string => {
  const normalized = normalizeSnapshotValue(value);
  const current = normalized === LEDGER_LATEST ? -1 : Number.parseInt(normalized, 10);

  if (direction === 'older') {
    return String(current - 1);
  }

  if (current >= -2) {
    return LEDGER_LATEST;
  }

  return String(current + 1);
};

const firstHopToRootIndex = (snapshot: string, rootIndex: number): number => {
  const trimmed = snapshot.trim().toLowerCase();
  if (trimmed === '' || trimmed === LEDGER_LATEST) {
    return rootIndex;
  }

  const parsed = Number.parseInt(trimmed, 10);
  if (Number.isNaN(parsed)) {
    return rootIndex;
  }

  return parsed;
};

const bridgeHopToIndex = (snapshot: string): number => {
  const normalized = normalizeSnapshotValue(snapshot);
  if (normalized === LEDGER_LATEST) {
    return -1;
  }
  return Number.parseInt(normalized, 10);
};

export const buildLedgerRouteBasePath = (
  hops: LedgerHop[],
  rootIndex: number,
): JournalPath => {
  if (hops.length === 0) {
    return [rootIndex];
  }

  const [first, ...rest] = hops;
  const path: JournalPath = [firstHopToRootIndex(first.snapshot, rootIndex)];

  for (const hop of rest) {
    path.push(['*bridge*', hop.name, 'chain']);
    path.push(bridgeHopToIndex(hop.snapshot));
  }

  return path;
};

export const buildLedgerStateRootPath = (
  hops: LedgerHop[],
  rootIndex: number,
): JournalPath => [...buildLedgerRouteBasePath(hops, rootIndex), ['*state*']];

export const buildLedgerBridgesPath = (
  hops: LedgerHop[],
  rootIndex: number,
): JournalPath => [...buildLedgerRouteBasePath(hops, rootIndex), ['*bridge*']];
