import { ExplorerSelection, LedgerHop } from '../types';
import {
  buildFragmentHash,
  parseFragmentHash,
} from './projectedFragments';

describe('projectedFragments', () => {
  const ledgerHops: LedgerHop[] = [
    { key: 'local', kind: 'local', name: 'Self', snapshot: '42' },
    { key: 'alice-1', kind: 'bridge', name: 'alice', snapshot: 'latest' },
    { key: 'bob-2', kind: 'bridge', name: 'bob', snapshot: '-3' },
  ];

  it('builds a stage fragment with url-safe encoding', () => {
    const stageSelection: ExplorerSelection = {
      path: [['*state*', 'docs', 'hello world.txt']],
      type: 'file',
    };

    const hash = buildFragmentHash({
      mode: 'stage',
      stageSelection,
      ledgerSelection: null,
      ledgerRootPath: [42, ['*state*']],
      ledgerHops,
      rootIndex: 42,
    });

    expect(hash).toBe('#stage/docs/hello%20world.txt');
  });

  it('round-trips a ledger fragment with bridges and history', () => {
    const ledgerSelection: ExplorerSelection = {
      path: [42, ['*bridge*', 'alice', 'chain'], -1, ['*bridge*', 'bob', 'chain'], -3, ['*state*', 'docs', 'readme.md']],
      type: 'file',
    };

    const hash = buildFragmentHash({
      mode: 'ledger',
      stageSelection: null,
      ledgerSelection,
      ledgerRootPath: [42, ['*bridge*', 'alice', 'chain'], -1, ['*bridge*', 'bob', 'chain'], -3, ['*state*']],
      ledgerHops,
      rootIndex: 42,
    });

    expect(hash).toBe('#ledger/previous/42/bridge/alice/bridge/bob/previous/-3/state/docs/readme.md');

    expect(parseFragmentHash(hash)).toEqual({
      mode: 'ledger',
      ledgerHops: [
        { key: 'local', kind: 'local', name: 'Self', snapshot: '42' },
        { key: 'alice-1', kind: 'bridge', name: 'alice', snapshot: 'latest' },
        { key: 'bob-2', kind: 'bridge', name: 'bob', snapshot: '-3' },
      ],
      selection: ledgerSelection,
    });
  });
});
