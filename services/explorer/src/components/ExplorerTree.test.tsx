import React from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import ExplorerTree from './ExplorerTree';
import { JournalService } from '../services/JournalService';

describe('ExplorerTree', () => {
  const mockJournalService = {
    getDirectoryEntries: jest.fn(),
  } as unknown as JournalService;

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('shows directories before files using segmented sorting', async () => {
    (mockJournalService.getDirectoryEntries as jest.Mock).mockResolvedValue([
      { name: 'alpha', type: 'value' },
      { name: '10-foo', type: 'directory' },
      { name: '2-foo', type: 'directory' },
      { name: 'beta', type: 'value' },
    ]);

    render(
      <ExplorerTree
        mode="stage"
        rootPath={[['*state*']]}
        selected={null}
        expandedNodes={new Set()}
        journalService={mockJournalService}
        refreshKey={0}
        onExpandedNodesChange={jest.fn()}
        onSelect={jest.fn()}
      />,
    );

    const twoFoo = await screen.findByText('2-foo');
    const tenFoo = screen.getByText('10-foo');
    const alpha = screen.getByText('alpha');
    const beta = screen.getByText('beta');

    const labels = [twoFoo, tenFoo, alpha, beta].map((node) => node.closest('button')?.textContent);
    expect(labels).toEqual(['▣2-foo', '▣10-foo', '▤alpha', '▤beta']);
  });

  it('shows an empty state for an empty stage tree', async () => {
    (mockJournalService.getDirectoryEntries as jest.Mock).mockResolvedValue([]);

    render(
      <ExplorerTree
        mode="stage"
        rootPath={[['*state*']]}
        selected={null}
        expandedNodes={new Set()}
        journalService={mockJournalService}
        refreshKey={0}
        onExpandedNodesChange={jest.fn()}
        onSelect={jest.fn()}
      />,
    );

    expect(await screen.findByText('No local documents yet.')).toBeInTheDocument();
  });

  it('emits a selection when a node is clicked', async () => {
    const onSelect = jest.fn();
    (mockJournalService.getDirectoryEntries as jest.Mock).mockResolvedValue([
      { name: 'draft.txt', type: 'value' },
    ]);

    render(
      <ExplorerTree
        mode="stage"
        rootPath={[['*state*']]}
        selected={null}
        expandedNodes={new Set()}
        journalService={mockJournalService}
        refreshKey={0}
        onExpandedNodesChange={jest.fn()}
        onSelect={onSelect}
      />,
    );

    const node = await screen.findByText('draft.txt');
    fireEvent.click(node);

    expect(onSelect).toHaveBeenCalledWith({
      path: [['*state*', 'draft.txt']],
      type: 'file',
    });
  });
});
