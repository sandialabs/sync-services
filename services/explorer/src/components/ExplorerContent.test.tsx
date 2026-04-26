import React from 'react';
import { fireEvent, render, screen, waitFor } from '@testing-library/react';
import ExplorerContent from './ExplorerContent';
import { JournalService } from '../services/JournalService';

jest.mock('../services/JournalService', () => ({
  JournalService: {
    extractSchemeValue: jest.fn((value) => ({ value, schemeType: null })),
    parseDirectoryResponse: jest.fn(),
    parseDirectoryEntries: jest.fn(),
  },
}));

describe('ExplorerContent', () => {
  const mockJournalService = {
    get: jest.fn(),
    set: jest.fn(),
    download: jest.fn(),
    pin: jest.fn(),
    unpin: jest.fn(),
  } as unknown as JournalService;

  beforeEach(() => {
    jest.clearAllMocks();
    (JournalService.extractSchemeValue as jest.Mock).mockImplementation((value) => ({ value, schemeType: null }));
    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue(null);
    (JournalService.parseDirectoryEntries as jest.Mock).mockReturnValue(null);
  });

  it('shows stage directory actions', async () => {
    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['directory', { docs: 'directory' }, true],
      'pinned?': false,
      proof: {},
    });
    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue({
      items: ['docs'],
      isComplete: true,
    });
    (JournalService.parseDirectoryEntries as jest.Mock).mockReturnValue([
      { name: 'docs', type: 'directory' },
    ]);

    render(
      <ExplorerContent
        mode="stage"
        selection={{ path: [['*state*', 'docs']], type: 'directory' }}
        journalService={mockJournalService}
        refreshKey={0}
        ledgerView="content"
        onLedgerViewToggle={jest.fn()}
        onStageCreateFile={jest.fn().mockResolvedValue(undefined)}
        onStageCreateDirectory={jest.fn().mockResolvedValue(undefined)}
        onStageUploadFile={jest.fn().mockResolvedValue(undefined)}
        onStageRename={jest.fn().mockResolvedValue(undefined)}
        onStageDelete={jest.fn().mockResolvedValue(undefined)}
        onSelectPath={jest.fn()}
      />,
    );

    expect(await screen.findByText('+ Document')).toBeInTheDocument();
    expect(screen.getByText('+ Directory')).toBeInTheDocument();
    expect(screen.getByText('Upload File')).toBeInTheDocument();
  });

  it('shows stage file actions', async () => {
    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: { '*type/string*': 'hello' },
      'pinned?': false,
      proof: {},
    });
    (JournalService.extractSchemeValue as jest.Mock).mockReturnValue({
      value: 'hello',
      schemeType: 'string',
    });

    render(
      <ExplorerContent
        mode="stage"
        selection={{ path: [['*state*', 'draft.txt']], type: 'file' }}
        journalService={mockJournalService}
        refreshKey={0}
        ledgerView="content"
        onLedgerViewToggle={jest.fn()}
        onStageCreateFile={jest.fn().mockResolvedValue(undefined)}
        onStageCreateDirectory={jest.fn().mockResolvedValue(undefined)}
        onStageUploadFile={jest.fn().mockResolvedValue(undefined)}
        onStageRename={jest.fn().mockResolvedValue(undefined)}
        onStageDelete={jest.fn().mockResolvedValue(undefined)}
        onSelectPath={jest.fn()}
      />,
    );

    expect(await screen.findByText('Edit')).toBeInTheDocument();
    expect(screen.getByText('Download')).toBeInTheDocument();
  });

  it('navigates when a directory entry is clicked', async () => {
    const onSelectPath = jest.fn();

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['directory', { docs: 'directory', 'readme.md': 'value' }, true],
      'pinned?': false,
      proof: {},
    });
    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue({
      items: ['docs', 'readme.md'],
      isComplete: true,
    });
    (JournalService.parseDirectoryEntries as jest.Mock).mockReturnValue([
      { name: 'docs', type: 'directory' },
      { name: 'readme.md', type: 'value' },
    ]);

    render(
      <ExplorerContent
        mode="ledger"
        selection={{ path: [42, ['*state*']], type: 'directory' }}
        journalService={mockJournalService}
        refreshKey={0}
        ledgerView="content"
        onLedgerViewToggle={jest.fn()}
        onStageCreateFile={jest.fn().mockResolvedValue(undefined)}
        onStageCreateDirectory={jest.fn().mockResolvedValue(undefined)}
        onStageUploadFile={jest.fn().mockResolvedValue(undefined)}
        onStageRename={jest.fn().mockResolvedValue(undefined)}
        onStageDelete={jest.fn().mockResolvedValue(undefined)}
        onSelectPath={onSelectPath}
      />,
    );

    const docsButton = await screen.findByRole('button', { name: /docs/i });
    fireEvent.click(docsButton);

    expect(onSelectPath).toHaveBeenCalledWith({
      path: [42, ['*state*', 'docs']],
      type: 'directory',
    });
  });

  it('shows ledger proof toggle and unpin state from boolean pinned response', async () => {
    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: { '*type/string*': 'peer content' },
      'pinned?': true,
      proof: { hash: 'abc' },
    });
    (JournalService.extractSchemeValue as jest.Mock).mockReturnValue({
      value: 'peer content',
      schemeType: 'string',
    });

    render(
      <ExplorerContent
        mode="ledger"
        selection={{ path: [42, ['*state*', 'peer.txt']], type: 'file' }}
        journalService={mockJournalService}
        refreshKey={0}
        ledgerView="content"
        onLedgerViewToggle={jest.fn()}
        onStageCreateFile={jest.fn().mockResolvedValue(undefined)}
        onStageCreateDirectory={jest.fn().mockResolvedValue(undefined)}
        onStageUploadFile={jest.fn().mockResolvedValue(undefined)}
        onStageRename={jest.fn().mockResolvedValue(undefined)}
        onStageDelete={jest.fn().mockResolvedValue(undefined)}
        onSelectPath={jest.fn()}
      />,
    );

    expect(await screen.findByText('Proof')).toBeInTheDocument();
    expect(await screen.findByText('Unpin')).toBeInTheDocument();
  });
});
