import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import ContentTab from './ContentTab';
import { JournalService } from '../services/JournalService';
import { AppState } from '../types';

// Mock JournalService
jest.mock('../services/JournalService', () => ({
  JournalService: {
    parseDirectoryResponse: jest.fn(),
    extractSchemeValue: jest.fn((value) => ({ value, schemeType: null })),
  },
}));

const mockJournalService = {
  get: jest.fn(),
  set: jest.fn(),
  pin: jest.fn(),
  unpin: jest.fn(),
} as unknown as JournalService;

const createAppState = (overrides: Partial<AppState> = {}): AppState => ({
  endpoint: 'http://test.com',
  authentication: 'password',
  rootIndex: 100,
  selectedPath: null,
  expandedNodes: new Set(),
  isLoading: false,
  error: null,
  ...overrides,
});

describe('ContentTab', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    // Reset the mock to default behavior
    (JournalService.extractSchemeValue as jest.Mock).mockImplementation((value) => ({ value, schemeType: null }));
    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue(null);
  });

  it('should show empty state when no path is selected', async () => {
    const appState = createAppState();
    render(
      <ContentTab
        appState={appState}
        journalService={mockJournalService}
        onContentUpdate={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('Select a document or directory to view its contents')).toBeInTheDocument();
    });
  });

  it('should load and display content when path is selected', async () => {
    const appState = createAppState({
      selectedPath: [['*state*', 'test']],
    });

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: { '*type/string*': 'Hello World' },
      'pinned?': null,
      proof: {},
    });

    (JournalService.extractSchemeValue as jest.Mock).mockReturnValue({
      value: 'Hello World',
      schemeType: 'string',
    });

    render(
      <ContentTab
        appState={appState}
        journalService={mockJournalService}
        onContentUpdate={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('Hello World')).toBeInTheDocument();
    });
  });

  it('should display directory contents', async () => {
    const appState = createAppState({
      selectedPath: [['*state*']],
    });

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['directory', ['file1', 'file2', 'file3'], true],
      'pinned?': null,
      proof: {},
    });

    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue({
      items: ['file1', 'file2', 'file3'],
      isComplete: true,
    });

    render(
      <ContentTab
        appState={appState}
        journalService={mockJournalService}
        onContentUpdate={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('Directory Contents:')).toBeInTheDocument();
      expect(screen.getByText('file1')).toBeInTheDocument();
      expect(screen.getByText('file2')).toBeInTheDocument();
      expect(screen.getByText('file3')).toBeInTheDocument();
    });
  });

  it('should show edit button for local paths', async () => {
    const appState = createAppState({
      selectedPath: [['*state*', 'test']],
    });

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: { '*type/string*': 'editable content' },
      'pinned?': null,
      proof: {},
    });

    (JournalService.extractSchemeValue as jest.Mock).mockReturnValue({
      value: 'editable content',
      schemeType: 'string',
    });

    render(
      <ContentTab
        appState={appState}
        journalService={mockJournalService}
        onContentUpdate={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('Edit')).toBeInTheDocument();
    });
  });

  it('should show pin button for non-local versioned paths', async () => {
    const appState = createAppState({
      selectedPath: [-1, ['*peer*', 'alice', 'chain'], -1, ['*state*', 'test']],
    });

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: { '*type/string*': 'peer content' },
      'pinned?': null,
      proof: {},
    });

    (JournalService.extractSchemeValue as jest.Mock).mockReturnValue({
      value: 'peer content',
      schemeType: 'string',
    });

    render(
      <ContentTab
        appState={appState}
        journalService={mockJournalService}
        onContentUpdate={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('📌 Pin')).toBeInTheDocument();
    });
  });

  it('should handle pin action', async () => {
    const appState = createAppState({
      selectedPath: [-1, ['*state*', 'test']],
    });

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: { '*type/string*': 'content' },
      'pinned?': null,
      proof: {},
    });
    (mockJournalService.pin as jest.Mock).mockResolvedValue(true);

    (JournalService.extractSchemeValue as jest.Mock).mockReturnValue({
      value: 'content',
      schemeType: 'string',
    });

    render(
      <ContentTab
        appState={appState}
        journalService={mockJournalService}
        onContentUpdate={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('📌 Pin')).toBeInTheDocument();
    });

    fireEvent.click(screen.getByText('📌 Pin'));

    await waitFor(() => {
      expect(mockJournalService.pin).toHaveBeenCalledWith([-1, ['*state*', 'test']]);
    });
  });

  it('should display empty document message', async () => {
    const appState = createAppState({
      selectedPath: [['*state*', 'empty']],
    });

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['nothing'],
      'pinned?': null,
      proof: {},
    });

    render(
      <ContentTab
        appState={appState}
        journalService={mockJournalService}
        onContentUpdate={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('Empty document')).toBeInTheDocument();
    });
  });

  it('should display pruned document message', async () => {
    const appState = createAppState({
      selectedPath: [-100, ['*state*', 'old']],
    });

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['unknown'],
      'pinned?': null,
      proof: {},
    });

    render(
      <ContentTab
        appState={appState}
        journalService={mockJournalService}
        onContentUpdate={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('Document has been pruned')).toBeInTheDocument();
    });
  });
});
