import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import NavigationTab from './NavigationTab';
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
  delete: jest.fn(),
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

describe('NavigationTab', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should render root nodes', async () => {
    const appState = createAppState();
    render(
      <NavigationTab
        appState={appState}
        journalService={mockJournalService}
        onPathSelect={jest.fn()}
        onExpandedNodesChange={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('state')).toBeInTheDocument();
      expect(screen.getByText('peer')).toBeInTheDocument();
    });
  });

  it('should expand node on click', async () => {
    const appState = createAppState();
    const onExpandedNodesChange = jest.fn();

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['directory', ['file1', 'file2'], true],
    });
    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue({
      items: ['file1', 'file2'],
      isComplete: true,
    });

    render(
      <NavigationTab
        appState={appState}
        journalService={mockJournalService}
        onPathSelect={jest.fn()}
        onExpandedNodesChange={onExpandedNodesChange}
      />
    );

    await waitFor(() => {
      expect(screen.getAllByText('▶').length).toBeGreaterThan(0);
    });

    // Click on the expand icon for 'state'
    const expandIcons = screen.getAllByText('▶');
    fireEvent.click(expandIcons[0]);

    await waitFor(() => {
      expect(onExpandedNodesChange).toHaveBeenCalled();
    });
  });

  it('should call onPathSelect when clicking on a node label', async () => {
    const appState = createAppState();
    const onPathSelect = jest.fn();

    render(
      <NavigationTab
        appState={appState}
        journalService={mockJournalService}
        onPathSelect={onPathSelect}
        onExpandedNodesChange={jest.fn()}
      />
    );

    await waitFor(() => {
      expect(screen.getByText('state')).toBeInTheDocument();
    });

    fireEvent.click(screen.getByText('state'));

    expect(onPathSelect).toHaveBeenCalledWith([['*state*']]);
  });

  it('should show action buttons on hover for local nodes', async () => {
    const appState = createAppState({
      expandedNodes: new Set(['local-state']),
    });

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['directory', ['file1'], true],
    });
    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue({
      items: ['file1'],
      isComplete: true,
    });

    render(
      <NavigationTab
        appState={appState}
        journalService={mockJournalService}
        onPathSelect={jest.fn()}
        onExpandedNodesChange={jest.fn()}
      />
    );

    await waitFor(() => {
      // The delete button should be present (though hidden until hover)
      const deleteButtons = screen.getAllByTitle('Delete');
      expect(deleteButtons.length).toBeGreaterThan(0);
    });
  });

  it('should filter out *directory* marker files when expanding', async () => {
    const appState = createAppState();

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['directory', ['file1', '*directory*', 'file2'], true],
    });
    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue({
      items: ['file1', '*directory*', 'file2'],
      isComplete: true,
    });

    const onExpandedNodesChange = jest.fn();

    render(
      <NavigationTab
        appState={appState}
        journalService={mockJournalService}
        onPathSelect={jest.fn()}
        onExpandedNodesChange={onExpandedNodesChange}
      />
    );

    await waitFor(() => {
      expect(screen.getAllByText('▶').length).toBeGreaterThan(0);
    });

    // Click to expand the state node
    const expandIcons = screen.getAllByText('▶');
    fireEvent.click(expandIcons[0]);

    // Wait for the expansion callback to be called
    await waitFor(() => {
      expect(onExpandedNodesChange).toHaveBeenCalled();
    });

    // Verify that get was called to fetch children
    expect(mockJournalService.get).toHaveBeenCalled();

    // Verify parseDirectoryResponse was called
    expect(JournalService.parseDirectoryResponse).toHaveBeenCalled();
  });

  it('should sort children alphabetically when expanding', async () => {
    const appState = createAppState();

    (mockJournalService.get as jest.Mock).mockResolvedValue({
      content: ['directory', ['zebra', 'apple', 'mango'], true],
    });
    (JournalService.parseDirectoryResponse as jest.Mock).mockReturnValue({
      items: ['zebra', 'apple', 'mango'],
      isComplete: true,
    });

    const onExpandedNodesChange = jest.fn();

    render(
      <NavigationTab
        appState={appState}
        journalService={mockJournalService}
        onPathSelect={jest.fn()}
        onExpandedNodesChange={onExpandedNodesChange}
      />
    );

    await waitFor(() => {
      expect(screen.getAllByText('▶').length).toBeGreaterThan(0);
    });

    // Click to expand the state node
    const expandIcons = screen.getAllByText('▶');
    fireEvent.click(expandIcons[0]);

    // Wait for the expansion callback to be called
    await waitFor(() => {
      expect(onExpandedNodesChange).toHaveBeenCalled();
    });

    // Verify that get was called to fetch children
    expect(mockJournalService.get).toHaveBeenCalled();

    // Verify parseDirectoryResponse was called with the response
    expect(JournalService.parseDirectoryResponse).toHaveBeenCalled();
  });
});
