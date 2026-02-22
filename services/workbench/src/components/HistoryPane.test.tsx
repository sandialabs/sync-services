import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { HistoryPane } from './HistoryPane';
import { HistoryEntry } from '../types/workbench';

describe('HistoryPane', () => {
  const createHistoryEntry = (overrides: Partial<HistoryEntry> = {}): HistoryEntry => ({
    id: 'test-id',
    timestamp: new Date('2024-01-15T10:30:00'),
    query: '(+ 1 2)',
    request: 'POST /interface',
    response: 'HTTP 200 OK',
    result: 3,
    ...overrides,
  });

  it('should render empty state when no history', () => {
    render(
      <HistoryPane
        history={[]}
        selectedEntry={null}
        onSelectEntry={jest.fn()}
      />
    );

    expect(screen.getByText('No queries executed yet')).toBeInTheDocument();
  });

  it('should render history entries', () => {
    const history = [
      createHistoryEntry({ id: '1', query: '(+ 1 2)' }),
      createHistoryEntry({ id: '2', query: '(* 3 4)' }),
    ];

    render(
      <HistoryPane
        history={history}
        selectedEntry={null}
        onSelectEntry={jest.fn()}
      />
    );

    expect(screen.getByText('(+ 1 2)')).toBeInTheDocument();
    expect(screen.getByText('(* 3 4)')).toBeInTheDocument();
  });

  it('should truncate long query previews', () => {
    const longQuery = '(this-is-a-very-long-function-name-that-exceeds-forty-characters arg1 arg2)';
    const history = [createHistoryEntry({ query: longQuery })];

    render(
      <HistoryPane
        history={history}
        selectedEntry={null}
        onSelectEntry={jest.fn()}
      />
    );

    // Look for the truncated text that ends with ...
    const preview = screen.getByText(/this-is-a-very-long-function-name-that-\.\.\./);
    expect(preview).toBeInTheDocument();
  });

  it('should call onSelectEntry when clicking an entry', () => {
    const onSelectEntry = jest.fn();
    const entry = createHistoryEntry();

    render(
      <HistoryPane
        history={[entry]}
        selectedEntry={null}
        onSelectEntry={onSelectEntry}
      />
    );

    fireEvent.click(screen.getByText('(+ 1 2)'));
    expect(onSelectEntry).toHaveBeenCalledWith(entry);
  });

  it('should highlight selected entry', () => {
    const entry = createHistoryEntry();

    const { container } = render(
      <HistoryPane
        history={[entry]}
        selectedEntry={entry}
        onSelectEntry={jest.fn()}
      />
    );

    const selectedElement = container.querySelector('.history-entry.selected');
    expect(selectedElement).toBeInTheDocument();
  });

  it('should show error indicator for failed queries', () => {
    const entry = createHistoryEntry({ error: 'Something went wrong' });

    const { container } = render(
      <HistoryPane
        history={[entry]}
        selectedEntry={null}
        onSelectEntry={jest.fn()}
      />
    );

    const errorElement = container.querySelector('.history-entry.error');
    expect(errorElement).toBeInTheDocument();
    expect(screen.getByText('✗')).toBeInTheDocument();
  });

  it('should show success indicator for successful queries', () => {
    const entry = createHistoryEntry();

    render(
      <HistoryPane
        history={[entry]}
        selectedEntry={null}
        onSelectEntry={jest.fn()}
      />
    );

    expect(screen.getByText('✓')).toBeInTheDocument();
  });

  it('should format timestamp correctly', () => {
    const entry = createHistoryEntry({
      timestamp: new Date('2024-01-15T14:30:45'),
    });

    render(
      <HistoryPane
        history={[entry]}
        selectedEntry={null}
        onSelectEntry={jest.fn()}
      />
    );

    // The exact format depends on locale, but should contain time components
    const timeElement = screen.getByText(/\d{1,2}:\d{2}:\d{2}/);
    expect(timeElement).toBeInTheDocument();
  });

  it('should render header', () => {
    render(
      <HistoryPane
        history={[]}
        selectedEntry={null}
        onSelectEntry={jest.fn()}
      />
    );

    expect(screen.getByText('History')).toBeInTheDocument();
  });
});
