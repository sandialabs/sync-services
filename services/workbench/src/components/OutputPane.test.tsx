import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { OutputPane } from './OutputPane';
import { HistoryEntry } from '../types/workbench';

// Mock SchemeEditor component
jest.mock('./SchemeEditor', () => ({
  SchemeEditor: ({ value, readOnly }: { value: string; readOnly?: boolean }) => (
    <div data-testid="scheme-editor" data-readonly={readOnly}>
      {value}
    </div>
  ),
}));

describe('OutputPane', () => {
  const createHistoryEntry = (overrides: Partial<HistoryEntry> = {}): HistoryEntry => ({
    id: 'test-id',
    timestamp: new Date(),
    query: '(+ 1 2)',
    request: 'POST /interface\n\n(+ 1 2)',
    response: 'HTTP 200 OK\n\n3',
    result: 3,
    ...overrides,
  });

  const defaultProps = {
    selectedEntry: null,
    theme: 'light' as const,
    vimMode: false,
  };

  it('should show empty state when no entry is selected', () => {
    render(<OutputPane {...defaultProps} />);
    expect(screen.getByText('Run a query to see results')).toBeInTheDocument();
  });

  it('should render all tab buttons', () => {
    render(<OutputPane {...defaultProps} />);
    
    expect(screen.getByRole('button', { name: 'Query' })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Result' })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Request' })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Response' })).toBeInTheDocument();
  });

  it('should show result tab by default', () => {
    const entry = createHistoryEntry({ result: { value: 42 } });
    render(<OutputPane {...defaultProps} selectedEntry={entry} />);
    
    const editor = screen.getByTestId('scheme-editor');
    expect(editor.textContent).toContain('42');
  });

  it('should switch to query tab when clicked', () => {
    const entry = createHistoryEntry({ query: '(my-query)' });
    render(<OutputPane {...defaultProps} selectedEntry={entry} />);
    
    fireEvent.click(screen.getByRole('button', { name: 'Query' }));
    
    const editor = screen.getByTestId('scheme-editor');
    expect(editor.textContent).toContain('(my-query)');
  });

  it('should switch to request tab when clicked', () => {
    const entry = createHistoryEntry({ request: 'POST /test' });
    render(<OutputPane {...defaultProps} selectedEntry={entry} />);
    
    fireEvent.click(screen.getByRole('button', { name: 'Request' }));
    
    const editor = screen.getByTestId('scheme-editor');
    expect(editor.textContent).toContain('POST /test');
  });

  it('should switch to response tab when clicked', () => {
    const entry = createHistoryEntry({ response: 'HTTP 200' });
    render(<OutputPane {...defaultProps} selectedEntry={entry} />);
    
    fireEvent.click(screen.getByRole('button', { name: 'Response' }));
    
    const editor = screen.getByTestId('scheme-editor');
    expect(editor.textContent).toContain('HTTP 200');
  });

  it('should show error message when entry has error', () => {
    const entry = createHistoryEntry({ error: 'Something went wrong' });
    render(<OutputPane {...defaultProps} selectedEntry={entry} />);
    
    const editor = screen.getByTestId('scheme-editor');
    expect(editor.textContent).toContain('Something went wrong');
  });

  it('should format JSON result', () => {
    const entry = createHistoryEntry({ result: { key: 'value' } });
    render(<OutputPane {...defaultProps} selectedEntry={entry} />);
    
    const editor = screen.getByTestId('scheme-editor');
    expect(editor.textContent).toContain('key');
    expect(editor.textContent).toContain('value');
  });

  it('should handle string result', () => {
    const entry = createHistoryEntry({ result: 'plain string' });
    render(<OutputPane {...defaultProps} selectedEntry={entry} />);
    
    const editor = screen.getByTestId('scheme-editor');
    expect(editor.textContent).toContain('plain string');
  });

  it('should disable copy button when no entry selected', () => {
    render(<OutputPane {...defaultProps} />);
    
    const copyButton = screen.getByTitle('Copy output');
    expect(copyButton).toBeDisabled();
  });

  it('should disable download button when no entry selected', () => {
    render(<OutputPane {...defaultProps} />);
    
    const downloadButton = screen.getByTitle('Download output');
    expect(downloadButton).toBeDisabled();
  });

  it('should enable copy button when entry is selected', () => {
    const entry = createHistoryEntry();
    render(<OutputPane {...defaultProps} selectedEntry={entry} />);
    
    const copyButton = screen.getByTitle('Copy output');
    expect(copyButton).not.toBeDisabled();
  });
});
