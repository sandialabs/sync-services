import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { QueryPane } from './QueryPane';
import { QueryTab } from '../types/workbench';

// Mock SchemeEditor component
jest.mock('./SchemeEditor', () => ({
  SchemeEditor: ({ value, onChange, placeholder }: { value: string; onChange?: (v: string) => void; placeholder?: string }) => (
    <textarea
      data-testid="scheme-editor"
      value={value}
      onChange={(e) => onChange?.(e.target.value)}
      placeholder={placeholder}
    />
  ),
}));

// Mock prettifyScheme
jest.mock('../utils/scheme', () => ({
  prettifyScheme: (code: string) => `prettified: ${code}`,
}));

describe('QueryPane', () => {
  const createTab = (overrides: Partial<QueryTab> = {}): QueryTab => ({
    id: '1',
    name: 'Query 1',
    content: '',
    ...overrides,
  });

  const defaultProps = {
    tabs: [createTab()],
    activeTabId: '1',
    theme: 'light' as const,
    vimMode: false,
    isLoading: false,
    onTabChange: jest.fn(),
    onTabContentChange: jest.fn(),
    onAddTab: jest.fn(),
    onCloseTab: jest.fn(),
    onRunQuery: jest.fn(),
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should render tab labels', () => {
    const tabs = [
      createTab({ id: '1', name: 'Query 1' }),
      createTab({ id: '2', name: 'Query 2' }),
    ];
    render(<QueryPane {...defaultProps} tabs={tabs} />);
    
    expect(screen.getByText('Query 1')).toBeInTheDocument();
    expect(screen.getByText('Query 2')).toBeInTheDocument();
  });

  it('should call onTabChange when clicking a tab', () => {
    const tabs = [
      createTab({ id: '1', name: 'Query 1' }),
      createTab({ id: '2', name: 'Query 2' }),
    ];
    render(<QueryPane {...defaultProps} tabs={tabs} />);
    
    fireEvent.click(screen.getByText('Query 2'));
    expect(defaultProps.onTabChange).toHaveBeenCalledWith('2');
  });

  it('should call onAddTab when clicking add button', () => {
    render(<QueryPane {...defaultProps} />);
    
    const addButton = screen.getByTitle('New tab');
    fireEvent.click(addButton);
    expect(defaultProps.onAddTab).toHaveBeenCalled();
  });

  it('should call onCloseTab when clicking close button', () => {
    const tabs = [
      createTab({ id: '1', name: 'Query 1' }),
      createTab({ id: '2', name: 'Query 2' }),
    ];
    render(<QueryPane {...defaultProps} tabs={tabs} />);
    
    const closeButtons = screen.getAllByText('×');
    fireEvent.click(closeButtons[0]);
    expect(defaultProps.onCloseTab).toHaveBeenCalledWith('1');
  });

  it('should not show close button when only one tab', () => {
    render(<QueryPane {...defaultProps} />);
    
    // The × for close should not be present (only the add button +)
    const closeButtons = screen.queryAllByText('×');
    expect(closeButtons).toHaveLength(0);
  });

  it('should call onRunQuery when clicking run button', () => {
    const tabs = [createTab({ content: '(+ 1 2)' })];
    render(<QueryPane {...defaultProps} tabs={tabs} />);
    
    const runButton = screen.getByText('▶ Run');
    fireEvent.click(runButton);
    expect(defaultProps.onRunQuery).toHaveBeenCalled();
  });

  it('should disable run button when loading', () => {
    const tabs = [createTab({ content: '(+ 1 2)' })];
    render(<QueryPane {...defaultProps} tabs={tabs} isLoading={true} />);
    
    // When loading, the button shows a spinner instead of text, so find by class
    const runButton = document.querySelector('.run-button');
    expect(runButton).toBeDisabled();
  });

  it('should disable run button when content is empty', () => {
    render(<QueryPane {...defaultProps} />);
    
    const runButton = screen.getByText('▶ Run');
    expect(runButton).toBeDisabled();
  });

  it('should call onTabContentChange when editor content changes', () => {
    render(<QueryPane {...defaultProps} />);
    
    const editor = screen.getByTestId('scheme-editor');
    fireEvent.change(editor, { target: { value: '(new content)' } });
    
    expect(defaultProps.onTabContentChange).toHaveBeenCalledWith('1', '(new content)');
  });

  it('should show current tab content in editor', () => {
    const tabs = [createTab({ content: '(existing content)' })];
    render(<QueryPane {...defaultProps} tabs={tabs} />);
    
    const editor = screen.getByTestId('scheme-editor');
    expect(editor).toHaveValue('(existing content)');
  });

  it('should call onTabContentChange with prettified code when prettify is clicked', () => {
    const tabs = [createTab({ content: '(+ 1 2)' })];
    render(<QueryPane {...defaultProps} tabs={tabs} />);
    
    const prettifyButton = screen.getByTitle('Prettify code');
    fireEvent.click(prettifyButton);
    
    expect(defaultProps.onTabContentChange).toHaveBeenCalledWith('1', 'prettified: (+ 1 2)');
  });

  it('should disable prettify button when content is empty', () => {
    render(<QueryPane {...defaultProps} />);
    
    const prettifyButton = screen.getByTitle('Prettify code');
    expect(prettifyButton).toBeDisabled();
  });

  it('should show keyboard hint', () => {
    render(<QueryPane {...defaultProps} />);
    
    // Should show either ⌘+Enter or Ctrl+Enter
    expect(screen.getByText(/\+Enter to run/)).toBeInTheDocument();
  });
});
