import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { WorkbenchHelpModal } from './WorkbenchHelpModal';

describe('WorkbenchHelpModal', () => {
  it('should render the modal with title', () => {
    render(<WorkbenchHelpModal onClose={jest.fn()} />);
    expect(screen.getByText('Synchronic Web Workbench Help')).toBeInTheDocument();
  });

  it('should render all section headers', () => {
    render(<WorkbenchHelpModal onClose={jest.fn()} />);
    
    expect(screen.getByText('Overview')).toBeInTheDocument();
    expect(screen.getByText('Top Pane (Input)')).toBeInTheDocument();
    expect(screen.getByText('Bottom Pane (Output)')).toBeInTheDocument();
    expect(screen.getByText('Left Pane (Reference)')).toBeInTheDocument();
    expect(screen.getByText('Right Pane (History)')).toBeInTheDocument();
    expect(screen.getByText('Editor Settings')).toBeInTheDocument();
    expect(screen.getByText('Keyboard Shortcuts')).toBeInTheDocument();
  });

  it('should call onClose when close button is clicked', () => {
    const onClose = jest.fn();
    render(<WorkbenchHelpModal onClose={onClose} />);
    
    const closeButton = screen.getByText('×');
    fireEvent.click(closeButton);
    
    expect(onClose).toHaveBeenCalled();
  });

  it('should call onClose when overlay is clicked', () => {
    const onClose = jest.fn();
    const { container } = render(<WorkbenchHelpModal onClose={onClose} />);
    
    const overlay = container.querySelector('.modal-overlay');
    fireEvent.click(overlay!);
    
    expect(onClose).toHaveBeenCalled();
  });

  it('should not call onClose when modal content is clicked', () => {
    const onClose = jest.fn();
    const { container } = render(<WorkbenchHelpModal onClose={onClose} />);
    
    const content = container.querySelector('.modal-content');
    fireEvent.click(content!);
    
    expect(onClose).not.toHaveBeenCalled();
  });

  it('should mention keyboard shortcut for running queries', () => {
    render(<WorkbenchHelpModal onClose={jest.fn()} />);
    
    // Use getAllByText since there are multiple mentions of +Enter
    const texts = screen.getAllByText(/\+Enter/);
    expect(texts.length).toBeGreaterThan(0);
  });

  it('should describe the prettify button', () => {
    render(<WorkbenchHelpModal onClose={jest.fn()} />);
    expect(screen.getByText(/✨ button to prettify/)).toBeInTheDocument();
  });
});
