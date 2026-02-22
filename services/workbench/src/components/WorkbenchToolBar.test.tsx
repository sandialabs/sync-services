import React from 'react';
import { render, screen, fireEvent } from '@testing-library/react';
import { WorkbenchToolBar } from './WorkbenchToolBar';

describe('WorkbenchToolBar', () => {
  const defaultProps = {
    theme: 'light' as const,
    vimMode: false,
    isLoading: false,
    error: null,
    onThemeToggle: jest.fn(),
    onVimModeToggle: jest.fn(),
    onHelpClick: jest.fn(),
    onLogoClick: jest.fn(),
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('should render the toolbar title', () => {
    render(<WorkbenchToolBar {...defaultProps} />);
    expect(screen.getByText('Synchronic Web Workbench')).toBeInTheDocument();
  });

  it('should render the logo', () => {
    render(<WorkbenchToolBar {...defaultProps} />);
    const logo = screen.getByAltText('Synchronic Web');
    expect(logo).toBeInTheDocument();
  });

  it('should call onLogoClick when logo is clicked', () => {
    render(<WorkbenchToolBar {...defaultProps} />);
    const logo = screen.getByAltText('Synchronic Web');
    fireEvent.click(logo);
    expect(defaultProps.onLogoClick).toHaveBeenCalled();
  });

  it('should show Ready status when not loading and no error', () => {
    render(<WorkbenchToolBar {...defaultProps} />);
    expect(screen.getByText('Ready')).toBeInTheDocument();
  });

  it('should show loading status when isLoading is true', () => {
    render(<WorkbenchToolBar {...defaultProps} isLoading={true} />);
    expect(screen.getByText('Executing...')).toBeInTheDocument();
  });

  it('should show error status when error is present', () => {
    render(<WorkbenchToolBar {...defaultProps} error="Something went wrong" />);
    expect(screen.getByText('Error: Something went wrong')).toBeInTheDocument();
  });

  it('should not show error when loading', () => {
    render(<WorkbenchToolBar {...defaultProps} isLoading={true} error="Error" />);
    expect(screen.queryByText('Error: Error')).not.toBeInTheDocument();
    expect(screen.getByText('Executing...')).toBeInTheDocument();
  });

  it('should call onThemeToggle when theme button is clicked', () => {
    render(<WorkbenchToolBar {...defaultProps} />);
    const themeButton = screen.getByTitle('Switch to dark mode');
    fireEvent.click(themeButton);
    expect(defaultProps.onThemeToggle).toHaveBeenCalled();
  });

  it('should show moon icon in light mode', () => {
    render(<WorkbenchToolBar {...defaultProps} theme="light" />);
    expect(screen.getByText('🌙')).toBeInTheDocument();
  });

  it('should show sun icon in dark mode', () => {
    render(<WorkbenchToolBar {...defaultProps} theme="dark" />);
    expect(screen.getByText('☀️')).toBeInTheDocument();
  });

  it('should call onVimModeToggle when vim button is clicked', () => {
    render(<WorkbenchToolBar {...defaultProps} />);
    const vimButton = screen.getByText('vim');
    fireEvent.click(vimButton);
    expect(defaultProps.onVimModeToggle).toHaveBeenCalled();
  });

  it('should show vim button as active when vimMode is true', () => {
    const { container } = render(<WorkbenchToolBar {...defaultProps} vimMode={true} />);
    const vimButton = container.querySelector('.vim-toggle.active');
    expect(vimButton).toBeInTheDocument();
  });

  it('should call onHelpClick when help button is clicked', () => {
    render(<WorkbenchToolBar {...defaultProps} />);
    const helpButton = screen.getByTitle('Help');
    fireEvent.click(helpButton);
    expect(defaultProps.onHelpClick).toHaveBeenCalled();
  });
});
