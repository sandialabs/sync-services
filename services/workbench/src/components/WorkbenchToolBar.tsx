import React from 'react';
import { Theme } from '../types/workbench';

interface WorkbenchToolBarProps {
  theme: Theme;
  vimMode: boolean;
  isLoading: boolean;
  error: string | null;
  onThemeToggle: () => void;
  onVimModeToggle: () => void;
  onHelpClick: () => void;
  onLogoClick: () => void;
}

export const WorkbenchToolBar: React.FC<WorkbenchToolBarProps> = ({
  theme,
  vimMode,
  isLoading,
  error,
  onThemeToggle,
  onVimModeToggle,
  onHelpClick,
  onLogoClick,
}) => {
  return (
    <div className="toolbar">
      <img
        src={process.env.PUBLIC_URL + '/logo.png'}
        alt="Synchronic Web"
        className="toolbar-logo"
        onClick={onLogoClick}
        title="Return to home"
      />

      <div className="toolbar-title">Synchronic Web Workbench</div>

      <div className="toolbar-status">
        {isLoading && (
          <span className="status-loading">
            <span className="loading-spinner" /> Executing...
          </span>
        )}
        {error && !isLoading && <span className="status-error">Error: {error}</span>}
        {!isLoading && !error && <span className="status-ready">Ready</span>}
      </div>

      <button
        className={`button button-icon vim-toggle ${vimMode ? 'active' : ''}`}
        onClick={onVimModeToggle}
        title={vimMode ? 'Disable Vim mode' : 'Enable Vim mode'}
      >
        vim
      </button>

      <button
        className="button button-icon"
        onClick={onThemeToggle}
        title={theme === 'light' ? 'Switch to dark mode' : 'Switch to light mode'}
      >
        {theme === 'light' ? '🌙' : '☀️'}
      </button>

      <button className="button button-icon" onClick={onHelpClick} title="Help">
        ⓘ
      </button>
    </div>
  );
};
