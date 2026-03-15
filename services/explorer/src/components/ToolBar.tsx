import React, { useState } from 'react';
import './ToolBar.css';
import HelpModal from './HelpModal';
import { ExplorerMode } from '../types';

interface ToolBarProps {
  authentication: string;
  error: string | null;
  isLoading: boolean;
  mode: ExplorerMode;
  theme: 'light' | 'dark';
  onAuthenticationChange: (authentication: string) => void;
  onModeChange: (mode: ExplorerMode) => void;
  onThemeToggle: () => void;
}

const ToolBar: React.FC<ToolBarProps> = ({
  authentication,
  error,
  isLoading,
  mode,
  theme,
  onAuthenticationChange,
  onModeChange,
  onThemeToggle,
}) => {
  const [showHelp, setShowHelp] = useState(false);

  const handleLogoClick = () => {
    window.location.href = window.location.pathname + window.location.search;
  };

  return (
    <>
      <div className="toolbar">
        <div className="toolbar-left">
          <img
            src={process.env.PUBLIC_URL + '/logo.png'}
            alt="Synchronic Web"
            className="toolbar-logo"
            onClick={handleLogoClick}
            title="Return to home"
          />
          <div className="mode-switch">
            <button
              className={`tab ${mode === 'ledger' ? 'active' : ''}`}
              onClick={() => onModeChange('ledger')}
            >
              Ledger
            </button>
            <button
              className={`tab ${mode === 'stage' ? 'active' : ''}`}
              onClick={() => onModeChange('stage')}
            >
              Stage
            </button>
          </div>
        </div>

        <div className="toolbar-right">
          <input
            type="password"
            className="input toolbar-password"
            placeholder="Authentication password"
            value={authentication}
            onChange={(event) => onAuthenticationChange(event.target.value)}
          />
          <button
            className="button button-icon"
            onClick={onThemeToggle}
            title={theme === 'light' ? 'Switch to dark mode' : 'Switch to light mode'}
          >
            {theme === 'light' ? '◐' : '◑'}
          </button>
          <button
            className="button button-icon"
            onClick={() => setShowHelp(true)}
            title="Help"
            aria-label="Help"
          >
            ⓘ
          </button>
        </div>
      </div>

      {(error || isLoading) && (
        <div className={`toolbar-status-line ${error ? 'error' : ''}`}>
          {error ?? 'Loading...'}
        </div>
      )}

      {showHelp && <HelpModal onClose={() => setShowHelp(false)} />}
    </>
  );
};

export default ToolBar;
