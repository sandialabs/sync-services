import React, { useState } from 'react';
import './ToolBar.css';
import HelpModal from './HelpModal';

interface ToolBarProps {
  endpoint: string;
  authentication: string;
  rootIndex: number;
  isLoading: boolean;
  error: string | null;
  theme: 'light' | 'dark';
  onEndpointChange: (endpoint: string) => void;
  onAuthenticationChange: (authentication: string) => void;
  onSynchronize: () => void;
  onThemeToggle: () => void;
}

const ToolBar: React.FC<ToolBarProps> = ({
  endpoint,
  authentication,
  rootIndex,
  isLoading,
  error,
  theme,
  onEndpointChange,
  onAuthenticationChange,
  onSynchronize,
  onThemeToggle,
}) => {
  const [showHelp, setShowHelp] = useState(false);

  const handleLogoClick = () => {
    // Navigate to home by clearing the hash and reloading
    window.location.href = window.location.pathname + window.location.search;
  };

  return (
    <>
      <div className="toolbar">
        <img 
          src="/logo.png" 
          alt="Synchronic Web" 
          className="toolbar-logo"
          onClick={handleLogoClick}
          style={{ cursor: 'pointer' }}
          title="Return to home"
        />

        <div className="toolbar-inputs">
          <input
            type="text"
            className="input"
            placeholder="Journal endpoint URL"
            value={endpoint}
            onChange={(e) => onEndpointChange(e.target.value)}
          />
          <input
            type="password"
            className="input"
            placeholder="Authentication password"
            value={authentication}
            onChange={(e) => onAuthenticationChange(e.target.value)}
          />
        </div>

        <button 
          className="button button-primary"
          onClick={onSynchronize}
          disabled={!endpoint || !authentication || isLoading}
        >
          {isLoading ? <span className="loading-spinner" /> : 'Synchronize'}
        </button>

        <div className="toolbar-status">
          {rootIndex >= 0 && !isLoading && !error && (
            <div className="index-display">
              <span className="index-label">Index:</span>
              <span className="index-value">{rootIndex}</span>
            </div>
          )}
          {isLoading && <span className="status-loading">Loading...</span>}
          {error && <span className="status-error">{error}</span>}
        </div>

        <button 
          className="button button-icon"
          onClick={onThemeToggle}
          title={theme === 'light' ? 'Switch to dark mode' : 'Switch to light mode'}
        >
          {theme === 'light' ? '🌙' : '☀️'}
        </button>

        <button 
          className="button button-icon"
          onClick={() => setShowHelp(true)}
          title="Help"
        >
          ⓘ
        </button>
      </div>

      {showHelp && <HelpModal onClose={() => setShowHelp(false)} />}
    </>
  );
};

export default ToolBar;
