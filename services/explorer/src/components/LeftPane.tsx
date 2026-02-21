import React, { useState } from 'react';
import './LeftPane.css';
import NavigationTab from './NavigationTab';
import PeerInfoTab from './PeerInfoTab';
import { AppState, JournalPath } from '../types';
import { JournalService } from '../services/JournalService';

interface LeftPaneProps {
  appState: AppState;
  journalService: JournalService | null;
  onPathSelect: (path: JournalPath) => void;
  onExpandedNodesChange: (expandedNodes: Set<string>) => void;
}

const LeftPane: React.FC<LeftPaneProps> = ({
  appState,
  journalService,
  onPathSelect,
  onExpandedNodesChange,
}) => {
  const [activeTab, setActiveTab] = useState<'navigation' | 'peers'>('navigation');

  return (
    <div className="pane left-pane">
      <div className="tabs">
        <button
          className={`tab ${activeTab === 'navigation' ? 'active' : ''}`}
          onClick={() => setActiveTab('navigation')}
        >
          Navigation
        </button>
        <button
          className={`tab ${activeTab === 'peers' ? 'active' : ''}`}
          onClick={() => setActiveTab('peers')}
        >
          Peers
        </button>
      </div>
      <div className="tab-content">
        {activeTab === 'navigation' ? (
          <NavigationTab
            appState={appState}
            journalService={journalService}
            onPathSelect={onPathSelect}
            onExpandedNodesChange={onExpandedNodesChange}
          />
        ) : (
          <PeerInfoTab
            appState={appState}
            journalService={journalService}
          />
        )}
      </div>
    </div>
  );
};

export default LeftPane;
