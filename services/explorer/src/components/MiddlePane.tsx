import React, { useState } from 'react';
import './MiddlePane.css';
import ContentTab from './ContentTab';
import VerificationTab from './VerificationTab';
import { AppState } from '../types';
import { JournalService } from '../services/JournalService';

interface MiddlePaneProps {
  appState: AppState;
  journalService: JournalService | null;
  onContentUpdate: () => void;
}

const MiddlePane: React.FC<MiddlePaneProps> = ({
  appState,
  journalService,
  onContentUpdate,
}) => {
  const [activeTab, setActiveTab] = useState<'content' | 'verification'>('content');

  return (
    <div className="pane middle-pane">
      <div className="tabs">
        <button
          className={`tab ${activeTab === 'content' ? 'active' : ''}`}
          onClick={() => setActiveTab('content')}
        >
          Content
        </button>
        <button
          className={`tab ${activeTab === 'verification' ? 'active' : ''}`}
          onClick={() => setActiveTab('verification')}
        >
          Verification
        </button>
      </div>
      <div className="tab-content">
        {activeTab === 'content' ? (
          <ContentTab
            appState={appState}
            journalService={journalService}
            onContentUpdate={onContentUpdate}
          />
        ) : (
          <VerificationTab
            appState={appState}
            journalService={journalService}
          />
        )}
      </div>
    </div>
  );
};

export default MiddlePane;
