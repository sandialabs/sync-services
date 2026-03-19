import React, { useState, useEffect } from 'react';
import { AppState, PeerInfo } from '../types';
import { JournalService } from '../services/JournalService';

interface PeerInfoTabProps {
  appState: AppState;
  journalService: JournalService | null;
}

const PeerInfoTab: React.FC<PeerInfoTabProps> = ({
  appState,
  journalService,
}) => {
  const [peers, setPeers] = useState<PeerInfo[]>([]);
  const [newPeerName, setNewPeerName] = useState('');
  const [newPeerEndpoint, setNewPeerEndpoint] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  useEffect(() => {
    if (journalService && appState.rootIndex >= 0) {
      loadPeers();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [journalService, appState.rootIndex]);

  const loadPeers = async () => {
    if (!journalService) return;

    try {
      const peerList = await journalService.getBridges();
      setPeers(peerList);
    } catch (error) {
      console.error('Failed to load bridges:', error);
      setPeers([]);
    }
  };

  const handleAddPeer = async () => {
    if (!journalService || !newPeerName || !newPeerEndpoint) return;

    setIsLoading(true);
    try {
      const success = await journalService.addBridge(newPeerName, newPeerEndpoint);
      if (success) {
        setNewPeerName('');
        setNewPeerEndpoint('');
        await loadPeers();
      } else {
        alert('Failed to add bridge: Operation returned false');
      }
    } catch (error) {
      console.error('Failed to add bridge:', error);
      alert(`Failed to add bridge: ${error instanceof Error ? error.message : 'Unknown error'}`);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div>
      <div className="peer-list">
        {peers.map((peer, index) => (
          <div key={index} className="peer-item">
            <div className="peer-name">{peer.name}</div>
            {peer.endpoint && (
              <div className="peer-endpoint">{peer.endpoint}</div>
            )}
          </div>
        ))}
      </div>

      <form className="add-peer-form" onSubmit={(e) => { e.preventDefault(); handleAddPeer(); }}>
        <h3>Add New Bridge</h3>
        <input
          type="text"
          className="input"
          placeholder="Bridge name"
          value={newPeerName}
          onChange={(e) => setNewPeerName(e.target.value)}
        />
        <input
          type="text"
          className="input"
          placeholder="Bridge endpoint URL"
          value={newPeerEndpoint}
          onChange={(e) => setNewPeerEndpoint(e.target.value)}
        />
        <button
          type="submit"
          className="button button-primary"
          disabled={!newPeerName || !newPeerEndpoint || isLoading}
        >
          {isLoading ? 'Adding...' : 'Add Bridge'}
        </button>
      </form>
    </div>
  );
};

export default PeerInfoTab;
