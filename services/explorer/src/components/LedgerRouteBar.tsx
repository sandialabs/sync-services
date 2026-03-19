import React from 'react';
import { LedgerHop } from '../types';
import { normalizeSnapshotInput } from '../utils/ledgerRoute';

interface LedgerRouteBarProps {
  hops: LedgerHop[];
  peerChoices: string[] | null;
  rootIndex: number;
  onSynchronize: () => void;
  onSnapshotChange: (index: number, value: string) => void;
  onStepSnapshot: (index: number, direction: 'older' | 'newer') => void;
  onRemoveHop: () => void;
  onOpenPeerPicker: () => void;
  onClosePeerPicker: () => void;
  onChoosePeer: (peerName: string) => void;
}

const LedgerRouteBar: React.FC<LedgerRouteBarProps> = ({
  hops,
  peerChoices,
  rootIndex,
  onSynchronize,
  onSnapshotChange,
  onStepSnapshot,
  onRemoveHop,
  onOpenPeerPicker,
  onClosePeerPicker,
  onChoosePeer,
}) => {
  const pickerOpen = Array.isArray(peerChoices);
  const getSnapshotDisplayValue = (hop: LedgerHop, index: number): string => {
    if (index === 0 && hop.snapshot.trim().toLowerCase() === 'latest' && rootIndex >= 0) {
      return String(rootIndex);
    }
    return hop.snapshot;
  };

  const normalizeHopInput = (hop: LedgerHop, index: number, rawValue: string): string => {
    const trimmed = rawValue.trim().toLowerCase();

    if (index === 0) {
      if (trimmed === '' || trimmed === 'latest') {
        return 'latest';
      }

      const parsed = Number.parseInt(trimmed, 10);
      if (!Number.isNaN(parsed) && parsed >= 0) {
        if (rootIndex >= 0 && parsed >= rootIndex) {
          return 'latest';
        }
        return String(parsed);
      }

      return rootIndex >= 0 ? String(rootIndex) : 'latest';
    }

    return normalizeSnapshotInput(rawValue);
  };

  return (
    <div className="route-builder unified">
      {hops.map((hop, index) => (
        <React.Fragment key={hop.key}>
          {index > 0 && <span className="arrow big">→</span>}
          <div
            className={`hop-card-unified ${index === hops.length - 1 ? 'active-hop' : ''} ${
              index === 0 ? 'root-hop-card' : ''
            }`}
          >
            {index === 0 && (
              <button
                className="sync-pill sync-pill-inline"
                title="Synchronize latest committed root"
                onClick={onSynchronize}
              >
                <span className="sync-pill-icon">⟳</span>
              </button>
            )}
            <div className="hop-tag">{hop.name}</div>
            <div className="stepper linear">
              <button onClick={() => onStepSnapshot(index, 'older')}>-</button>
              <input
                value={getSnapshotDisplayValue(hop, index)}
                onChange={(event) => onSnapshotChange(index, event.target.value)}
                onBlur={(event) => onSnapshotChange(index, normalizeHopInput(hop, index, event.target.value))}
                onKeyDown={(event) => {
                  if (event.key === 'Enter') {
                    onSnapshotChange(index, normalizeHopInput(hop, index, event.currentTarget.value));
                    event.currentTarget.blur();
                  }
                }}
                aria-label={`${hop.name} snapshot`}
              />
              <button onClick={() => onStepSnapshot(index, 'newer')}>+</button>
            </div>
          </div>
        </React.Fragment>
      ))}

      <div className="route-control-rail">
        {pickerOpen ? (
          <div className="peer-picker-shell">
            <button className="route-action" title="Exit bridge selection" onClick={onClosePeerPicker}>
              ×
            </button>
            <div className="peer-rail">
              <div className="peer-rail-scroll">
                {peerChoices.length > 0 ? (
                  peerChoices.map((peerName) => (
                    <button
                      key={peerName}
                      className="peer-pill"
                      onClick={() => onChoosePeer(peerName)}
                    >
                      {peerName}
                    </button>
                  ))
                ) : (
                  <div className="peer-rail-empty">No bridges available</div>
                )}
              </div>
            </div>
          </div>
        ) : (
          <>
            <button
              className="route-action"
              title="Move back one hop"
              onClick={onRemoveHop}
              disabled={hops.length <= 1}
            >
              ←
            </button>
            <button className="route-action ghost" title="Extend route to a bridge" onClick={onOpenPeerPicker}>
              →
            </button>
          </>
        )}
      </div>
    </div>
  );
};

export default LedgerRouteBar;
