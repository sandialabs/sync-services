import React from 'react';
import './HelpModal.css';

interface HelpModalProps {
  onClose: () => void;
}

const HelpModal: React.FC<HelpModalProps> = ({ onClose }) => {
  return (
    <div className="modal-overlay" onClick={onClose}>
      <div className="modal-content" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h2>Synchronic Web Explorer Help</h2>
          <button className="modal-close" onClick={onClose}>×</button>
        </div>
        <div className="modal-body">
          <section>
            <h3>Overview</h3>
            <p>
              The Synchronic Web Explorer has two modes. Stage is for local editable state.
              Ledger is for committed, route-based browsing across bridges and snapshots.
            </p>
          </section>

          <section>
            <h3>Getting Started</h3>
            <ol>
              <li>Enter your authentication password.</li>
              <li>Use Stage to browse and edit local files and folders.</li>
              <li>Use Ledger to synchronize and browse committed state.</li>
            </ol>
          </section>

          <section>
            <h3>Stage Mode</h3>
            <ul>
              <li>The left tree shows local staged files and folders.</li>
              <li>Tree rows provide rename and delete actions.</li>
              <li>Selecting a directory shows its contents and file/folder creation actions.</li>
              <li>Selecting a file shows a read-only view until you press Edit.</li>
            </ul>
          </section>

          <section>
            <h3>Ledger Mode</h3>
            <ul>
              <li>The route strip defines the current committed view.</li>
              <li>The first hop is the local journal and additional hops extend through bridges.</li>
              <li>Each hop accepts <code>latest</code> or a negative snapshot index.</li>
              <li>The tree below the route strip shows the state at the current route tip.</li>
            </ul>
          </section>

          <section>
            <h3>Content Pane</h3>
            <ul>
              <li>Directories are shown as a simple contents view.</li>
              <li>Files can be viewed as content in both modes.</li>
              <li>In Ledger, the content header toggles between content and proof for the current file.</li>
              <li>Pinning is available from the ledger file header.</li>
            </ul>
          </section>
        </div>
      </div>
    </div>
  );
};

export default HelpModal;
