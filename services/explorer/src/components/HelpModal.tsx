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
              The Synchronic Web Explorer is a UI for exploring synchronic web journals.
              It allows you to navigate, read, and write data across a peer-to-peer network.
            </p>
          </section>

          <section>
            <h3>Getting Started</h3>
            <ol>
              <li>Enter your journal endpoint URL (e.g., http://localhost:8192/interface)</li>
              <li>Enter your authentication password</li>
              <li>Click "Synchronize" to connect and load the latest data</li>
            </ol>
          </section>

          <section>
            <h3>Navigation (Left Pane)</h3>
            <ul>
              <li><strong>state:</strong> Your local journal's data</li>
              <li><strong>peer:</strong> Connected journals in the network</li>
              <li>Click folders to expand/collapse</li>
              <li>Click files to view their content</li>
              <li>Use action buttons to delete, add files, or pin/unpin data</li>
            </ul>
          </section>

          <section>
            <h3>Content (Middle Pane)</h3>
            <ul>
              <li>View selected documents or directories</li>
              <li>Edit local documents by clicking "Edit"</li>
              <li>Save changes with the "Save" button</li>
              <li>View cryptographic proofs in the Verification tab</li>
            </ul>
          </section>

          <section>
            <h3>History (Right Pane)</h3>
            <ul>
              <li>View different versions of documents</li>
              <li>Each tab represents a journal in the path</li>
              <li>Load older versions to see historical data</li>
            </ul>
          </section>

          <section>
            <h3>Peer Management</h3>
            <p>
              Use the "Peers" tab in the left pane to:
            </p>
            <ul>
              <li>View connected peers</li>
              <li>Add new peers by providing their name and endpoint</li>
            </ul>
          </section>
        </div>
      </div>
    </div>
  );
};

export default HelpModal;
