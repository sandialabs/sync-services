import React from 'react';
import { modKey } from '../utils/theme';

interface WorkbenchHelpModalProps {
  onClose: () => void;
}

export const WorkbenchHelpModal: React.FC<WorkbenchHelpModalProps> = ({ onClose }) => {
  return (
    <div className="modal-overlay" onClick={onClose}>
      <div className="modal-content" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h2>Synchronic Web Workbench Help</h2>
          <button className="modal-close" onClick={onClose}>
            ×
          </button>
        </div>
        <div className="modal-body">
          <section>
            <h3>Overview</h3>
            <p>
              The Synchronic Web Workbench is a developer interface for querying synchronic web
              journals. It provides a structured interface to interact programmatically with the
              journal, from sending simple queries to authoring core operating software.
            </p>
          </section>

          <section>
            <h3>Top Pane (Input)</h3>
            <ul>
              <li>Write your Scheme queries in the editor</li>
              <li>Create multiple tabs to work on different queries simultaneously</li>
              <li>Click "Run" or press {modKey}+Enter to execute the current query</li>
              <li>Use the ✨ button to prettify/format your Scheme code</li>
              <li>Use the copy button to copy your query to clipboard</li>
              <li>Use the download button to save your query to a file</li>
            </ul>
          </section>

          <section>
            <h3>Bottom Pane (Output)</h3>
            <ul>
              <li><strong>Query:</strong> Shows the input query that was executed</li>
              <li><strong>Result:</strong> Shows the output result (default view)</li>
              <li><strong>Request:</strong> Shows the raw HTTP request</li>
              <li><strong>Response:</strong> Shows the raw HTTP response</li>
            </ul>
          </section>

          <section>
            <h3>Left Pane (Reference)</h3>
            <ul>
              <li><strong>API:</strong> Browse journal API functions with templates and examples</li>
              <li><strong>Functions:</strong> Browse and search available Scheme functions</li>
              <li><strong>Examples:</strong> Browse example queries with descriptions</li>
            </ul>
          </section>

          <section>
            <h3>Right Pane (History)</h3>
            <ul>
              <li>Shows a running history of all executed queries</li>
              <li>Click on an entry to view its details in the bottom pane</li>
              <li>Most recent queries appear at the top</li>
            </ul>
          </section>

          <section>
            <h3>Editor Settings</h3>
            <ul>
              <li><strong>Vim Mode:</strong> Click the "vim" button in the toolbar to toggle Vim keybindings</li>
              <li><strong>Theme:</strong> Click the 🌙/☀️ button to toggle between light and dark mode</li>
            </ul>
          </section>

          <section>
            <h3>Keyboard Shortcuts</h3>
            <ul>
              <li><strong>{modKey}+Enter:</strong> Run current query</li>
            </ul>
          </section>
        </div>
      </div>
    </div>
  );
};
