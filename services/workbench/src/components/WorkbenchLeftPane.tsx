import React, { useState, useEffect } from 'react';
import { FunctionEntry, ApiEntry, ExampleEntry, LeftPaneTab } from '../types/workbench';

interface WorkbenchLeftPaneProps {
  onUseExample: (code: string) => void;
}

export const WorkbenchLeftPane: React.FC<WorkbenchLeftPaneProps> = ({ onUseExample }) => {
  const [activeTab, setActiveTab] = useState<LeftPaneTab>('api');

  // Functions state
  const [functions, setFunctions] = useState<FunctionEntry[]>([]);
  const [functionsLoading, setFunctionsLoading] = useState(false);
  const [functionsError, setFunctionsError] = useState<string | null>(null);
  const [functionsSearch, setFunctionsSearch] = useState('');
  const [expandedFunction, setExpandedFunction] = useState<string | null>(null);

  // API state
  const [apiEntries, setApiEntries] = useState<ApiEntry[]>([]);
  const [apiLoading, setApiLoading] = useState(false);
  const [apiError, setApiError] = useState<string | null>(null);
  const [apiSearch, setApiSearch] = useState('');
  const [expandedApi, setExpandedApi] = useState<string | null>(null);
  const [apiFilter, setApiFilter] = useState<'all' | 'any' | 'user' | 'root'>('all');

  // Examples state
  const [examples, setExamples] = useState<ExampleEntry[]>([]);
  const [examplesLoading, setExamplesLoading] = useState(false);
  const [examplesError, setExamplesError] = useState<string | null>(null);
  const [examplesSearch, setExamplesSearch] = useState('');
  const [expandedExample, setExpandedExample] = useState<string | null>(null);

  // Load help-functions.json on mount
  useEffect(() => {
    const loadFunctions = async () => {
      setFunctionsLoading(true);
      setFunctionsError(null);
      try {
        const response = await fetch(process.env.PUBLIC_URL + '/help-functions.json');
        if (!response.ok) {
          throw new Error(`Failed to load functions: ${response.status}`);
        }
        const data = await response.json();
        const entries: FunctionEntry[] = Object.entries(data).map(([name, description]) => ({
          name,
          description: description as string,
        }));
        setFunctions(entries);
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : 'Failed to load functions';
        setFunctionsError(errorMessage);
      } finally {
        setFunctionsLoading(false);
      }
    };

    loadFunctions();
  }, []);

  // Load help-api.json on mount
  useEffect(() => {
    const loadApi = async () => {
      setApiLoading(true);
      setApiError(null);
      try {
        const response = await fetch(process.env.PUBLIC_URL + '/help-api.json');
        if (!response.ok) {
          throw new Error(`Failed to load API: ${response.status}`);
        }
        const data = await response.json();
        const entries: ApiEntry[] = Object.entries(data).map(([name, info]: [string, any]) => ({
          name,
          description: info.description || '',
          template: info.template || '',
          example: info.example || '',
          permission: info.permission || 'user',
        }));
        setApiEntries(entries);
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : 'Failed to load API';
        setApiError(errorMessage);
      } finally {
        setApiLoading(false);
      }
    };

    loadApi();
  }, []);

  // Load help-examples.json on mount
  useEffect(() => {
    const loadExamples = async () => {
      setExamplesLoading(true);
      setExamplesError(null);
      try {
        const response = await fetch(process.env.PUBLIC_URL + '/help-examples.json');
        if (!response.ok) {
          throw new Error(`Failed to load examples: ${response.status}`);
        }
        const data = await response.json();
        const entries: ExampleEntry[] = Object.entries(data).map(([name, info]: [string, any]) => ({
          name,
          description: info.description || '',
          code: info.code || '',
        }));
        setExamples(entries);
      } catch (err) {
        const errorMessage = err instanceof Error ? err.message : 'Failed to load examples';
        setExamplesError(errorMessage);
      } finally {
        setExamplesLoading(false);
      }
    };

    loadExamples();
  }, []);

  const filteredFunctions = functions.filter((fn) => {
    if (!functionsSearch.trim()) return true;
    const search = functionsSearch.toLowerCase();
    return (
      fn.name.toLowerCase().includes(search) ||
      fn.description.toLowerCase().includes(search)
    );
  });

  const filteredApiEntries = apiEntries.filter((entry) => {
    const matchesSearch = !apiSearch.trim() ||
      entry.name.toLowerCase().includes(apiSearch.toLowerCase()) ||
      entry.description.toLowerCase().includes(apiSearch.toLowerCase());
    const matchesFilter = apiFilter === 'all' || entry.permission === apiFilter;
    return matchesSearch && matchesFilter;
  });

  const filteredExamples = examples.filter((example) => {
    if (!examplesSearch.trim()) return true;
    const search = examplesSearch.toLowerCase();
    return (
      example.name.toLowerCase().includes(search) ||
      example.description.toLowerCase().includes(search) ||
      example.code.toLowerCase().includes(search)
    );
  });

  const renderFunctionsContent = () => {
    if (functionsLoading) {
      return (
        <div className="empty-state">
          <span className="loading-spinner" /> Loading functions...
        </div>
      );
    }

    if (functionsError) {
      return <div className="empty-state error-state">Error: {functionsError}</div>;
    }

    if (functions.length === 0) {
      return <div className="empty-state">No functions available</div>;
    }

    return (
      <div className="functions-panel">
        <div className="functions-search">
          <input
            type="text"
            className="input"
            placeholder="Search functions..."
            value={functionsSearch}
            onChange={(e) => setFunctionsSearch(e.target.value)}
          />
        </div>
        <div className="functions-list">
          {filteredFunctions.length === 0 ? (
            <div className="empty-state">No functions match your search</div>
          ) : (
            filteredFunctions.map((fn) => (
              <div key={fn.name} className="function-item">
                <div
                  className="function-header"
                  onClick={() => setExpandedFunction(expandedFunction === fn.name ? null : fn.name)}
                >
                  <span className="function-expand-icon">
                    {expandedFunction === fn.name ? '▼' : '▶'}
                  </span>
                  <span className="function-name">{fn.name}</span>
                </div>
                {expandedFunction === fn.name && (
                  <div className="function-details">
                    <pre className="function-description">{fn.description}</pre>
                  </div>
                )}
              </div>
            ))
          )}
        </div>
      </div>
    );
  };

  const renderApiContent = () => {
    if (apiLoading) {
      return (
        <div className="empty-state">
          <span className="loading-spinner" /> Loading API...
        </div>
      );
    }

    if (apiError) {
      return <div className="empty-state error-state">Error: {apiError}</div>;
    }

    if (apiEntries.length === 0) {
      return <div className="empty-state">No API documentation available</div>;
    }

    return (
      <div className="api-panel">
        <div className="api-search">
          <input
            type="text"
            className="input"
            placeholder="Search API..."
            value={apiSearch}
            onChange={(e) => setApiSearch(e.target.value)}
          />
        </div>
        <div className="api-filters">
          <button
            className={`api-filter-btn ${apiFilter === 'all' ? 'active' : ''}`}
            onClick={() => setApiFilter('all')}
          >
            All
          </button>
          <button
            className={`api-filter-btn ${apiFilter === 'any' ? 'active' : ''}`}
            onClick={() => setApiFilter('any')}
          >
            Anon
          </button>
          <button
            className={`api-filter-btn ${apiFilter === 'user' ? 'active' : ''}`}
            onClick={() => setApiFilter('user')}
          >
            User
          </button>
          <button
            className={`api-filter-btn ${apiFilter === 'root' ? 'active' : ''}`}
            onClick={() => setApiFilter('root')}
          >
            Root
          </button>
        </div>
        <div className="api-list">
          {filteredApiEntries.length === 0 ? (
            <div className="empty-state">No API entries match your search</div>
          ) : (
            filteredApiEntries.map((entry) => (
              <div key={entry.name} className="api-item">
                <div
                  className="api-header"
                  onClick={() => setExpandedApi(expandedApi === entry.name ? null : entry.name)}
                >
                  <span className="api-expand-icon">
                    {expandedApi === entry.name ? '▼' : '▶'}
                  </span>
                  <span className="api-name">{entry.name}</span>
                  <span className={`api-permission api-permission-${entry.permission}`}>
                    {entry.permission}
                  </span>
                </div>
                {expandedApi === entry.name && (
                  <div className="api-details">
                    <div className="api-description">{entry.description}</div>
                    {entry.template && (
                      <div className="api-section">
                        <div className="api-section-title">Template:</div>
                        <pre className="api-code">{entry.template}</pre>
                      </div>
                    )}
                    {entry.example && (
                      <div className="api-section">
                        <div className="api-section-title">Example:</div>
                        <pre className="api-code">{entry.example}</pre>
                        <button
                          className="button button-small"
                          onClick={() => onUseExample(entry.example)}
                        >
                          Use Example
                        </button>
                      </div>
                    )}
                  </div>
                )}
              </div>
            ))
          )}
        </div>
      </div>
    );
  };

  const renderExamplesContent = () => {
    if (examplesLoading) {
      return (
        <div className="empty-state">
          <span className="loading-spinner" /> Loading examples...
        </div>
      );
    }

    if (examplesError) {
      return <div className="empty-state error-state">Error: {examplesError}</div>;
    }

    if (examples.length === 0) {
      return <div className="empty-state">No examples available</div>;
    }

    return (
      <div className="examples-panel">
        <div className="examples-search">
          <input
            type="text"
            className="input"
            placeholder="Search examples..."
            value={examplesSearch}
            onChange={(e) => setExamplesSearch(e.target.value)}
          />
        </div>
        <div className="examples-list">
          {filteredExamples.length === 0 ? (
            <div className="empty-state">No examples match your search</div>
          ) : (
            filteredExamples.map((example) => (
              <div key={example.name} className="example-item">
                <div
                  className="example-header"
                  onClick={() => setExpandedExample(expandedExample === example.name ? null : example.name)}
                >
                  <span className="example-expand-icon">
                    {expandedExample === example.name ? '▼' : '▶'}
                  </span>
                  <span className="example-name">{example.name}</span>
                </div>
                {expandedExample === example.name && (
                  <div className="example-details">
                    <div className="example-description">{example.description}</div>
                    {example.code && (
                      <div className="example-section">
                        <div className="example-section-title">Code:</div>
                        <pre className="example-code">{example.code}</pre>
                        <button
                          className="button button-small"
                          onClick={() => onUseExample(example.code)}
                        >
                          Use Example
                        </button>
                      </div>
                    )}
                  </div>
                )}
              </div>
            ))
          )}
        </div>
      </div>
    );
  };

  const renderContent = () => {
    switch (activeTab) {
      case 'api':
        return renderApiContent();
      case 'functions':
        return renderFunctionsContent();
      case 'examples':
        return renderExamplesContent();
      default:
        return null;
    }
  };

  return (
    <>
      <div className="tabs">
        <button
          className={`tab ${activeTab === 'api' ? 'active' : ''}`}
          onClick={() => setActiveTab('api')}
        >
          API
        </button>
        <button
          className={`tab ${activeTab === 'functions' ? 'active' : ''}`}
          onClick={() => setActiveTab('functions')}
        >
          Functions
        </button>
        <button
          className={`tab ${activeTab === 'examples' ? 'active' : ''}`}
          onClick={() => setActiveTab('examples')}
        >
          Examples
        </button>
      </div>
      <div className="tab-content">{renderContent()}</div>
    </>
  );
};
