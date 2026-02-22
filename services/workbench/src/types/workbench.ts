/**
 * Query tab state
 */
export interface QueryTab {
  id: string;
  name: string;
  content: string;
}

/**
 * History entry for executed queries
 */
export interface HistoryEntry {
  id: string;
  timestamp: Date;
  query: string;
  request: string;
  response: string;
  result: any;
  error?: string;
}

/**
 * Function entry from help-functions.json
 */
export interface FunctionEntry {
  name: string;
  description: string;
}

/**
 * API entry from help-api.json
 */
export interface ApiEntry {
  name: string;
  description: string;
  template: string;
  example: string;
  permission: 'any' | 'user' | 'root';
}

/**
 * Example entry from help-examples.json
 */
export interface ExampleEntry {
  name: string;
  description: string;
  code: string;
}

/**
 * Bottom pane tab types
 */
export type BottomPaneTab = 'query' | 'result' | 'request' | 'response';

/**
 * Left pane tab types
 */
export type LeftPaneTab = 'api' | 'functions' | 'examples';

/**
 * Theme type
 */
export type Theme = 'light' | 'dark';
