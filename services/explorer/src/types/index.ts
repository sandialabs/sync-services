/**
 * Core types for the Synchronic Web Explorer application
 */

export interface AppState {
  endpoint: string;
  authentication: string;
  rootIndex: number;
  selectedPath: JournalPath | null;
  expandedNodes: Set<string>;
  isLoading: boolean;
  error: string | null;
}

export type JournalPath = Array<number | string[]>;

export interface JournalResponse<T = any> {
  content: T;
  'pinned?': JournalPath | null;
  proof: any;
}

export interface PeerInfo {
  name: string;
  endpoint: string;
}

export interface TreeNode {
  id: string;
  label: string;
  type: 'peer' | 'directory' | 'file';
  path: JournalPath;
  children?: TreeNode[];
  isPinned?: boolean;
  isLocal?: boolean;
}

export interface DocumentContent {
  path: JournalPath;
  content: any;
  isPinned: boolean;
  proof: any;
}

export interface HistoryEntry {
  index: number;
  content: any;
  path: JournalPath;
  timestamp?: string;
}

// Journal API types
export interface JournalRequest {
  function: string;
  arguments?: any[];
  authentication?: string;
}

export interface SchemeString {
  '*type/string*': string;
}

export interface SchemeByteVector {
  '*type/byte-vector*': string;
}

// Union type for Scheme wrapped values
export type SchemeValue = SchemeString | SchemeByteVector;

// Directory response structure
export interface DirectoryResult {
  items: string[];
  isComplete: boolean;
}
