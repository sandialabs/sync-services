/**
 * Service for interacting with the Synchronic Web Journal API
 */

import { 
  JournalRequest, 
  JournalResponse, 
  JournalPath, 
  PeerInfo,
  SchemeString,
  DirectoryResult
} from '../types';

// Functions that don't require authentication
const PUBLIC_FUNCTIONS = ['size', 'synchronize', 'resolve', 'information'];

export class JournalService {
  private endpoint: string;
  private authentication: string;

  constructor(endpoint: string, authentication: string) {
    this.endpoint = endpoint;
    this.authentication = authentication;
  }

  /**
   * Extract the actual value from Scheme type wrappers
   * Returns the unwrapped value and the type name if it was wrapped
   */
  static extractSchemeValue(value: any): { value: any; schemeType: string | null } {
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      if ('*type/string*' in value) {
        return { value: value['*type/string*'], schemeType: 'string' };
      }
      if ('*type/byte-vector*' in value) {
        return { value: value['*type/byte-vector*'], schemeType: 'byte-vector' };
      }
    }
    return { value, schemeType: null };
  }

  /**
   * Parse a directory response into a normalized structure
   * Returns null if the content is not a directory
   */
  static parseDirectoryResponse(content: any): DirectoryResult | null {
    if (!Array.isArray(content) || content.length < 2) {
      return null;
    }

    let items: any[] = [];
    let isComplete = true;

    if (content[0] === 'directory') {
      // Current authoritative format: ["directory", { "name": "type" }, true]
      if (content[1] && typeof content[1] === 'object' && !Array.isArray(content[1])) {
        items = Object.keys(content[1]);
        isComplete = content[2] !== false;
      } else if (Array.isArray(content[1])) {
        // Backward compatibility for older format: ["directory", ["a", "b"], true]
        items = content[1];
        isComplete = content[2] !== false;
      } else {
        return null;
      }
    } else if (Array.isArray(content[0])) {
      // Format: [[items...], isComplete]
      items = content[0];
      isComplete = content[1] !== false;
    } else {
      return null;
    }

    // Extract string values from Scheme string objects
    const normalizedItems = items.map(item => {
      const { value } = JournalService.extractSchemeValue(item);
      return typeof value === 'string' ? value : String(value);
    });

    return { items: normalizedItems, isComplete };
  }

  /**
   * Make a JSON POST request to the journal
   */
  private async request<T = any>(func: string, args?: any[]): Promise<T> {
    const body: JournalRequest = { function: func };

    if (args && args.length > 0) {
      body.arguments = args;
    }

    if (this.authentication && !PUBLIC_FUNCTIONS.includes(func)) {
      body.authentication = this.authentication;
    }

    console.log('Journal Request:', {
      url: this.endpoint,
      function: func,
      arguments: args,
      hasAuth: !!body.authentication,
    });
    console.log('JSON Body:', JSON.stringify(body, null, 2));

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 30000);

    try {
      const response = await fetch(this.endpoint, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        console.error('Journal request failed:', {
          status: response.status,
          statusText: response.statusText,
        });
        throw new Error(`Journal request failed: ${response.statusText}`);
      }

      const result = await response.json();
      console.log('Journal Response:', { function: func, result });
      return result;
    } catch (error) {
      clearTimeout(timeoutId);
      if (error instanceof Error && error.name === 'AbortError') {
        throw new Error('Request timeout: The journal service did not respond in time');
      }
      throw error;
    }
  }

  /**
   * Get current size of the ledger
   */
  async getSize(): Promise<number> {
    return this.request<number>('size');
  }

  /**
   * Add a new peer
   */
  async addPeer(name: string, endpoint: string): Promise<boolean> {
    const nameStr: SchemeString = { '*type/string*': name };
    const endpointStr: SchemeString = { '*type/string*': endpoint };
    return this.request<boolean>('general-peer!', [nameStr, endpointStr]);
  }

  /**
   * Set data at path to the new value
   */
  async set(path: JournalPath, value: any): Promise<boolean> {
    const wrappedValue = typeof value === 'string' 
      ? { '*type/string*': value } 
      : value;
    return this.request<boolean>('set!', [path, wrappedValue]);
  }

  /**
   * Get the existing value at the path alongside metadata
   */
  async get(path: JournalPath): Promise<JournalResponse> {
    return this.request<JournalResponse>('get', [path, true]);
  }

  /**
   * Pin the value at the specified path
   */
  async pin(path: JournalPath): Promise<boolean> {
    return this.request<boolean>('pin!', [path]);
  }

  /**
   * Unpin the value at the specified path
   */
  async unpin(path: JournalPath): Promise<boolean> {
    return this.request<boolean>('unpin!', [path]);
  }

  /**
   * Delete a document by setting it to ["nothing"]
   */
  async delete(path: JournalPath): Promise<boolean> {
    return this.set(path, ['nothing']);
  }

  /**
   * Get peer information
   */
  async getPeers(path: JournalPath): Promise<PeerInfo[]> {
    const peerPath = [...path, ['*peer*']];
    const response = await this.get(peerPath);
    
    const directory = JournalService.parseDirectoryResponse(response.content);
    if (!directory) {
      return [];
    }

    return directory.items.map(name => ({ name, endpoint: '' }));
  }
}
