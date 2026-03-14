/**
 * Service for interacting with the Synchronic Web Gateway API
 */

import { 
  JournalResponse, 
  JournalPath, 
  PeerInfo,
  SchemeString,
  DirectoryResult,
  DirectoryEntry,
  DirectoryEntryType
} from '../types';

interface GatewayErrorPayload {
  error?: string;
  message?: string;
  details?: unknown;
  hints?: unknown;
  source?: string;
}

class GatewayRequestError extends Error {
  readonly status: number;
  readonly code: string;
  readonly details?: unknown;
  readonly hints?: unknown;
  readonly source?: string;

  constructor(input: {
    status: number;
    code: string;
    message: string;
    details?: unknown;
    hints?: unknown;
    source?: string;
  }) {
    super(input.message);
    this.name = 'GatewayRequestError';
    this.status = input.status;
    this.code = input.code;
    this.details = input.details;
    this.hints = input.hints;
    this.source = input.source;
  }
}

export class JournalService {
  private endpointBase: string;
  private authentication: string;

  constructor(endpoint: string, authentication: string) {
    this.endpointBase = endpoint.replace(/\/+$/, '');
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
   * Parse directory entries with explicit child-type metadata.
   * Returns null when content is not a directory payload.
   */
  static parseDirectoryEntries(content: any): DirectoryEntry[] | null {
    if (!Array.isArray(content) || content.length < 2) {
      return null;
    }

    const normalizeType = (value: unknown): DirectoryEntryType => {
      if (value === 'directory' || value === 'value' || value === 'unknown') {
        return value;
      }
      return 'unknown';
    };

    if (content[0] === 'directory') {
      if (content[1] && typeof content[1] === 'object' && !Array.isArray(content[1])) {
        return Object.entries(content[1]).map(([name, type]) => ({
          name,
          type: normalizeType(type),
        }));
      }
      if (Array.isArray(content[1])) {
        return content[1].map((item: unknown) => {
          const { value } = JournalService.extractSchemeValue(item);
          return {
            name: typeof value === 'string' ? value : String(value),
            type: 'unknown' as const,
          };
        });
      }
      return null;
    }

    if (Array.isArray(content[0])) {
      return content[0].map((item: unknown) => {
        const { value } = JournalService.extractSchemeValue(item);
        return {
          name: typeof value === 'string' ? value : String(value),
          type: 'unknown' as const,
        };
      });
    }

    return null;
  }

  private buildGatewayUrl(path: string): string {
    const suffix = path.startsWith('/') ? path : `/${path}`;
    return `${this.endpointBase}${suffix}`;
  }

  private parseGatewayError(status: number, payload: unknown): GatewayRequestError {
    const objectPayload =
      payload && typeof payload === 'object' && !Array.isArray(payload)
        ? (payload as GatewayErrorPayload)
        : undefined;
    const code = objectPayload?.error || `http_${status}`;
    const baseMessage =
      objectPayload?.message ||
      (typeof payload === 'string' ? payload : `Gateway request failed (${status})`);
    const message = `${code}: ${baseMessage}`;
    return new GatewayRequestError({
      status,
      code,
      message,
      details: objectPayload?.details,
      hints: objectPayload?.hints,
      source: objectPayload?.source,
    });
  }

  private async request<T = any>(input: {
    method: 'GET' | 'POST';
    path: string;
    requiresAuth?: boolean;
    args?: Record<string, any>;
  }): Promise<T> {
    const { method, path, requiresAuth = true, args } = input;
    const url = this.buildGatewayUrl(path);
    const headers: Record<string, string> = {};
    let body: string | undefined;

    if (method === 'POST') {
      headers['Content-Type'] = 'application/json';
      if (args && Object.keys(args).length > 0) {
        body = JSON.stringify(args);
      }
    }

    if (requiresAuth && this.authentication) {
      headers.Authorization = `Bearer ${this.authentication}`;
    }

    console.log('Gateway Request:', {
      url,
      method,
      arguments: args,
      hasAuth: Boolean(headers.Authorization),
    });

    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 30000);

    try {
      const response = await fetch(url, {
        method,
        headers,
        body,
        signal: controller.signal,
      });

      const raw = await response.text();
      let parsed: unknown = raw;
      if (raw) {
        try {
          parsed = JSON.parse(raw);
        } catch {
          // Keep plain-text payloads as-is.
        }
      }

      clearTimeout(timeoutId);

      if (!response.ok) {
        console.error('Gateway request failed:', {
          status: response.status,
          statusText: response.statusText,
          payload: parsed,
        });
        throw this.parseGatewayError(response.status, parsed);
      }

      console.log('Gateway Response:', { path, result: parsed });
      return parsed as T;
    } catch (error) {
      clearTimeout(timeoutId);
      if (error instanceof Error && error.name === 'AbortError') {
        throw new Error('Request timeout: The gateway did not respond in time');
      }
      throw error;
    }
  }

  /**
   * Get current size of the ledger
   */
  async getSize(): Promise<number> {
    return this.request<number>({
      method: 'GET',
      path: '/general/size',
      requiresAuth: false,
    });
  }

  /**
   * Add a new peer
   */
  async addPeer(name: string, endpoint: string): Promise<boolean> {
    const nameStr: SchemeString = { '*type/string*': name };
    const endpointStr: SchemeString = { '*type/string*': endpoint };
    return this.request<boolean>({
      method: 'POST',
      path: '/general/general-peer',
      args: {
        name: nameStr,
        interface: endpointStr,
      },
    });
  }

  /**
   * Set data at path to the new value
   */
  async set(path: JournalPath, value: any): Promise<boolean> {
    const wrappedValue = typeof value === 'string' 
      ? { '*type/string*': value } 
      : value;
    return this.request<boolean>({
      method: 'POST',
      path: '/general/set',
      args: {
        path,
        value: wrappedValue,
      },
    });
  }

  /**
   * Get the existing value at the path alongside metadata
   */
  async get(path: JournalPath): Promise<JournalResponse> {
    return this.request<JournalResponse>({
      method: 'POST',
      path: '/general/get',
      args: {
        path,
        'details?': true,
      },
    });
  }

  /**
   * Pin the value at the specified path
   */
  async pin(path: JournalPath): Promise<boolean> {
    return this.request<boolean>({
      method: 'POST',
      path: '/general/pin',
      args: { path },
    });
  }

  /**
   * Unpin the value at the specified path
   */
  async unpin(path: JournalPath): Promise<boolean> {
    return this.request<boolean>({
      method: 'POST',
      path: '/general/unpin',
      args: { path },
    });
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
  async getPeers(): Promise<PeerInfo[]> {
    const peers = await this.request<unknown[]>({
      method: 'GET',
      path: '/general/peers',
    });
    if (!Array.isArray(peers)) return [];
    return peers.map((peer) => {
      const { value } = JournalService.extractSchemeValue(peer);
      return { name: typeof value === 'string' ? value : String(value), endpoint: '' };
    });
  }
}
