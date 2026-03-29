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

  private static isIndexedPath(path: JournalPath): boolean {
    return typeof path[0] === 'number';
  }

  private static extractBridgeNames(config: unknown): string[] {
    if (!config || typeof config !== 'object' || Array.isArray(config)) {
      return [];
    }

    const privateBlock = (config as Record<string, unknown>).private;
    if (!privateBlock || typeof privateBlock !== 'object' || Array.isArray(privateBlock)) {
      return [];
    }

    const bridgeBlock = (privateBlock as Record<string, unknown>).bridge;
    if (!bridgeBlock || typeof bridgeBlock !== 'object' || Array.isArray(bridgeBlock)) {
      return [];
    }

    return Object.keys(bridgeBlock).sort();
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
   * Add a new bridge
   */
  async addBridge(name: string, endpoint: string): Promise<boolean> {
    const endpointStr: SchemeString = { '*type/string*': endpoint };
    return this.request<boolean>({
      method: 'POST',
      path: '/general/bridge',
      args: {
        name,
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
  async get(
    path: JournalPath,
    options: { pinned?: boolean; proof?: boolean } = {},
  ): Promise<JournalResponse> {
    const { pinned = true, proof = true } = options;
    const indexedPath = JournalService.isIndexedPath(path);
    return this.request<JournalResponse>({
      method: 'POST',
      path: indexedPath ? '/general/resolve' : '/general/get',
      args: indexedPath
        ? {
            path,
            'pinned?': pinned,
            'proof?': proof,
          }
        : {
            path,
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

  private requireStatePath(path: JournalPath): string[] {
    const last = path[path.length - 1];
    if (!Array.isArray(last) || last[0] !== '*state*') {
      throw new Error('Expected a state path');
    }
    return last;
  }

  private buildStateChildPath(parentPath: JournalPath, childName: string): JournalPath {
    const block = this.requireStatePath(parentPath);
    return [
      ...parentPath.slice(0, -1),
      ['*state*', ...block.slice(1), childName],
    ];
  }

  private buildStateSiblingPath(path: JournalPath, siblingName: string): JournalPath {
    const block = this.requireStatePath(path);
    return [
      ...path.slice(0, -1),
      ['*state*', ...block.slice(1, -1), siblingName],
    ];
  }

  private buildDirectoryMarkerPath(path: JournalPath): JournalPath {
    const block = this.requireStatePath(path);
    return [
      ...path.slice(0, -1),
      ['*state*', ...block.slice(1), '*directory*'],
    ];
  }

  async getDirectoryEntries(path: JournalPath): Promise<DirectoryEntry[]> {
    const response = await this.get(path);
    return JournalService.parseDirectoryEntries(response.content) ?? [];
  }

  async createFile(parentPath: JournalPath, fileName: string): Promise<boolean> {
    return this.set(this.buildStateChildPath(parentPath, fileName), { '*type/string*': '' });
  }

  async createDirectory(parentPath: JournalPath, directoryName: string): Promise<boolean> {
    const dirPath = this.buildStateChildPath(parentPath, directoryName);
    return this.set(this.buildDirectoryMarkerPath(dirPath), true);
  }

  async uploadFile(parentPath: JournalPath, file: File): Promise<boolean> {
    const bytes = new Uint8Array(await file.arrayBuffer());
    const hex = Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('');
    return this.set(this.buildStateChildPath(parentPath, file.name), {
      '*type/byte-vector*': hex,
    });
  }

  async renameStagePath(path: JournalPath, nextName: string): Promise<boolean> {
    const targetPath = this.buildStateSiblingPath(path, nextName);
    await this.copyStagePath(path, targetPath);
    await this.deleteStagePath(path);
    return true;
  }

  async deleteStagePath(path: JournalPath): Promise<boolean> {
    const response = await this.get(path);
    const directoryEntries = JournalService.parseDirectoryEntries(response.content);
    if (directoryEntries) {
      for (const entry of directoryEntries.filter((item) => item.name !== '*directory*')) {
        const childPath = this.buildStateChildPath(path, entry.name);
        await this.deleteStagePath(childPath);
      }
      await this.delete(this.buildDirectoryMarkerPath(path));
      return true;
    }

    return this.delete(path);
  }

  async download(path: JournalPath): Promise<{ blob: Blob; filename: string }> {
    const response = await this.get(path);
    const stateBlock = this.requireStatePath(path);
    const fallbackName = stateBlock[stateBlock.length - 1] || 'download';

    if (
      response.content &&
      typeof response.content === 'object' &&
      !Array.isArray(response.content) &&
      '*type/byte-vector*' in response.content
    ) {
      const hex = String(response.content['*type/byte-vector*']);
      const bytes = new Uint8Array(
        hex.match(/.{1,2}/g)?.map((chunk) => Number.parseInt(chunk, 16)) ?? [],
      );
      return {
        blob: new Blob([bytes]),
        filename: fallbackName,
      };
    }

    const { value } = JournalService.extractSchemeValue(response.content);
    const serialized =
      typeof value === 'string' ? value : JSON.stringify(value, null, 2);
    return {
      blob: new Blob([serialized], { type: 'text/plain;charset=utf-8' }),
      filename: fallbackName,
    };
  }

  private async copyStagePath(sourcePath: JournalPath, targetPath: JournalPath): Promise<void> {
    const response = await this.get(sourcePath);
    const directoryEntries = JournalService.parseDirectoryEntries(response.content);

    if (directoryEntries) {
      await this.set(this.buildDirectoryMarkerPath(targetPath), true);
      for (const entry of directoryEntries.filter((item) => item.name !== '*directory*')) {
        await this.copyStagePath(
          this.buildStateChildPath(sourcePath, entry.name),
          this.buildStateChildPath(targetPath, entry.name),
        );
      }
      return;
    }

    await this.set(targetPath, response.content);
  }

  /**
   * Get bridge info
   */
  async getBridges(): Promise<PeerInfo[]> {
    const config = await this.request<unknown>({
      method: 'POST',
      path: '/general/config',
      args: {
        path: ['private', 'bridge'],
      },
    });
    return JournalService.extractBridgeNames(config).map((name) => ({
      name,
      endpoint: '',
    }));
  }

  async addPeer(name: string, endpoint: string): Promise<boolean> {
    return this.addBridge(name, endpoint);
  }

  async getPeers(): Promise<PeerInfo[]> {
    return this.getBridges();
  }
}
