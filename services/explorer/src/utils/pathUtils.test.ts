import {
  encodePathToHash,
  decodeHashToPath,
  generateExpandedNodesFromPath,
  getBasePath,
  buildVersionPath,
  getVersionAtTab,
} from './pathUtils';

describe('encodePathToHash', () => {
  it('should encode a simple path', () => {
    const path = [['*state*', 'test']];
    const result = encodePathToHash(path);
    expect(result).toBe(encodeURIComponent(JSON.stringify(path)));
  });

  it('should encode a path with version number', () => {
    const path = [-1, ['*state*', 'test']];
    const result = encodePathToHash(path);
    expect(result).toBe(encodeURIComponent(JSON.stringify(path)));
  });

  it('should encode a complex peer path', () => {
    const path = [-1, ['*peer*', 'alice', 'chain'], -1, ['*state*', 'data']];
    const result = encodePathToHash(path);
    expect(result).toBe(encodeURIComponent(JSON.stringify(path)));
  });
});

describe('decodeHashToPath', () => {
  it('should decode a valid hash', () => {
    const path = [['*state*', 'test']];
    const hash = '#' + encodeURIComponent(JSON.stringify(path));
    const result = decodeHashToPath(hash);
    expect(result).toEqual(path);
  });

  it('should decode hash without # prefix', () => {
    const path = [['*state*', 'test']];
    const hash = encodeURIComponent(JSON.stringify(path));
    const result = decodeHashToPath(hash);
    expect(result).toEqual(path);
  });

  it('should return null for empty hash', () => {
    expect(decodeHashToPath('')).toBeNull();
    expect(decodeHashToPath('#')).toBeNull();
  });

  it('should return null for invalid JSON', () => {
    expect(decodeHashToPath('#invalid-json')).toBeNull();
  });

  it('should return null for non-array JSON', () => {
    expect(decodeHashToPath('#' + encodeURIComponent('{"foo": "bar"}'))).toBeNull();
  });
});

describe('generateExpandedNodesFromPath', () => {
  it('should generate expanded nodes for a simple path', () => {
    const path = [['*state*', 'dir1', 'file1']];
    const result = generateExpandedNodesFromPath(path);
    expect(result.size).toBe(1);
    expect(result.has(JSON.stringify([['*state*', 'dir1', 'file1']]))).toBe(true);
  });

  it('should generate expanded nodes for a path with version', () => {
    const path = [-1, ['*state*', 'test']];
    const result = generateExpandedNodesFromPath(path);
    expect(result.size).toBe(2);
    expect(result.has(JSON.stringify([-1]))).toBe(true);
    expect(result.has(JSON.stringify([-1, ['*state*', 'test']]))).toBe(true);
  });

  it('should generate expanded nodes for a peer path', () => {
    const path = [-1, ['*peer*', 'alice', 'chain'], -1, ['*state*']];
    const result = generateExpandedNodesFromPath(path);
    expect(result.size).toBe(4);
  });
});

describe('getBasePath', () => {
  it('should extract base path from staged path', () => {
    const path = [['*state*', 'test']];
    const result = getBasePath(path);
    expect(result).toBe(JSON.stringify([['*state*', 'test']]));
  });

  it('should extract base path from versioned path', () => {
    const path = [-1, ['*state*', 'test']];
    const result = getBasePath(path);
    expect(result).toBe(JSON.stringify([['*state*', 'test']]));
  });

  it('should extract base path from peer path', () => {
    const path = [-1, ['*peer*', 'alice', 'chain'], -2, ['*state*', 'data']];
    const result = getBasePath(path);
    expect(result).toBe(JSON.stringify([['*peer*', 'alice', 'chain'], ['*state*', 'data']]));
  });
});

describe('buildVersionPath', () => {
  it('should prepend version to staged path for tab 0', () => {
    const path = [['*state*', 'test']];
    const result = buildVersionPath(path, 0, -1);
    expect(result).toEqual([-1, ['*state*', 'test']]);
  });

  it('should replace version in versioned path for tab 0', () => {
    const path = [-1, ['*state*', 'test']];
    const result = buildVersionPath(path, 0, -2);
    expect(result).toEqual([-2, ['*state*', 'test']]);
  });

  it('should update peer version for tab > 0', () => {
    const path = [-1, ['*peer*', 'alice', 'chain'], -1, ['*state*', 'data']];
    const result = buildVersionPath(path, 1, -4);
    expect(result).toEqual([-1, ['*peer*', 'alice', 'chain'], -4, ['*state*', 'data']]);
  });
});

describe('getVersionAtTab', () => {
  it('should return version for tab 0 with versioned path', () => {
    const path = [-1, ['*state*', 'test']];
    const result = getVersionAtTab(path, 0);
    expect(result).toBe(-1);
  });

  it('should return null for tab 0 with staged path', () => {
    const path = [['*state*', 'test']];
    const result = getVersionAtTab(path, 0);
    expect(result).toBeNull();
  });

  it('should return peer version for tab > 0', () => {
    const path = [-1, ['*peer*', 'alice', 'chain'], -4, ['*state*', 'data']];
    const result = getVersionAtTab(path, 1);
    expect(result).toBe(-4);
  });

  it('should return null if tab index exceeds peer count', () => {
    const path = [-1, ['*state*', 'test']];
    const result = getVersionAtTab(path, 1);
    expect(result).toBeNull();
  });
});
