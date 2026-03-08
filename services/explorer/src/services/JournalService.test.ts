import { JournalService } from './JournalService';

describe('JournalService', () => {
  describe('extractSchemeValue', () => {
    it('should extract string type values', () => {
      const input = { '*type/string*': 'hello world' };
      const result = JournalService.extractSchemeValue(input);
      expect(result).toEqual({ value: 'hello world', schemeType: 'string' });
    });

    it('should extract byte-vector type values', () => {
      const input = { '*type/byte-vector*': 'base64data' };
      const result = JournalService.extractSchemeValue(input);
      expect(result).toEqual({ value: 'base64data', schemeType: 'byte-vector' });
    });

    it('should return original value for non-scheme objects', () => {
      const input = { foo: 'bar' };
      const result = JournalService.extractSchemeValue(input);
      expect(result).toEqual({ value: { foo: 'bar' }, schemeType: null });
    });

    it('should return original value for arrays', () => {
      const input = ['directory', ['a', 'b']];
      const result = JournalService.extractSchemeValue(input);
      expect(result).toEqual({ value: ['directory', ['a', 'b']], schemeType: null });
    });

    it('should return original value for primitives', () => {
      expect(JournalService.extractSchemeValue('string')).toEqual({ value: 'string', schemeType: null });
      expect(JournalService.extractSchemeValue(123)).toEqual({ value: 123, schemeType: null });
      expect(JournalService.extractSchemeValue(true)).toEqual({ value: true, schemeType: null });
      expect(JournalService.extractSchemeValue(null)).toEqual({ value: null, schemeType: null });
    });
  });

  describe('parseDirectoryResponse', () => {
    it('should parse complete directory response in object-map format', () => {
      const input = ['directory', { file1: 'value', file2: 'directory', file3: 'unknown' }, true];
      const result = JournalService.parseDirectoryResponse(input);
      expect(result).toEqual({
        items: ['file1', 'file2', 'file3'],
        isComplete: true,
      });
    });

    it('should parse incomplete directory response in object-map format', () => {
      const input = ['directory', { file1: 'value', file2: 'directory' }, false];
      const result = JournalService.parseDirectoryResponse(input);
      expect(result).toEqual({
        items: ['file1', 'file2'],
        isComplete: false,
      });
    });

    it('should parse directory response without completeness flag in object-map format', () => {
      const input = ['directory', { file1: 'value' }];
      const result = JournalService.parseDirectoryResponse(input);
      expect(result).toEqual({
        items: ['file1'],
        isComplete: true,
      });
    });

    it('should parse legacy directory response in array format for backward compatibility', () => {
      const input = ['directory', ['file1', 'file2'], true];
      const result = JournalService.parseDirectoryResponse(input);
      expect(result).toEqual({
        items: ['file1', 'file2'],
        isComplete: true,
      });
    });

    it('should return null for non-directory content', () => {
      expect(JournalService.parseDirectoryResponse({ '*type/string*': 'hello' })).toBeNull();
      expect(JournalService.parseDirectoryResponse(['nothing'])).toBeNull();
      expect(JournalService.parseDirectoryResponse('string')).toBeNull();
      expect(JournalService.parseDirectoryResponse(null)).toBeNull();
    });

    it('should return null for malformed directory response', () => {
      expect(JournalService.parseDirectoryResponse(['directory'])).toBeNull();
      expect(JournalService.parseDirectoryResponse(['directory', 'not-an-object'])).toBeNull();
    });
  });

  describe('parseDirectoryEntries', () => {
    it('should parse entry types from object-map directory payload', () => {
      const input = ['directory', { folder: 'directory', doc: 'value', mystery: 'unknown' }, true];
      const result = JournalService.parseDirectoryEntries(input);
      expect(result).toEqual([
        { name: 'folder', type: 'directory' },
        { name: 'doc', type: 'value' },
        { name: 'mystery', type: 'unknown' },
      ]);
    });

    it('should treat legacy array directory payload as unknown entry types', () => {
      const input = ['directory', [{ '*type/string*': 'a' }, 'b'], true];
      const result = JournalService.parseDirectoryEntries(input);
      expect(result).toEqual([
        { name: 'a', type: 'unknown' },
        { name: 'b', type: 'unknown' },
      ]);
    });

    it('should return null for non-directory content', () => {
      expect(JournalService.parseDirectoryEntries({ '*type/string*': 'hello' })).toBeNull();
    });
  });
});

describe('JournalService API', () => {
  let service: JournalService;
  let mockFetch: jest.Mock;
  const mockJsonResponse = (payload: unknown, ok = true, status = 200, statusText = 'OK') => ({
    ok,
    status,
    statusText,
    text: () => Promise.resolve(JSON.stringify(payload)),
  });
  const mockTextResponse = (payload: string, ok = true, status = 200, statusText = 'OK') => ({
    ok,
    status,
    statusText,
    text: () => Promise.resolve(payload),
  });

  beforeEach(() => {
    service = new JournalService('http://test-endpoint.com/api/v1', 'test-password');
    mockFetch = jest.fn();
    global.fetch = mockFetch;
  });

  afterEach(() => {
    jest.resetAllMocks();
  });

  describe('getSize', () => {
    it('should call size endpoint and return result', async () => {
      mockFetch.mockResolvedValueOnce(mockTextResponse('42'));

      const result = await service.getSize();

      expect(result).toBe(42);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/size',
        expect.objectContaining({
          method: 'GET',
          headers: {},
        })
      );
    });
  });

  describe('get', () => {
    it('should call get endpoint with path and include-proof flag', async () => {
      const mockResponse = {
        content: { '*type/string*': 'test content' },
        'pinned?': null,
        proof: {},
      };
      mockFetch.mockResolvedValueOnce(mockJsonResponse(mockResponse));

      const path = [['*state*', 'test']];
      const result = await service.get(path);

      expect(result).toEqual(mockResponse);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/get',
        expect.objectContaining({
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: 'Bearer test-password',
          },
          body: JSON.stringify({
            arguments: { path, 'details?': true },
          }),
        })
      );
    });
  });

  describe('set', () => {
    it('should call set endpoint with path and string value', async () => {
      mockFetch.mockResolvedValueOnce(mockTextResponse('true'));

      const path = [['*state*', 'test']];
      const result = await service.set(path, 'test value');

      expect(result).toBe(true);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/set',
        expect.objectContaining({
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: 'Bearer test-password',
          },
          body: JSON.stringify({
            arguments: { path, value: { '*type/string*': 'test value' } },
          }),
        })
      );
    });

    it('should call set endpoint with non-string value as-is', async () => {
      mockFetch.mockResolvedValueOnce(mockTextResponse('true'));

      const path = [['*state*', 'test']];
      const result = await service.set(path, true);

      expect(result).toBe(true);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/set',
        expect.objectContaining({
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: 'Bearer test-password',
          },
          body: JSON.stringify({
            arguments: { path, value: true },
          }),
        })
      );
    });
  });

  describe('pin', () => {
    it('should call pin endpoint with path', async () => {
      mockFetch.mockResolvedValueOnce(mockTextResponse('true'));

      const path = [-1, ['*state*', 'test']];
      const result = await service.pin(path);

      expect(result).toBe(true);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/pin',
        expect.objectContaining({
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: 'Bearer test-password',
          },
          body: JSON.stringify({
            arguments: { path },
          }),
        })
      );
    });
  });

  describe('unpin', () => {
    it('should call unpin endpoint with path', async () => {
      mockFetch.mockResolvedValueOnce(mockTextResponse('true'));

      const path = [-1, ['*state*', 'test']];
      const result = await service.unpin(path);

      expect(result).toBe(true);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/unpin',
        expect.objectContaining({
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: 'Bearer test-password',
          },
          body: JSON.stringify({
            arguments: { path },
          }),
        })
      );
    });
  });

  describe('delete', () => {
    it('should call set with nothing value', async () => {
      mockFetch.mockResolvedValueOnce(mockTextResponse('true'));

      const path = [['*state*', 'test']];
      const result = await service.delete(path);

      expect(result).toBe(true);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/set',
        expect.objectContaining({
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: 'Bearer test-password',
          },
          body: JSON.stringify({
            arguments: { path, value: ['nothing'] },
          }),
        })
      );
    });
  });

  describe('addPeer', () => {
    it('should call general-peer! with name and endpoint', async () => {
      mockFetch.mockResolvedValueOnce(mockTextResponse('true'));

      const result = await service.addPeer('peer-name', 'http://peer-endpoint.com');

      expect(result).toBe(true);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/general-peer',
        expect.objectContaining({
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: 'Bearer test-password',
          },
          body: JSON.stringify({
            arguments: {
              name: { '*type/string*': 'peer-name' },
              interface: { '*type/string*': 'http://peer-endpoint.com' },
            },
          }),
        })
      );
    });
  });

  describe('getPeers', () => {
    it('should call peers endpoint and normalize names', async () => {
      mockFetch.mockResolvedValueOnce(
        mockJsonResponse([{ '*type/string*': 'alice' }, 'bob'])
      );
      const result = await service.getPeers();
      expect(result).toEqual([
        { name: 'alice', endpoint: '' },
        { name: 'bob', endpoint: '' },
      ]);
      expect(mockFetch).toHaveBeenCalledWith(
        'http://test-endpoint.com/api/v1/general/peers',
        expect.objectContaining({
          method: 'GET',
          headers: {
            Authorization: 'Bearer test-password',
          },
        })
      );
    });
  });

  describe('error handling', () => {
    it('should surface gateway error message on non-ok response', async () => {
      mockFetch.mockResolvedValueOnce(
        mockJsonResponse(
          {
            error: 'authentication-error',
            message: 'Could not authenticate restricted interface call',
          },
          false,
          401,
          'Unauthorized'
        )
      );
      await expect(service.getPeers()).rejects.toThrow(
        'authentication-error: Could not authenticate restricted interface call'
      );
    });

    it('should throw error on network failure', async () => {
      mockFetch.mockRejectedValueOnce(new Error('Network error'));

      await expect(service.getSize()).rejects.toThrow('Network error');
    });
  });
});
