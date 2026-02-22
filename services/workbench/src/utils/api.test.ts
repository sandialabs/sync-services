import { executeQuery } from './api';

// Mock fetch globally
const mockFetch = jest.fn();
global.fetch = mockFetch;

describe('executeQuery', () => {
  beforeEach(() => {
    mockFetch.mockClear();
  });

  it('should execute a successful query', async () => {
    const mockResponse = { result: 42 };
    mockFetch.mockResolvedValueOnce({
      ok: true,
      status: 200,
      statusText: 'OK',
      text: () => Promise.resolve(JSON.stringify(mockResponse)),
    });

    const result = await executeQuery('http://localhost:4096/interface', '(+ 1 2)');

    expect(result.error).toBeUndefined();
    expect(result.result).toEqual(mockResponse);
    expect(result.request).toContain('POST http://localhost:4096/interface');
    expect(result.request).toContain('(+ 1 2)');
    expect(result.response).toContain('HTTP 200 OK');
  });

  it('should handle HTTP errors', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: false,
      status: 500,
      statusText: 'Internal Server Error',
      text: () => Promise.resolve('Server error'),
    });

    const result = await executeQuery('http://localhost:4096/interface', '(invalid)');

    expect(result.error).toBe('Request failed: 500 Internal Server Error');
    expect(result.result).toBeNull();
    expect(result.response).toContain('HTTP 500');
  });

  it('should handle network errors', async () => {
    mockFetch.mockRejectedValueOnce(new Error('Network error'));

    const result = await executeQuery('http://localhost:4096/interface', '(+ 1 2)');

    expect(result.error).toBe('Network error');
    expect(result.result).toBeNull();
    expect(result.response).toContain('Error: Network error');
  });

  it('should handle non-JSON responses', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      status: 200,
      statusText: 'OK',
      text: () => Promise.resolve('plain text response'),
    });

    const result = await executeQuery('http://localhost:4096/interface', '(display "hello")');

    expect(result.error).toBeUndefined();
    expect(result.result).toBe('plain text response');
  });

  it('should trim query whitespace', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      status: 200,
      statusText: 'OK',
      text: () => Promise.resolve('42'),
    });

    await executeQuery('http://localhost:4096/interface', '  (+ 1 2)  \n');

    expect(mockFetch).toHaveBeenCalledWith(
      'http://localhost:4096/interface',
      expect.objectContaining({
        body: '(+ 1 2)',
      })
    );
  });

  it('should send correct headers', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      status: 200,
      statusText: 'OK',
      text: () => Promise.resolve('42'),
    });

    await executeQuery('http://localhost:4096/interface', '(+ 1 2)');

    expect(mockFetch).toHaveBeenCalledWith(
      'http://localhost:4096/interface',
      expect.objectContaining({
        method: 'POST',
        headers: {
          'Content-Type': 'text/plain',
        },
      })
    );
  });

  it('should handle unknown error types', async () => {
    mockFetch.mockRejectedValueOnce('string error');

    const result = await executeQuery('http://localhost:4096/interface', '(+ 1 2)');

    expect(result.error).toBe('Unknown error');
  });
});
