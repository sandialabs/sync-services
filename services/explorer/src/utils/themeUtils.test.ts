import { getInitialTheme } from './themeUtils';

describe('getInitialTheme', () => {
  const originalLocalStorage = global.localStorage;
  const originalMatchMedia = global.matchMedia;

  beforeEach(() => {
    // Mock localStorage
    const localStorageMock = {
      getItem: jest.fn(),
      setItem: jest.fn(),
      removeItem: jest.fn(),
      clear: jest.fn(),
      length: 0,
      key: jest.fn(),
    };
    Object.defineProperty(global, 'localStorage', { value: localStorageMock });
  });

  afterEach(() => {
    Object.defineProperty(global, 'localStorage', { value: originalLocalStorage });
    Object.defineProperty(global, 'matchMedia', { value: originalMatchMedia });
  });

  it('should return stored theme from localStorage', () => {
    (localStorage.getItem as jest.Mock).mockReturnValue('dark');
    expect(getInitialTheme()).toBe('dark');
  });

  it('should return light if stored in localStorage', () => {
    (localStorage.getItem as jest.Mock).mockReturnValue('light');
    expect(getInitialTheme()).toBe('light');
  });

  it('should return dark if system prefers dark mode', () => {
    (localStorage.getItem as jest.Mock).mockReturnValue(null);
    Object.defineProperty(global, 'matchMedia', {
      value: jest.fn().mockReturnValue({ matches: true }),
    });
    expect(getInitialTheme()).toBe('dark');
  });

  it('should return light if system prefers light mode', () => {
    (localStorage.getItem as jest.Mock).mockReturnValue(null);
    Object.defineProperty(global, 'matchMedia', {
      value: jest.fn().mockReturnValue({ matches: false }),
    });
    expect(getInitialTheme()).toBe('light');
  });

  it('should return light if matchMedia is not available', () => {
    (localStorage.getItem as jest.Mock).mockReturnValue(null);
    Object.defineProperty(global, 'matchMedia', { value: undefined });
    expect(getInitialTheme()).toBe('light');
  });
});
