import { getInitialTheme, getInitialVimMode, applyTheme } from './theme';

describe('theme utilities', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    (window.localStorage.getItem as jest.Mock).mockReturnValue(null);
    // Set up default matchMedia mock that returns false for matches
    (window.matchMedia as jest.Mock).mockImplementation((query: string) => ({
      matches: false,
      media: query,
    }));
  });

  describe('getInitialTheme', () => {
    it('should return stored theme from localStorage', () => {
      (window.localStorage.getItem as jest.Mock).mockReturnValue('dark');
      expect(getInitialTheme()).toBe('dark');
    });

    it('should return light theme from localStorage', () => {
      (window.localStorage.getItem as jest.Mock).mockReturnValue('light');
      expect(getInitialTheme()).toBe('light');
    });

    it('should return dark theme if system prefers dark', () => {
      (window.matchMedia as jest.Mock).mockImplementation((query: string) => ({
        matches: query === '(prefers-color-scheme: dark)',
        media: query,
      }));

      expect(getInitialTheme()).toBe('dark');
    });

    it('should return light theme as default', () => {
      expect(getInitialTheme()).toBe('light');
    });

    it('should ignore invalid stored values', () => {
      (window.localStorage.getItem as jest.Mock).mockReturnValue('invalid');
      expect(getInitialTheme()).toBe('light');
    });
  });

  describe('getInitialVimMode', () => {
    it('should return true when stored as true', () => {
      (window.localStorage.getItem as jest.Mock).mockReturnValue('true');
      expect(getInitialVimMode()).toBe(true);
    });

    it('should return false when stored as false', () => {
      (window.localStorage.getItem as jest.Mock).mockReturnValue('false');
      expect(getInitialVimMode()).toBe(false);
    });

    it('should return false when not stored', () => {
      expect(getInitialVimMode()).toBe(false);
    });

    it('should return false for invalid values', () => {
      (window.localStorage.getItem as jest.Mock).mockReturnValue('invalid');
      expect(getInitialVimMode()).toBe(false);
    });
  });

  describe('applyTheme', () => {
    it('should set data-theme attribute on document', () => {
      const mockSetAttribute = jest.spyOn(document.documentElement, 'setAttribute');

      applyTheme('dark');

      expect(mockSetAttribute).toHaveBeenCalledWith('data-theme', 'dark');
      expect(window.localStorage.setItem).toHaveBeenCalledWith('workbench-theme', 'dark');
    });

    it('should persist theme to localStorage', () => {
      applyTheme('light');

      expect(window.localStorage.setItem).toHaveBeenCalledWith('workbench-theme', 'light');
    });
  });
});
