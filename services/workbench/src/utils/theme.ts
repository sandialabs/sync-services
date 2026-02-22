import { Theme } from '../types/workbench';

/**
 * Get initial theme from localStorage or system preference
 */
export const getInitialTheme = (): Theme => {
  const stored = localStorage.getItem('workbench-theme');
  if (stored === 'light' || stored === 'dark') {
    return stored;
  }
  if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
    return 'dark';
  }
  return 'light';
};

/**
 * Get initial vim mode from localStorage
 */
export const getInitialVimMode = (): boolean => {
  const stored = localStorage.getItem('workbench-vim-mode');
  return stored === 'true';
};

/**
 * Apply theme to document and persist to localStorage
 */
export const applyTheme = (theme: Theme): void => {
  document.documentElement.setAttribute('data-theme', theme);
  localStorage.setItem('workbench-theme', theme);
};

/**
 * Detect if user is on Mac for keyboard shortcut display
 */
export const isMac = typeof navigator !== 'undefined' && /Mac|iPod|iPhone|iPad/.test(navigator.platform);
export const modKey = isMac ? '⌘' : 'Ctrl';
