import { JournalPath } from '../types';

/**
 * Encode a JournalPath to a URL-safe string
 */
export const encodePathToHash = (path: JournalPath): string => {
  return encodeURIComponent(JSON.stringify(path));
};

/**
 * Decode a URL hash to a JournalPath
 */
export const decodeHashToPath = (hash: string): JournalPath | null => {
  if (!hash || hash === '#') return null;
  try {
    const decoded = decodeURIComponent(hash.startsWith('#') ? hash.slice(1) : hash);
    if (!decoded) return null;
    const parsed = JSON.parse(decoded);
    if (Array.isArray(parsed)) {
      return parsed as JournalPath;
    }
    return null;
  } catch {
    return null;
  }
};

/**
 * Generate expanded nodes set from a path
 * This ensures all parent nodes are expanded so the selected path is visible
 */
export const generateExpandedNodesFromPath = (path: JournalPath): Set<string> => {
  const expanded = new Set<string>();
  
  // Build up partial paths and add them to expanded set
  for (let i = 1; i <= path.length; i++) {
    const partialPath = path.slice(0, i);
    expanded.add(JSON.stringify(partialPath));
  }
  
  return expanded;
};

/**
 * Extract the "base" path by removing version numbers, for comparison purposes.
 * This helps determine if we're looking at the same document or a different one.
 */
export const getBasePath = (path: JournalPath): string => {
  // Filter out numbers and stringify for comparison
  const filtered = path.filter(segment => Array.isArray(segment));
  return JSON.stringify(filtered);
};

/**
 * Build a path with a specific version offset for a given tab index
 */
export const buildVersionPath = (
  basePath: JournalPath,
  tabIndex: number,
  versionOffset: number
): JournalPath => {
  if (tabIndex === 0) {
    // For the local journal
    if (Array.isArray(basePath[0])) {
      // Path starts with a list (staged) - prepend the version offset
      return [versionOffset, ...basePath];
    }
    // Path already starts with a number - replace it
    return [versionOffset, ...basePath.slice(1)];
  }

  // For peer journals - update the appropriate index in the path
  const modifiedPath = [...basePath];
  let peerCount = 0;

  for (let i = 0; i < modifiedPath.length; i++) {
    if (typeof modifiedPath[i] === 'number') {
      peerCount++;
      if (peerCount === tabIndex + 1) {
        modifiedPath[i] = versionOffset;
        break;
      }
    }
  }

  return modifiedPath;
};

/**
 * Get the version index at a specific tab position in the path
 */
export const getVersionAtTab = (path: JournalPath, tabIndex: number): number | null => {
  if (tabIndex === 0) {
    const firstElement = path[0];
    return typeof firstElement === 'number' ? firstElement : null;
  }

  let peerCount = 0;
  for (const segment of path) {
    if (typeof segment === 'number') {
      peerCount++;
      if (peerCount === tabIndex + 1) {
        return segment;
      }
    }
  }
  return null;
};

/**
 * Build the child path based on the parent node's path and the child item name
 */
export const buildChildPath = (parentPath: JournalPath, itemName: string): JournalPath => {
  const lastSegment = parentPath[parentPath.length - 1];

  if (!Array.isArray(lastSegment)) {
    return parentPath;
  }

  const segmentType = lastSegment[0];

  if (segmentType === '*peer*') {
    if (lastSegment.length === 1) {
      // Listing peers - create peer chain path
      return [...parentPath.slice(0, -1), ['*peer*', itemName, 'chain'], -1];
    }
    if (lastSegment.length === 3) {
      // Already in peer's chain
      return [...parentPath, -1, ['*peer*', itemName, 'chain'], -1];
    }
  }

  if (segmentType === '*state*') {
    // In a state directory - extend the state segment
    return [...parentPath.slice(0, -1), ['*state*', ...lastSegment.slice(1), itemName]];
  }

  return parentPath;
};
