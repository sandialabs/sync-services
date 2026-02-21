import React, { useEffect, useState } from 'react';
import { AppState, JournalPath, TreeNode } from '../types';
import { JournalService } from '../services/JournalService';

interface NavigationTabProps {
  appState: AppState;
  journalService: JournalService | null;
  onPathSelect: (path: JournalPath) => void;
  onExpandedNodesChange: (expandedNodes: Set<string>) => void;
}

/**
 * Build the child path based on the parent node's path and the child item name
 */
const buildChildPath = (parentPath: JournalPath, itemName: string): JournalPath => {
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

/**
 * Remove a node from the tree by its id
 */
const removeNodeFromTree = (nodes: TreeNode[], nodeId: string): TreeNode[] => {
  return nodes.filter(node => {
    if (node.id === nodeId) {
      return false;
    }
    if (node.children) {
      node.children = removeNodeFromTree(node.children, nodeId);
    }
    return true;
  });
};

const NavigationTab: React.FC<NavigationTabProps> = ({
  appState,
  journalService,
  onPathSelect,
  onExpandedNodesChange,
}) => {
  const [treeData, setTreeData] = useState<TreeNode[]>([]);

  useEffect(() => {
    if (journalService && appState.rootIndex >= 0) {
      loadRootNodes();
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [journalService, appState.rootIndex]);

  const loadRootNodes = () => {
    const rootNodes: TreeNode[] = [
      {
        id: 'local-state',
        label: 'state',
        type: 'directory',
        path: [['*state*']],
        isLocal: true,
      },
      {
        id: 'peers',
        label: 'peer',
        type: 'directory',
        path: [appState.rootIndex, ['*peer*']],
        isLocal: false,
      },
    ];
    setTreeData(rootNodes);
  };

  const toggleNode = async (node: TreeNode) => {
    const newExpanded = new Set(appState.expandedNodes);

    if (newExpanded.has(node.id)) {
      newExpanded.delete(node.id);
    } else {
      newExpanded.add(node.id);
      if (!node.children && node.type === 'directory') {
        await loadChildren(node);
      }
    }

    onExpandedNodesChange(newExpanded);
  };

  const isPeerChainNode = (path: JournalPath): boolean => {
    if (path.length < 2) return false;
    
    const lastSegment = path[path.length - 1];
    const previousSegment = path[path.length - 2];
    
    return (
      typeof lastSegment === 'number' &&
      Array.isArray(previousSegment) &&
      previousSegment[0] === '*peer*' &&
      previousSegment.length === 3
    );
  };

  const createPeerChainChildren = (node: TreeNode): TreeNode[] => [
    {
      id: `${node.id}-state`,
      label: 'state',
      type: 'directory',
      path: [...node.path, ['*state*']],
      isLocal: false,
    },
    {
      id: `${node.id}-peer`,
      label: 'peer',
      type: 'directory',
      path: [...node.path, ['*peer*']],
      isLocal: false,
    },
  ];

  const loadChildren = async (node: TreeNode) => {
    if (!journalService) return;

    try {
      // Special handling for peer chain nodes
      if (isPeerChainNode(node.path)) {
        node.children = createPeerChainChildren(node);
        setTreeData([...treeData]);
        return;
      }

      const response = await journalService.get(node.path);

      const directory = JournalService.parseDirectoryResponse(response.content);
      
      if (!directory) {
        // Not a directory - this is a file, update the node type
        node.type = 'file';
        node.children = undefined;
        setTreeData([...treeData]);
        return;
      }

      const children: TreeNode[] = directory.items
        .filter(itemName => itemName !== '*directory*') // Hide the directory marker file
        .sort((a, b) => a.localeCompare(b)) // Sort alphabetically
        .map(itemName => {
          return {
            id: `${node.id}-${itemName}`,
            label: itemName,
            // Default to directory, will be updated when expanded
            type: 'directory' as const,
            path: buildChildPath(node.path, itemName),
            isLocal: node.isLocal,
          };
        });

      node.children = children;
      setTreeData([...treeData]);
    } catch (error) {
      node.children = [{
        id: `${node.id}-error`,
        label: `Error: ${error instanceof Error ? error.message : 'Failed to load'}`,
        type: 'file',
        path: node.path,
        isLocal: false,
      }];
      setTreeData([...treeData]);
    }
  };

  const handleDelete = async (node: TreeNode) => {
    if (!journalService || !node.isLocal) return;

    try {
      await journalService.delete(node.path);
      // Remove the node from the tree without reloading
      setTreeData(prevTreeData => removeNodeFromTree([...prevTreeData], node.id));
    } catch (error) {
      alert(`Failed to delete: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  };

  const handleAddFile = async (node: TreeNode) => {
    if (!journalService || !node.isLocal || node.type !== 'directory') return;

    const fileName = prompt('Enter file name:');
    if (!fileName) return;

    const lastSegment = node.path[node.path.length - 1];
    
    if (!Array.isArray(lastSegment) || lastSegment[0] !== '*state*') {
      return;
    }

    const filePath: JournalPath = [
      ...node.path.slice(0, -1), 
      ['*state*', ...lastSegment.slice(1), fileName]
    ];

    try {
      await journalService.set(filePath, { '*type/string*': '' });
      await loadChildren(node);
    } catch (error) {
      alert(`Failed to add file: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  };

  const handleAddDirectory = async (node: TreeNode) => {
    if (!journalService || !node.isLocal || node.type !== 'directory') return;

    const dirName = prompt('Enter directory name:');
    if (!dirName) return;

    const lastSegment = node.path[node.path.length - 1];
    
    if (!Array.isArray(lastSegment) || lastSegment[0] !== '*state*') {
      return;
    }

    // Create a dummy file inside the new directory to mark it as a directory
    const dirMarkerPath: JournalPath = [
      ...node.path.slice(0, -1), 
      ['*state*', ...lastSegment.slice(1), dirName, '*directory*']
    ];

    try {
      await journalService.set(dirMarkerPath, true);
      await loadChildren(node);
    } catch (error) {
      alert(`Failed to add directory: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  };

  const renderNode = (node: TreeNode, level: number = 0): JSX.Element => {
    const isExpanded = appState.expandedNodes.has(node.id);
    const isSelected = JSON.stringify(node.path) === JSON.stringify(appState.selectedPath);
    const isError = node.label.startsWith('Error:');
    const isSpecial = node.label === 'peer' || node.label === 'state';
    const isDirectory = node.type === 'directory';
    // Only show add buttons for confirmed directories (those that have been expanded and have children)
    const isConfirmedDirectory = isDirectory && node.children !== undefined;

    return (
      <div key={node.id} className="tree-node">
        <div 
          className={`tree-node-content ${isSelected ? 'selected' : ''}`}
          style={{ paddingLeft: `${level * 10}px` }}
        >
          <span className="tree-node-icon" onClick={() => isDirectory && toggleNode(node)}>
            {isDirectory ? (isExpanded ? '▼' : '▶') : '📄'}
          </span>
          <span 
            className={`tree-node-label ${isSpecial ? 'special' : ''} ${isError ? 'tree-node-error' : ''}`}
            onClick={() => !isError && onPathSelect(node.path)}
          >
            {node.label}
          </span>
          {node.isLocal && (
            <div className="tree-node-actions">
              <button
                className="tree-node-action"
                onClick={() => handleDelete(node)}
                title="Delete"
              >
                🗑️
              </button>
              {isConfirmedDirectory && (
                <>
                  <button
                    className="tree-node-action"
                    onClick={() => handleAddFile(node)}
                    title="New file"
                  >
                    📝
                  </button>
                  <button
                    className="tree-node-action"
                    onClick={() => handleAddDirectory(node)}
                    title="New folder"
                  >
                    📁
                  </button>
                </>
              )}
            </div>
          )}
        </div>
        {isExpanded && node.children && (
          <div className="tree-node-children">
            {node.children.map(child => renderNode(child, level + 1))}
          </div>
        )}
      </div>
    );
  };

  return (
    <div className="tree-view">
      {treeData.map(node => renderNode(node))}
    </div>
  );
};

export default NavigationTab;
