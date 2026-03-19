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

  if (segmentType === '*bridge*') {
    if (lastSegment.length === 1) {
      // Listing bridges - create bridge chain path
      return [...parentPath.slice(0, -1), ['*bridge*', itemName, 'chain'], -1];
    }
    if (lastSegment.length === 3) {
      // Already in bridge's chain
      return [...parentPath, -1, ['*bridge*', itemName, 'chain'], -1];
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
        id: 'bridges',
        label: 'bridge',
        type: 'directory',
        path: [appState.rootIndex, ['*bridge*']],
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

  const isBridgeChainNode = (path: JournalPath): boolean => {
    if (path.length < 2) return false;
    
    const lastSegment = path[path.length - 1];
    const previousSegment = path[path.length - 2];
    
    return (
      typeof lastSegment === 'number' &&
      Array.isArray(previousSegment) &&
      previousSegment[0] === '*bridge*' &&
      previousSegment.length === 3
    );
  };

  const createBridgeChainChildren = (node: TreeNode): TreeNode[] => [
    {
      id: `${node.id}-state`,
      label: 'state',
      type: 'directory',
      valueType: 'directory',
      path: [...node.path, ['*state*']],
      isLocal: false,
    },
    {
      id: `${node.id}-bridge`,
      label: 'bridge',
      type: 'directory',
      valueType: 'directory',
      path: [...node.path, ['*bridge*']],
      isLocal: false,
    },
  ];

  const loadChildren = async (node: TreeNode) => {
    if (!journalService) return;

    try {
      // Special handling for bridge chain nodes
      if (isBridgeChainNode(node.path)) {
        node.children = createBridgeChainChildren(node);
        setTreeData([...treeData]);
        return;
      }

      const response = await journalService.get(node.path);

      const directory = JournalService.parseDirectoryEntries(response.content);
      
      if (!directory) {
        // Not a directory - this is a file, update the node type
        node.type = 'file';
        node.valueType = 'value';
        node.children = undefined;
        setTreeData([...treeData]);
        return;
      }

      const children: TreeNode[] = directory
        .filter(entry => entry.name !== '*directory*') // Hide the directory marker file
        .sort((a, b) => a.name.localeCompare(b.name)) // Sort alphabetically
        .map(entry => {
          const nodeType: TreeNode['type'] =
            entry.type === 'directory' ? 'directory' : 'file';
          return {
            id: `${node.id}-${entry.name}`,
            label: entry.name,
            type: nodeType,
            valueType: entry.type,
            path: buildChildPath(node.path, entry.name),
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
        valueType: 'unknown',
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
    const isSpecial = node.label === 'bridge' || node.label === 'state';
    const isDirectory = node.type === 'directory';
    const nodeKind = node.valueType ?? (isDirectory ? 'directory' : 'value');
    const typeBadge =
      nodeKind === 'directory' ? 'DIR' : nodeKind === 'value' ? 'DOC' : 'UNK';
    // Only show add buttons for confirmed directories (those that have been expanded and have children)
    const isConfirmedDirectory = isDirectory && node.children !== undefined;

    return (
      <div key={node.id} className="tree-node">
        <div 
          className={`tree-node-content ${isSelected ? 'selected' : ''}`}
          style={{ paddingLeft: `${level * 10}px` }}
        >
          <span
            className={`tree-node-icon ${!isDirectory ? 'disabled' : ''}`}
            onClick={() => isDirectory && toggleNode(node)}
          >
            {isDirectory ? (isExpanded ? '▼' : '▶') : nodeKind === 'unknown' ? '❔' : '📄'}
          </span>
          <span 
            className={`tree-node-label ${isSpecial ? 'special' : ''} ${isError ? 'tree-node-error' : ''}`}
            onClick={() => !isError && onPathSelect(node.path)}
          >
            {node.label}
          </span>
          {!isError && !isSpecial && (
            <span className={`tree-node-type tree-node-type-${nodeKind}`}>{typeBadge}</span>
          )}
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
