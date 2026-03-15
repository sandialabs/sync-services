import React from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import LedgerRouteBar from './LedgerRouteBar';
import { LedgerHop } from '../types';

describe('LedgerRouteBar', () => {
  const hops: LedgerHop[] = [
    { key: 'local', kind: 'local', name: 'Self', snapshot: '42' },
    { key: 'alice-1', kind: 'peer', name: 'alice', snapshot: 'latest' },
  ];

  it('shows the root sync button inline with the first hop', () => {
    render(
      <LedgerRouteBar
        hops={hops}
        peerChoices={null}
        rootIndex={42}
        onSynchronize={jest.fn()}
        onSnapshotChange={jest.fn()}
        onStepSnapshot={jest.fn()}
        onRemoveHop={jest.fn()}
        onOpenPeerPicker={jest.fn()}
        onClosePeerPicker={jest.fn()}
        onChoosePeer={jest.fn()}
      />,
    );

    expect(screen.getByTitle('Synchronize latest committed root')).toBeInTheDocument();
    expect(screen.getByDisplayValue('42')).toBeInTheDocument();
  });

  it('normalizes peer snapshot input on blur', () => {
    const onSnapshotChange = jest.fn();

    render(
      <LedgerRouteBar
        hops={hops}
        peerChoices={null}
        rootIndex={42}
        onSynchronize={jest.fn()}
        onSnapshotChange={onSnapshotChange}
        onStepSnapshot={jest.fn()}
        onRemoveHop={jest.fn()}
        onOpenPeerPicker={jest.fn()}
        onClosePeerPicker={jest.fn()}
        onChoosePeer={jest.fn()}
      />,
    );

    const inputs = screen.getAllByRole('textbox');
    fireEvent.change(inputs[1], { target: { value: '' } });
    fireEvent.blur(inputs[1], { target: { value: '' } });

    expect(onSnapshotChange).toHaveBeenLastCalledWith(1, 'latest');
  });

  it('shows peer choices inside the chooser shell when open', () => {
    render(
      <LedgerRouteBar
        hops={hops}
        peerChoices={['bob', 'carol']}
        rootIndex={42}
        onSynchronize={jest.fn()}
        onSnapshotChange={jest.fn()}
        onStepSnapshot={jest.fn()}
        onRemoveHop={jest.fn()}
        onOpenPeerPicker={jest.fn()}
        onClosePeerPicker={jest.fn()}
        onChoosePeer={jest.fn()}
      />,
    );

    expect(screen.getByText('bob')).toBeInTheDocument();
    expect(screen.getByText('carol')).toBeInTheDocument();
    expect(screen.getByTitle('Exit peer selection')).toBeInTheDocument();
  });
});
