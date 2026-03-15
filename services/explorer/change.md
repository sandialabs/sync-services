# Explorer Change Notes

This file is a temporary planning note for the current explorer refactor.
It replaces `ai-spec.md` as the working document for the next round of changes.

## Goal

Make the explorer feel more like the projected file-system view exposed by the file-system service, while still talking directly to the gateway/journal APIs.

The current explorer is still journal-native:

- top-level navigation is `state` and `peer`
- UI identity is mostly raw `JournalPath`
- peer traversal is treated as special-case journal logic
- content view exposes raw journal paths and raw document structure directly

The file-system model is cleaner:

- explicit projected roots
- stable path semantics
- directory-first browsing
- clearer separation between writable local state and committed ledger state

## Desired Direction

The explorer should adopt the same high-level split as the file-system, but not copy it literally.

The explorer UI should be organized around:

- `stage`
- `ledger`

There should be no explorer-side `control` namespace.
In the file-system service, `control` exists as a workaround for non-filesystem operations.
In the explorer, those behaviors already belong in explicit UI actions such as pin/unpin, so they should not be represented as a browsable tree.

This stage/ledger split should become the primary browsing model in the UI.
`JournalPath` should remain an internal transport detail rather than the main user-facing navigation model.

## Proposed Changes

### 1. Shared Explorer Shell

The explorer should have one shared top bar across both modes with:

- Sync Web logo
- `Stage` / `Ledger` mode switch
- password field
- dark/light mode toggle
- info/help button
- status/error line for network failures

The current wireframe reference for this shell is:

- `/code/wireframes/explorer-ledger-wireframes.html`

### 2. Stage Mode

Stage mode should be an editor-like local view.

Structure:

- left tree for local staged files/directories
- right content pane for the selected file or directory
- no dedicated history UI
- no stage route strip

Stage tree behavior:

- no visible `stage` root label at the top of the tree
- tree clicks select the current file or directory
- rename and delete move into inline tree-row actions
- no explicit stage sync control in the first version

Stage content behavior:

- selecting a directory shows a directory contents view
- selecting a file shows a read-only content view until `Edit` is pressed
- `Edit` turns into `Save` while editing

Stage content header actions:

When a directory is selected:

- `+ File`
- `+ Folder`
- `Upload File`

When a file is selected:

- `Edit` / `Save`
- `Download`

Rename and delete are not in the content header; they live in the tree.

### 3. Ledger Mode

Ledger mode should be a committed, route-based, read-only view.

Structure:

- top route strip
- left tree for the current route tip
- right content pane for the selected file or directory
- no dedicated history pane

Ledger route strip behavior:

- sync button at far left
- first hop is always the local/root journal
- each hop has a linear snapshot ticker
- snapshot field accepts `latest` or negative integers
- route persists when switching away from ledger and back

Ledger route extension behavior:

- choosing a next peer is a transient inline interaction
- the peer chooser appears only while extending the route
- the current wireframe shows the chooser-open state inline in the route strip

Ledger tree behavior:

- no visible `state` root label at the top of the tree
- tree implicitly represents the state tree for the current route tip
- selecting a directory shows a directory contents view
- selecting a file shows either content or proof

Ledger content header actions:

- one toggle button that flips between `Proof` and `Content`
- `Pin`

The proof/content toggle applies only to the currently selected document view.

### 4. UI Path Model

Introduce a projected path model for the explorer UI.

The UI should primarily work with filesystem-like projected paths, and convert them to `JournalPath` only at the service boundary.

This should follow the same semantics already used by the file-system layer in:

- `services/file-system/src/FileSystem.Server/JournalPathMapper.cs`

The explorer should not literally call the file-system service, but it should mimic its namespace and path semantics.
It should also intentionally omit the file-system service's `control` namespace.

### 5. Internal Representation Boundary

Recommended boundary:

- UI state uses projected paths and mode-specific UI state
- service layer maps projected paths to `JournalPath`
- journal/gateway responses are normalized into explorer-friendly directory/file concepts

This is the cleanest way to make the explorer actually resemble the file-system instead of only renaming a few labels.

## Primary Code Areas

The main files likely affected by this refactor are:

- `src/components/NavigationTab.tsx`
- `src/components/ContentTab.tsx`
- `src/utils/pathUtils.ts`
- `src/types/index.ts`
- `src/services/JournalService.ts`

## Current Implementation Notes

These are the main first-pass choices for the implementation now underway:

1. Stage does not expose any explicit sync affordance.
2. Stage tree row actions are icon-based rename/delete controls.
3. Ledger proof/content is a one-button toggle in the current document header.
4. Raw `JournalPath` should stay out of the default UI unless needed later for advanced inspection/debugging.

## Non-Goal

This refactor should not make the explorer depend on the file-system service itself.
The goal is to match its namespace model and browsing semantics, not to reuse it as a backend.
