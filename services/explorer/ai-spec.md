# Project: Synchronic Web Explorer

## Objective

This service is a UI front-end for exploring a synchronic web journal implementing the "general" interface.
It provides non-technical users with a simple interface to both write to their journal as well as read from an arbitrarily large network of directly and indirectly connected peers.
As an exploration tool, it will mainly be used by new users to understand the synchronic web and for experienced users to showcase the size and ease of use.
While agnostic to any particular domain or application, the features on the web page should highlight essential features and capabilities of the system.

## Architecture

- Language/runtime: Web browser
- Frameworks: React, Typescript
- Deployment target: local (testing) and single Docker container (deployment)
- Data storage: NONE (must use Journal for all persistent state)

## Restrictions

- Must not make any network calls except to the journal endpoint
- Must not make any system calls to access host information
- Must ask before introducing new dependencies

## Visual Design

- core palette
  - blue: #00add0
  - medium blue: #0076a9
  - dark blue: #002b4c
  - black: #000000
- supporting palette
  - purple: #830065rgb
  - red: #ad0000rgb
  - orange: #ff8800rgb
  - yellow: #ffc200rgb
  - green: #6cb312rgb
  - teal: #008e74rgb
  - blue gray: #7d8ea0rgb

## Coding standards

- Style: prettier
- Documentation: TSDoc
- Testing: Vitest

## Dev workflow 

- Setup: `npm install`
- Run: `npm start`
- Test: `npm test`
- Lint/format: `npm run lint`
- Containerize: `docker build .`

## Journal Service

The backend to this user interface is a synchronic web journal.
A journal is essentially one node in an arbitrarily large and distributed peer-to-peer network of nodes.
For the purposes of this application, all relevant journals implements a "general" interface with a root "ledger" object that does two things:

- tracks writable data into a versioned and hierarchically organized local state
- allows cryptographically verifiable read-only access to other directly and indirectly peered journals in the network

## Journal API Call Format

All calls to the synchronic web journal will be a JSON POST request to the user-provided endpoint.
Request bodies will always have the following shape:

```json
{
    "function": <function name>,
    "arguments": [
        "<potentially>",
        "<empty>",
        "<list>",
        "<of>",
        "<positional>",
        "<arguments>",
    ],
    "authentication": <password>,
}
```

Arguments can be any valid JSON type.
If there are no arguments or authentication is not requred, these fields can be omitted.
On the journal side, the build-in scheme interpreter will convert JSON types into native s7 Scheme representations.
There is some nuance to this, but for the purposes of this application, please note the following:

- Arguments that should be s7 Scheme strings, use: `{ "*type/string*": <contents of the string> }`
- Arguments that should be s7 Scheme byte vectors, use: `{ "*type/byte-vector*": <hex-encoded byte string> }`

## Journal Path Format

An important argument for most function calls the `path` argument.
This is a (nested) list of integers and symbols and that determines the location of a piece of data in the synchronic web relative to a given journal index.
The following is a realistic example of a path:

```json
[42, ["*peer*", "alice", "chain"], -1, ["*peer*", "bob", "chain"], -3, ["*state*", "some", "directory"]]
```

This path is an unambiguous and verifiable way to get the data at "some/directory" in the third-to-latest index of "bob"'s journal tracked by the latest index of "alice"'s journal tracked by the 42nd index of the local root journal.
To list bob's alices peers ahead of time, the path can be something like `[42, ["*peer*", "alice", "chain"], -1, ["*peer*"]]`
Some normative notes for all paths:

- The top-level structure of the path MUST alternate between an integer and a list and end on a list
  - If the first element is a list, then it is understood that the query is targeting the unpublished "staging" version of the document tree
- Except for the very last list, all other lists MUST be of the form `["*peer*", <name of peer as symbol>, "chain"]`
- The very last list MUST have the form `["*state*", <some>, <length>, <of> <symbols>]` to retrieve data content OR `["*peer*"]` to retrieve a list of peers
- For the purpose of this user interface, the first index integer MUST be a positive number while all other index integers MUST be a negative number (interpreted as negative indexing from the latest state in the chain).

## Journal API Functions

The following is the authoritative list of Journal API functions that can be called.

- `size`
  - Description: get current size of the ledger
  - Authenticated: no
  - Arguments: None
  - Return: `<some integer>`
- `general-peer!`:
  - Description: add a new peer 
  - Authenticated: yes
  - Arguments: `[<peer name as symbol>, <http endpoint as string>]`
  - Return `<true|false to indicate success>`
- `set!`:
  - Description: set data at path to the new value.
    - if the value is the list `["nothing"]`, then the effect is to delete the document
  - Authenticated: yes
  - Arguments: [`<path>`, `<value with any type>`]
  - Return `<true|false to indicate success>`
- `get`
  - Description: get the existing value at the path alongside metadata.
    - for the purpose of this app, the second argument MUST be set to true
  - Authenticated: yes
  - Arguments: [`<path>`, true]
  - Return `{ "content": <value with any type>, "pinned?": <canonical path>, "proof": <arbitrarily complex object with cryptographic information>}`
    - if the path is a directory, then the `content` field will have a list of subdirectories
    - if the path is definitely empty, the `content` field will be `["nothing"]`
    - if the path is unknown (because it has been pruned), the `content field will be `["unknown"]`
    - Otherwise, the `content` field is a normal document that can take any other valid JSON form
- `pin!`
  - Description: pin the value at the specified path so it doesn't get pruned
  - Authenticated: yes
  - Arguments: [`<path>`]
  - Return `<true|false to indicate success>`
- `unpin!`
  - Description: unpin the value at the specified path so it gets pruned once enough time has passed 
  - Arguments: [`<path>`]
  
## Application Overview 

The application is a single-page app that is broken down into three panes with a tool bar at the top.
Each pane has some tabs that can select between the panes.
Additionally, all three panes should enable scrolling to handle potentially long content.
The left pane handles navigating across the document space (peers and file systems).
The middle pane handles information about the specified document content.
The right pane displaying history across document time (different versions).
The overall look and feel should resemble a clean text editor.
Upon refresh (of the page, or manually via the synchronize button) the app should immediate query the journal for the latest size and use the resulting index (size - 1) as the first part of every path argument.

## Tool Bar

The navigation bar should contain the following elements:
- synchronize button: to get the latest size and use that as the root of the path query (side effect: reset everything else on the page)
- endpoint/authentication input: a pair of free-text fields to point the application to a backend journal and password to use for authentication
- status message: some type of dynamic message or loading graphic to let the user know a network query is being executed and when it is finished
- help button: a button that displays a modal with information taken from this document about how to interact with the explorer

## Left Pane

### Tab 1: Navigation

- Display an expandable tree, resembling a file explorer tree, that allows user to expand and click on peers and subdirectories to expand the tree
  - The tree structure should resemble the `path` but make it cleaner, namely:
    - Representing *peer* and *state* as `peer` and `state`, respectively, distinguishing them as special by bolding them
    - Ommitting the indices (that's handled in the history pane)
- For the journal's local state, also have icons next to directories that allow:
  - deleting the directory
  - adding a new empty file under that directory
- When clicking on any directory or document directly, it should appear in the middle content pane
  - For all directories and documents in the journal's local state, the `get` `path` should start with a list to pull it from the staging tree
  - For all directories and documents from remote journals, the remote path indices should be `-1` to start off with unless later changed in the right history pane

### Tab 2: Peer Information

- Display expandable information about immediate peers using the `peer` function
- Input form to add new peer using the `general-peer!` function

## Middle Pane

### Tab 1: Content

- View main directory or document as text
- If document is under the journal's local state, and it is on the "staging" version, then include a button that allows it to go into "edit" mode
  - Once the user is done editing, then the user should be able to press a "save" button located in the same place as the initial "edit" button to `set!` the document to the journal
- For all files/directories that are not from staging (i.e., its path starts with a number, there should also be a "pin/unpin" icon
  - The initial value can be retrieved from the `pinned?` field of the `get` function
  - The effect of pressing the icon is to send the corresponding function to the journal 
  - If the user pins or unpins a directory, recursively apply the effect to all children
  
## Tab 2: Verification

- This is a read-only informative pane that displays a read-only, pretty-printed, and syntax-highlighted dump of the contents under the `proof` field of the `get` return

## Right Pane

- The right pane has an arbitrary number of tabs: one for each journal in the path to the contents in the middle pane
  - For instance, if selecting a document from the local tree, there should be one tab
  - If selecting a document that is two peers away, there should be three tabs
- Initially, each tab will only have one row that includes a truncated, one-line text prefix of the content in the middle pane
- Underneath, there should be options to expand previous versions of the document using increasingly distant negative prefixes
  - The negative prefixes should increment as a power of 2, so -2, -4, -8, -16, -32, etc.
  - As the user clicks on each, change the path query to `get` the document that corresponds to setting that negative prefix (as opposed to the default of -1)
  - If the user selects another piece of data (or re-clicks the current one), make sure the clear the history tab since it is no longer relevant
