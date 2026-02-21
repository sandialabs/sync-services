# Synchronic Web Explorer

A React-based UI for exploring and interacting with synchronic web journals.

## Features

- Navigate hierarchical document structures across peer-to-peer networks
- View and edit journal content
- Manage peer connections
- View document history and versions
- Verify cryptographic proofs

## Development

### Prerequisites

- Node.js 18+
- npm or yarn

### Setup

```bash
npm install
```

### Run Development Server

```bash
npm start
```

The app will be available at http://localhost:3000

### Run Tests

```bash
npm test
```

### Build for Production

```bash
npm run build
```

### Docker

Build the container:

```bash
docker build -t explorer .
```

Run the container:

```bash
docker run -p 8080:80 explorer
```

## Configuration

The explorer connects to a synchronic web journal endpoint. You'll need:

1. A running journal service (e.g., using the compose/general setup)
2. The journal's endpoint URL (e.g., http://localhost:8192/interface/json)
3. The authentication password

### Environment Variables

You can pre-configure the endpoint and password using environment variables:

- `SYNC_EXPLORER_ENDPOINT`: Default journal endpoint URL
- `SYNC_EXPLORER_PASSWORD`: Default authentication password

#### Development
```bash
REACT_APP_SYNC_EXPLORER_ENDPOINT=http://localhost:8192/interface/json \
REACT_APP_SYNC_EXPLORER_PASSWORD=mypassword \
npm start
```

#### Docker
```bash
docker run -p 8080:80 \
  -e SYNC_EXPLORER_ENDPOINT=http://localhost:8192/interface/json \
  -e SYNC_EXPLORER_PASSWORD=mypassword \
  explorer
```

## Usage

1. Enter your journal endpoint and password in the toolbar
2. Click "Synchronize" to connect
3. Navigate through the file tree in the left pane
4. View and edit content in the middle pane
5. Explore document history in the right pane

For more detailed help, click the "Help" button in the toolbar.
