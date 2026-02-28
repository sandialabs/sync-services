# Synchronic Web Workbench

A developer interface for querying synchronic web journals.

## Overview

The Synchronic Web Workbench provides a structured interface to interact programmatically with synchronic web journals. It allows developers to write queries, view outputs, and explore the journal API.

## Development

### Prerequisites

- Node.js 18+
- npm

### Setup

    npm install

### Run locally

    npm start

The application will be available at http://localhost:3000

### Environment Variables

- REACT_APP_SYNC_WORKBENCH_ENDPOINT: Journal endpoint URL (default: http://localhost:4096/interface)

### Testing

    npm test

### Linting

    npm run lint

## Docker

### Build

    docker build -t synchronic-workbench .

### Run

    docker run -p 80:80 -e SYNC_WORKBENCH_ENDPOINT=http://your-journal:4096/interface synchronic-workbench

## Architecture

The application is a single-page React app with four main panes:

- **Left Pane**: API reference, functions, examples, and help documentation
- **Top Pane**: Query editor with multiple tabs
- **Bottom Pane**: Output viewer showing query, result, request, and response
- **Right Pane**: Query history

## Visual Design

The application uses a developer-focused design with a monospace font and IDE-like appearance. It supports both light and dark themes.

### Color Palette

- Blue: #00add0
- Medium Blue: #0076a9
- Dark Blue: #002b4c
- Supporting colors for syntax highlighting and status indicators
