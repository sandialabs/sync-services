# Project: Synchronic Web Workbench

## Objective

This service is a UI front-end for querying a synchronic web journal, with focus on the "general" interface
It provides potential developers with a structured interface to interact programmatically with the journal ranging from sending simple queries to authoring core operating software.
The core functionality is to allow the user to write queries and view outputs of the query.
As a developer interface, its look and feel should conform to similar terminal, text editing, IDE, and query tools.

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

## Journal API

The default journal endpoint should be settable as an environmental variable `SYNC_WORKBENCH_ENDPOINT=http://localhost:4096/interface`
All queries to the journal will take the following form:

`POST <endpoint> <scheme-query-string>`

## API Template

// todo: this should come from the journal

## Application Overview

The application is a single-page app this broken down into four main panes with a tool bar at the top.
The left and right panes should take up the whole height of the screen while the "top" and "bottom" panes are really middle-top and middle-bottom, but they should be the widest since they have actual text input/output.
The left pane contains help information for guiding the user.
The top pane contains a free text input that serves as the query.

## Tool Bar

The tool bar should contain the following elements:
- a home button displaying a circular synchronic web logo that allows the user to go to the "home" page
- status message: some type of dynamic message or loading graphic to let the user know a network query is being executed and when it is finished
- dark/light mode button: self explanatory
- help button: a button that displays a modal with information taken from this document about how to interact with the workbench

## Top Pane (Input)

- The top pane should have an arbitrary number of tabs, a download button, and a query button
- Tabs should be creatable by users to have different queries they work on simultaneously, essentially
- The save button allows them to download the query on the current opened tab
- The run button allows them to send the current query to the journal

## Bottom Pane (Output)

- The bottom pane has four tabs and a download button
- The first "query" tab displays the input query
- The first "result" tab displays the output result
- The second "request" tab shows the raw HTTP request
- The third "response" tab shows the raw HTTP response
- The second tab should be the one that's opened by default
- The download button allows the user to download whichever tab is currently active

## Left Pane (API)

- The left pane contains three tabs: API, Functions, Examples
- The API tab lists, in order, the API names in `help-api.json` and the ability to hover over or click on them for the full description and templates/examples
- The Functions tab lists, in order, the function names in `help-functions.json` and the ability to hover over or click on them for the full description
- The Functions tab lists, in order, the advanced examples in `help-examples.json` and the ability to hover over or click on them for the full description and copy the code to the input

## Right Pane (History)

- The right pane shows a running history of all previous queries
- Every time the "query" button is sent, it should push a new line to the top of the history pane.
- The text of the history pane should include a concise description of the query/result
- When a user clicks on a line, it should populate the bottom pane with the saved information
