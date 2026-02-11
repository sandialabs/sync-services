# Synchronic Web Ledger Compose Network

This repository contains materials to deploy a single ledger journal using Docker Compose

## Requirements

- Docker
- Docker Compose (currently developed on v2.26.1)

## Configuration

Please set the following environmental variables to configure the notary journal.

- `SECRET` (required): a string used to generate authentication credentials
- `PORT`: port number to forward on the host machine (default:8192)
- `PERIOD`: a nonnegative integer that determines the period of each synchronization step where period = 2 ^ PERIODICITY
- `WINDOW`: the number previous unpinned historical states to persist

## Start

`$ SECRET=password PORT=80 docker compose up`

## End

`$ docker compose down -v`
