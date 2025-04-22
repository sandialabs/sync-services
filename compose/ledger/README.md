# Synchronic Web Ledger Compose Network

This repository contains materials to deploy a single ledger journal using Docker Compose

## Requirements

- Docker
- Docker Compose (currently developed on v2.26.1)

## Configuration

Please set the following environmental variables to configure the notary journal.

- `PORT`: port number to forward on the host machine (default:8192)
- `SEED` (required): a 32-byte hex string used to identify and authenticate journal requests
- `PERIODICITY`: a nonnegative integer that determines the period of each synchronization step where period = 2 ^ PERIODICITY
- `NOTARY`: a URL pointing to an instance of a synchronic web notary 

## Start

`$ docker compose up`

## End

`$ docker compose down -v`
