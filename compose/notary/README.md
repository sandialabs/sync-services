# Synchronic Web Notary Compose Network

This repository contains materials to deploy a single notary journal using Docker Compose

## Requirements

- Docker
- Docker Compose (currently developed on v2.26.1)

## Configuration

Please set the following environmental variables to configure the notary journal.

- `PORT`: port number to forward on the host machine (default:8192)
- `SEED` (required): a 32-byte hex string used to identify and authenticate journal requests
- `PERIODICITY`: a nonnegative integer that determines the period of each synchronization step where period = 2 ^ PERIODICITY
- `PROMISE_INTERVAL`: a nonnegative integer that determines the number of blocks in the future the notary will accept promises
- `PROOF_INTERVAL`: a nonnegative integer that determines the number of blocks in the past that the notary will provide proofs

To enable TLS, please generate the certificate and corresponding private key and place them in following files in the root directory:

- certificate: ./tls_certificate.pem
- private key: ./tls_private.pem

## Start

`$ docker compose up`

## End

`$ docker compose down -v`
