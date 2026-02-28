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
- `LISP_DIR`: optional container path to override `control.scm`, `standard.scm`, `log-chain.scm`, `tree.scm`, `configuration.scm`, and `ledger.scm`

## Start

`$ SECRET=password PORT=80 docker compose up`

## Local Lisp Override

Use a second compose file to bind-mount Lisp files from another repository and override the journal startup script.

```bash
cd /code
LOCAL_LISP_PATH=/absolute/path/to/lisp \
SECRET=password \
PORT=8192 \
docker compose \
  -f compose/general/docker-compose.yml \
  -f tests/docker-compose.local.yml \
  up
```

`LOCAL_LISP_PATH` must contain:
- `control.scm`
- `standard.scm`
- `log-chain.scm`
- `tree.scm`
- `configuration.scm`
- `ledger.scm`

Then open:
- `http://localhost:8192/explorer/`
- `http://localhost:8192/workbench/`

## Programmatic Smoke Test

From repository root:

```bash
# Remote/baked Lisp content
./tests/smoke-compose.sh

# Local Lisp override from another repo
LOCAL_LISP_PATH=/absolute/path/to/lisp ./tests/smoke-compose.sh
```

The script starts the compose network, waits for Explorer/Workbench, checks journal API responses, and tears everything down automatically.

## End

`$ docker compose down -v`
