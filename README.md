# Synchronic Web Services

Monorepo for the Synchronic journal compose stack plus two web UIs:

- `explorer` for browsing/editing journal content
- `workbench` for developer-oriented journal queries

## Quick Start

Run the compose stack (journal + nginx interface + local explorer/workbench builds):

```bash
SECRET=password PORT=8192 ./tests/up-compose.sh
```

Run with local Lisp sources for the journal bootstrap:

```bash
LOCAL_LISP_PATH=/absolute/path/to/lisp SECRET=password PORT=8192 ./tests/up-compose.sh
```

Run automated smoke validation (up, verify, down):

```bash
./tests/smoke-compose.sh
```

Smoke validation with local Lisp override:

```bash
LOCAL_LISP_PATH=/absolute/path/to/lisp ./tests/smoke-compose.sh
```

Bring down the base compose stack manually:

```bash
docker compose -f compose/general/docker-compose.yml down -v
```

## Documentation Map

- Compose deployment/testing docs: [compose/general/README.md](/code/compose/general/README.md)
- Explorer service docs: [services/explorer/README.md](/code/services/explorer/README.md)
- Workbench service docs: [services/workbench/README.md](/code/services/workbench/README.md)
