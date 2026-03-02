# Synchronic Web Services

Monorepo for the Synchronic journal compose stack plus two web UIs:

- `explorer` for browsing/editing journal content
- `workbench` for developer-oriented journal queries
- `gateway` for versioned web-native API routes and Swagger docs over journal interfaces

## Quick Start

Run the compose stack (journal + nginx interface + gateway + explorer + workbench):

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

- Compose deployment/testing docs: [compose/general/README.md](compose/general/README.md)
- Explorer service docs: [services/explorer/README.md](services/explorer/README.md)
- Workbench service docs: [services/workbench/README.md](services/workbench/README.md)
- Gateway service docs: [services/gateway/README.md](services/gateway/README.md)

## Issues

- [ ] Prettify feature incorrectly removes whitespace before single quote
- [ ] Need to ensure that smoke-test/up-compose works locally
- [ ] Put better control over the functions exposed by interface (e.g., ignore step)
- [ ] Point explorer to swagger interface rather than journal directly
