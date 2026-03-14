# Synchronic Web Services

Monorepo for the Synchronic journal compose stack plus two web UIs:

- `explorer` for browsing/editing journal content
- `workbench` for developer-oriented journal queries
- `gateway` for versioned web-native API routes and Swagger docs over journal interfaces
- `router` for edge routing and optional TLS termination
- `file-system` for SMB projection of `/stage`, `/ledger`, and `/control`

## Quick Start

Run the compose stack (journal + nginx router + gateway + explorer + workbench):

```bash
SECRET=password PORT=8192 ./tests/local-compose.sh up
```

Run with direct HTTP compose deployment (no TLS):

```bash
SECRET=password PORT=8192 \
docker compose -f compose/general/docker-compose.yml up -d
```

Run with optional TLS (single compose file; router auto-enables TLS if cert/key files exist):

```bash
TLS_CERT_HOST_PATH=/absolute/path/to/fullchain.pem \
TLS_KEY_HOST_PATH=/absolute/path/to/privkey.pem \
SECRET=password PORT=8192 \
HTTPS_PORT=443 \
docker compose -f compose/general/docker-compose.yml up -d
```

Run with local Lisp sources for the journal bootstrap:

```bash
LOCAL_LISP_DIRECTORY=/absolute/path/to/lisp SECRET=password PORT=8192 ./tests/local-compose.sh up
```

Run automated smoke validation (up, verify, down):

```bash
./tests/local-compose.sh smoke
```

Run the compose stack with the SMB file-system service:

```bash
SECRET=password PORT=8192 SMB_PORT=445 ./tests/local-compose.sh up
```

Run the compose smoke with the SMB file-system service:

```bash
./tests/local-compose.sh smoke
```

`local-compose.sh` forces HTTP mode by default for local runs. To allow TLS behavior in local-compose, set `LOCAL_COMPOSE_FORCE_HTTP=0`.
If you need to temporarily disable the file-system service in local-compose, set `ENABLE_FILE_SYSTEM=0`.
If you need a local override image for active development, set `FILE_SYSTEM_IMAGE`.

Smoke validation with local Lisp override:

```bash
LOCAL_LISP_DIRECTORY=/absolute/path/to/lisp ./tests/local-compose.sh smoke
```

Bring down the base compose stack manually:

```bash
docker compose -f compose/general/docker-compose.yml down -v
```

## Documentation Map

- Compose deployment/testing docs: [compose/general/README.md](compose/general/README.md)
- Router service docs: [services/router/README.md](services/router/README.md)
- Explorer service docs: [services/explorer/README.md](services/explorer/README.md)
- Workbench service docs: [services/workbench/README.md](services/workbench/README.md)
- Gateway service docs: [services/gateway/README.md](services/gateway/README.md)
