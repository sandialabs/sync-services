# Synchronic Web Ledger Compose Network

This compose stack runs one journal with gateway, explorer, workbench, router, and the `file-system` SMB service.

## Requirements

- Docker
- Docker Compose

## Configuration

- `SECRET` (required): authentication secret for restricted journal/gateway operations
- `PORT` (default `8192`): host HTTP port exposed by router
- `HTTPS_PORT` (default `443`): host TLS port exposed by router
- `PERIOD` (default `2`): journal periodicity exponent
- `WINDOW` (default `1024`): retained historical state window
- `TLS_CERT_HOST_PATH` (default `./tls/tls.crt`): host certificate file mounted into router
- `TLS_KEY_HOST_PATH` (default `./tls/tls.key`): host key file mounted into router
- `ACME_WEBROOT_HOST_PATH` (default `./acme-challenge`): host directory mounted at `/var/www/acme-challenge` for HTTP-01 challenge files
- `TLS_CERT_FILE` (default `/etc/nginx/certs/tls.crt`): in-container certificate path used by router
- `TLS_KEY_FILE` (default `/etc/nginx/certs/tls.key`): in-container key path used by router
- `SMB_PORT` (default `445`): host port exposed by the `file-system` service
- `FILE_SYSTEM_IMAGE` (default `ghcr.io/sandialabs/sync-services/file-system:1.1.0`): image used by the optional `file-system` service
- `SYNC_FS_Backend` (default `http-journal-stage`): file-system backend override
- `SYNC_FS_JournalJsonUrl` (default `http://journal/interface/json`): direct journal JSON endpoint used by the default file-system backend
- `SYNC_FS_GatewayBaseUrl` (default `http://gateway/api/v1`): gateway endpoint used only when gateway-backed file-system modes are selected

Gateway note:
- `ALLOW_ADMIN_ROUTES` is enabled by default in `compose/general/docker-compose.yml`.
- Public/client-facing API traffic should go to `gateway` under `/api/v1/general/*` and `/api/v1/control/*`.
- The raw `/interface` endpoint is still present for direct journal transport use and bridge-oriented internals.
- The journal's periodic scheduler uses the raw control-step call `(*step* "<secret>")`, which depends on the merged `sync-records` control-step pipeline.

## TLS Behavior

This stack uses one compose file. Router auto-selects mode at startup:

- HTTP mode: if TLS cert/key files are not present
- TLS mode: if both `TLS_CERT_FILE` and `TLS_KEY_FILE` exist

`./tests/local-compose.sh` runs in normal HTTP mode by default unless valid TLS files are present at the configured paths.
`./tests/local-compose.sh` now forces HTTP mode by default (`LOCAL_COMPOSE_FORCE_HTTP=1`) for predictable local smoke runs.
Set `LOCAL_COMPOSE_FORCE_HTTP=0` if you explicitly want TLS behavior during local-compose execution.

In TLS mode, router serves:
- `80` for ACME HTTP-01 challenge path and HTTPS redirects
- `443` for proxied application routes

This is certificate-provider agnostic. Only file paths are required.

HTTP-only deployment (no TLS files configured):

```bash
SECRET=password PORT=8192 \
docker compose -f compose/general/docker-compose.yml up -d
```

The default compose stack now mounts ACME webroot to `/var/www/acme-challenge`.
Use `ACME_WEBROOT_HOST_PATH` to override where challenge files come from on the host.

Example:

```bash
TLS_CERT_HOST_PATH=/absolute/path/to/fullchain.pem \
TLS_KEY_HOST_PATH=/absolute/path/to/privkey.pem \
SECRET=password PORT=8192 \
HTTPS_PORT=443 \
docker compose -f compose/general/docker-compose.yml up -d
```

## Local Runner

Use the local helper from repository root:

```bash
# Interactive run
./tests/local-compose.sh up

# Smoke test
./tests/local-compose.sh smoke
```

The local compose helper enables the SMB file-system service by default:

```bash
./tests/local-compose.sh up
./tests/local-compose.sh smoke
```

To override the published file-system image during local development:

```bash
FILE_SYSTEM_IMAGE=sync-services/file-system:dev ./tests/local-compose.sh up
FILE_SYSTEM_IMAGE=sync-services/file-system:dev ./tests/local-compose.sh smoke
```

### Optional Local Scheme Sources

If `LOCAL_LISP_DIRECTORY` is set, the runner serves that directory over a temporary local HTTP server and builds `compose/general` with `LISP_REPOSITORY` pointing at that local server.

```bash
LOCAL_LISP_DIRECTORY=/absolute/path/to/lisp ./tests/local-compose.sh up
LOCAL_LISP_DIRECTORY=/absolute/path/to/lisp ./tests/local-compose.sh smoke
```

`LOCAL_LISP_DIRECTORY` must contain:
- `control.scm`
- `standard.scm`
- `log-chain.scm`
- `linear-chain.scm`
- `tree.scm`
- `ledger.scm`
- `interface.scm`

## Manual Teardown

```bash
docker compose -f compose/general/docker-compose.yml down -v
```
