# Synchronic Web Ledger Compose Network

This compose stack runs one journal with gateway, explorer, workbench, and the dedicated `router` service.
It can also run the optional `file-system` SMB service through the `filesystem` compose profile.

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
- `SMB_PORT` (default `445`): host port exposed by the optional `file-system` service when the `filesystem` profile is enabled
- `FILE_SYSTEM_IMAGE` (default `sync-services/file-system:dev`): image used by the optional `file-system` service

Gateway note:
- `ALLOW_ADMIN_ROUTES` is enabled by default in `compose/general/docker-compose.yml`.

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

If you need to disable it temporarily:

```bash
ENABLE_FILE_SYSTEM=0 ./tests/local-compose.sh up
ENABLE_FILE_SYSTEM=0 ./tests/local-compose.sh smoke
```

If your local Docker/Colima setup cannot execute the amd64-only `journal-sdk:1.1.0` base image used by `compose/general`, you can skip the local `general` build and use the published image instead:

```bash
USE_REMOTE_GENERAL=1 ./tests/local-compose.sh up
USE_REMOTE_GENERAL=1 ./tests/local-compose.sh smoke
```

The journal service itself also runs as `linux/amd64` by default (`GENERAL_PLATFORM=linux/amd64`) because the current published `general` image and its `journal-sdk:1.1.0` base are amd64-only.

### Optional Local Lisp Sources

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
- `configuration.scm`
- `ledger.scm`

## Manual Teardown

```bash
docker compose -f compose/general/docker-compose.yml down -v
```
