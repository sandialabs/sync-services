# Synchronic Gateway

Web-facing API gateway for Synchronic `interface` and (optionally) `control` operations.
It presents a versioned HTTP interface that maps function-oriented journal calls into web-native routes, request schemas, and header-based authentication.

## What This Service Is For

Use `gateway` when clients should not call the journal interface directly.
It adds:

- stable versioned route paths (`/api/v1/...`)
- explicit auth headers instead of body-only credentials
- JSON and Lisp request-body support behind one route shape
- Swagger/OpenAPI docs for discoverability and onboarding
- Prometheus-compatible metrics emission at `/metrics`
- readiness/liveness probes for container orchestration

## Development

### Prerequisites

- Node.js 20+
- npm

### Install

```bash
npm install
```

### Run (dev)

```bash
npm run dev
```

### Build

```bash
npm run build
```

### Unit tests

```bash
npm test
```

### Run (prod)

```bash
npm run start
```

## Environment Variables

- `HOST` (default: `0.0.0.0`)
- `PORT` (default: `8180`)
- `JOURNAL_JSON_ENDPOINT` (default: `http://127.0.0.1:8192/interface/json`)
- `JOURNAL_LISP_ENDPOINT` (default: `http://127.0.0.1:8192/interface`)
- `REQUEST_TIMEOUT_MS` (default: `30000`)
- Request body limit: `64 MiB`
- `ALLOW_ADMIN_ROUTES` (default: `false`)
- `DEBUG_FORWARDING` (default: `false`)
- `DEBUG_FORWARDING_INCLUDE_AUTH` (default: `false`; unsafe, local debugging only)

## API Style

- Versioned base: `/api/v1`
- Function-final aliases:
  - `POST /api/v1/general/set` -> journal `set!`
  - `POST /api/v1/general/pin` -> journal `pin!`
  - etc.
- Public `GET` endpoints:
  - `/api/v1/general/size`
  - `/api/v1/general/info`

Swagger UI:

- `GET /api/v1/docs` (canonical)

Landing page:

- `GET /` (overview, route groups, quick examples)

## Content Negotiation

Gateway supports both JSON and Lisp request bodies for `POST` operation endpoints.

### JSON Mode

- `Content-Type: application/json`
- Body shape:

```json
{ ... }
```

- Use keyword-style argument object fields directly (for example `{ "path": ... }` for staged reads or `{ "path": ..., "pinned?": true, "proof?": true }` for committed/indexed `resolve` calls).

- Forwarded to: `/interface/json`

### Lisp Mode

- `Content-Type: text/plain` or `application/lisp`
- Body: Lisp arguments expression text only (not a full query envelope)

Example body:

```scheme
((path ((*state* docs article hash))) (pinned? #t) (proof? #t))
```

Gateway composes the full Lisp call expression and forwards to:

- `/interface`

## Authentication Headers

Restricted routes accept either:

- `Authorization: Bearer <secret>`
- `X-Sync-Auth: <secret>`

If both are present, the `Authorization` header is used first.

## Route Summary

### Health

- `GET /healthz`
- `GET /readyz`
- `GET /metrics` (public Prometheus metrics)

### Metrics

The gateway emits Prometheus-format metrics directly from the process at `GET /metrics`.

Included metrics:

- default Node.js/process metrics from `prom-client`
- `sync_gateway_requests_total`
- `sync_gateway_request_duration_seconds`
- `sync_gateway_in_flight_requests`
- `sync_gateway_journal_requests_total`
- `sync_gateway_journal_request_duration_seconds`

### General

- `GET /api/v1/general/size` (public)
- `GET /api/v1/general/info` (public)
- `POST /api/v1/general/get`
- `POST /api/v1/general/set`
- `POST /api/v1/general/pin`
- `POST /api/v1/general/unpin`
- `POST /api/v1/general/batch`
- `POST /api/v1/general/synchronize`
- `POST /api/v1/general/resolve`
- `POST /api/v1/general/trace` (public)
- `POST /api/v1/general/bridge`
- `POST /api/v1/general/config`
- `POST /api/v1/general/set-secret`

### Control (disabled by default)

Enable with `ALLOW_ADMIN_ROUTES=1`:

- `POST /api/v1/control/eval`
- `POST /api/v1/control/call`
- `POST /api/v1/control/step`
- `POST /api/v1/control/set-secret`
- `POST /api/v1/control/set-step`
- `POST /api/v1/control/set-query`

## Examples

Public read:

```bash
curl http://127.0.0.1:8180/api/v1/general/size
```

Restricted JSON call:

```bash
curl -X POST http://127.0.0.1:8180/api/v1/general/get \
  -H "Authorization: Bearer password" \
  -H "Content-Type: application/json" \
  -d '{"path":[["*state*","docs","article","hash"]]}'
```

Restricted Lisp call:

```bash
curl -X POST http://127.0.0.1:8180/api/v1/general/get \
  -H "Authorization: Bearer password" \
  -H "Content-Type: text/plain" \
  -d '((path ((*state* docs article hash))))'
```

Restricted batch call:

```bash
curl -X POST http://127.0.0.1:8180/api/v1/general/batch \
  -H "Authorization: Bearer password" \
  -H "Content-Type: application/json" \
  -d '{
    "queries": [
      {
        "function": "get",
        "arguments": {
          "path": [["*state*","docs","article","hash"]]
        }
      },
      {
        "function": "config"
      }
    ]
  }'
```

Restricted batch call in Lisp mode:

```bash
curl -X POST http://127.0.0.1:8180/api/v1/general/batch \
  -H "Authorization: Bearer password" \
  -H "Content-Type: text/plain" \
  -d '((queries (((function get) (arguments ((path ((*state* docs article hash)))))
               ((function config))))))'
```

Forwarding debug mode:

```bash
DEBUG_FORWARDING=1 npm run dev
```

## Developer Notes

Recommended integration pattern:

1. Use `GET` endpoints for simple public reads (`size`, `info`).
2. Use `POST /api/v1/general/<operation>` for everything that takes arguments.
3. Default to JSON in services; use Lisp mode for advanced evaluator-native flows.
4. Use `batch` when one workflow needs multiple ordered ledger requests under one authenticated call.
5. Validate payloads in Swagger first, then copy canonical samples into tests.

## Metrics

The gateway emits Prometheus-format metrics at:

- `GET /metrics`

Current metrics include:

- default Node.js/process metrics from `prom-client`
- `sync_gateway_requests_total`
- `sync_gateway_request_duration_seconds`
- `sync_gateway_in_flight_requests`
- `sync_gateway_journal_requests_total`
- `sync_gateway_journal_request_duration_seconds`

Operational cautions:

- `control` routes are admin-level and disabled by default.
- `DEBUG_FORWARDING_INCLUDE_AUTH=1` logs raw secrets and should only be used in local debugging.
