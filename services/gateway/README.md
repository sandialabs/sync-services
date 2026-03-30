# Synchronic Gateway

Web-facing API gateway for Synchronic `general` and (optionally) `root` operations.
It presents a versioned HTTP API that maps function-oriented journal calls into web-native routes, request schemas, and header-based authentication.

## What This Service Is For

Use `gateway` when clients should not call the raw journal transport endpoints directly.
It adds:

- stable versioned route paths (`/api/v1/...`)
- explicit auth headers instead of body-only credentials
- JSON and Scheme request-body support behind one route shape
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
- `JOURNAL_SCHEME_ENDPOINT` (default: `http://127.0.0.1:8192/interface`)
- `ROOT_JSON_ENDPOINT` (default: `http://127.0.0.1:8192/interface/json`)
- `ROOT_SCHEME_ENDPOINT` (default: `http://127.0.0.1:8192/interface`)
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

Gateway supports both JSON and Scheme request bodies for `POST` operation endpoints.

### JSON Mode

- `Content-Type: application/json`
- Body shape:

```json
{ ... }
```

- Use keyword-style argument object fields directly (for example `{ "path": ... }` for staged reads or `{ "path": ..., "pinned?": true, "proof?": true }` for committed/indexed `resolve` calls).

- General routes are forwarded to the raw journal interface transport endpoint: `/interface/json`

### Scheme Mode

- `Content-Type: text/plain` or `application/scheme`
- Body: Scheme arguments expression text only (not a full query envelope)

Example body:

```scheme
((path ((*state* docs article hash))) (pinned? #t) (proof? #t))
```

Gateway composes the full Scheme call expression and forwards to the raw journal transport endpoint:

- `/interface`

### Root Route Forwarding

Root routes do not use the interface query envelope upstream.
They are forwarded as raw root calls instead:

- JSON mode:
  - `POST /api/v1/root/step` with `[]` becomes `["*step*", {"*type/string*": "<secret>"}]`
- Scheme mode:
  - `POST /api/v1/root/step` with body `()` becomes `(*step* "<secret>")`
  - `POST /api/v1/root/step` with body `(ledger-step #t)` becomes `(*step* "<secret>" (ledger-step #t))`

This matters because `*step*`, `*set-step*`, and related admin operations are raw root expressions, not general-interface queries.

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

### Root (disabled by default)

Enable with `ALLOW_ADMIN_ROUTES=1`:

- `POST /api/v1/root/eval`
- `POST /api/v1/root/call`
- `POST /api/v1/root/step`
- `POST /api/v1/root/set-secret`
- `POST /api/v1/root/set-step`
- `POST /api/v1/root/set-query`

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

Restricted Scheme call:

```bash
curl -X POST http://127.0.0.1:8180/api/v1/general/get \
  -H "Authorization: Bearer password" \
  -H "Content-Type: text/plain" \
  -d '((path ((*state* docs article hash))))'
```

Restricted root step call:

```bash
curl -X POST http://127.0.0.1:8180/api/v1/root/step \
  -H "Authorization: Bearer password" \
  -H "Content-Type: application/json" \
  -d '[]'
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

Restricted batch call in Scheme mode:

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
3. Default to JSON in services; use Scheme mode for advanced evaluator-native flows.
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

- `root` routes are admin-level and disabled by default.
- `DEBUG_FORWARDING_INCLUDE_AUTH=1` logs raw secrets and should only be used in local debugging.
