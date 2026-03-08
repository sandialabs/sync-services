# Synchronic Gateway

Web-facing API gateway for Synchronic `general` and (optionally) `control` operations.
It presents a versioned HTTP interface that maps function-oriented journal calls into web-native routes, request schemas, and header-based authentication.

## What This Service Is For

Use `gateway` when clients should not call the journal interface directly.
It adds:

- stable versioned route paths (`/api/v1/...`)
- explicit auth headers instead of body-only credentials
- JSON and Lisp request-body support behind one route shape
- Swagger/OpenAPI docs for discoverability and onboarding
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
- `REQUEST_TIMEOUT_MS` (default: `10000`)
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
  - `/api/v1/general/information`
- Restricted `GET` endpoint:
  - `/api/v1/general/peers`

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
{
  "arguments": { ... }
}
```

- Preferred for ledger calls: keyword-style argument object fields.
- Direct keyword-object bodies are also accepted (for example `{ "path": ..., "details?": true }`).

- Forwarded to: `/interface/json`

### Lisp Mode

- `Content-Type: text/plain` or `application/lisp`
- Body: Lisp arguments expression text only (not a full query envelope)

Example body:

```scheme
((path ((*state* docs article hash))) (details? #t))
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

### General

- `GET /api/v1/general/size` (public)
- `GET /api/v1/general/information` (public)
- `GET /api/v1/general/peers` (restricted)
- `POST /api/v1/general/get`
- `POST /api/v1/general/set`
- `POST /api/v1/general/pin`
- `POST /api/v1/general/unpin`
- `POST /api/v1/general/synchronize`
- `POST /api/v1/general/resolve`
- `POST /api/v1/general/peer`
- `POST /api/v1/general/general-peer`
- `POST /api/v1/general/configuration`
- `POST /api/v1/general/step-generate`
- `POST /api/v1/general/step-chain`
- `POST /api/v1/general/step-peer`
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
  -d '{"arguments":{"path":[["*state*","docs","article","hash"]],"details?":true}}'
```

Restricted Lisp call:

```bash
curl -X POST http://127.0.0.1:8180/api/v1/general/get \
  -H "Authorization: Bearer password" \
  -H "Content-Type: text/plain" \
  -d '((path ((*state* docs article hash))) (details? #t))'
```

Forwarding debug mode:

```bash
DEBUG_FORWARDING=1 npm run dev
```

## Developer Notes

Recommended integration pattern:

1. Use `GET` endpoints for simple public reads (`size`, `information`).
2. Use `POST /api/v1/general/<operation>` for everything that takes arguments.
3. Default to JSON in services; use Lisp mode for advanced evaluator-native flows.
4. Validate payloads in Swagger first, then copy canonical samples into tests.

Operational cautions:

- `control` routes are admin-level and disabled by default.
- `DEBUG_FORWARDING_INCLUDE_AUTH=1` logs raw secrets and should only be used in local debugging.
