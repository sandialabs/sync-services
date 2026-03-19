# Project: Synchronic Gateway

## Objective

This service is a web-facing API gateway for the Synchronic Web `general` interface.
It accepts external HTTP requests (including from other compose services), validates and normalizes them, and forwards equivalent calls to journal endpoints.
The goal is to provide a stable, versioned, and web-native API surface while preserving correctness against the underlying journal semantics.

## Chosen Stack

- Runtime: Node.js 20+
- Language: TypeScript
- Framework: Fastify
- API docs: `@fastify/swagger` + `@fastify/swagger-ui`
- Validation: Fastify JSON schema route validation

Rationale: this is the lightest-weight path to a maintainable service in the existing repo, with strong routing performance and straightforward OpenAPI support.

## Scope (Initial)

- Support both `general` and `control` operations through separate route namespaces.
- Keep endpoint consistency with function-final paths (for example `/api/v1/general/get`).
- Keep simple read aliases (`size`, `information`) as `GET`.
- Use `POST` as canonical call method for all function-style operations.
- Move client authentication to headers at the gateway boundary.
- Provide API versioning for forward/backward compatibility.
- Publish OpenAPI docs with Swagger UI.
- Forward requests to journal `interface/json` or `/interface` based on content negotiation.

## Non-Goals (Initial)

- Replacing journal semantics or implementing independent state logic.
- Supporting unrestricted `*eval*`-style admin execution over public routes.
- Building long-term credential management (start with shared-secret header support).

## API Design Principles

- Compatibility-first: map cleanly to existing `general` methods and path semantics.
- Explicitness: routes should clearly encode operation intent.
- Consistency: final URL segment is always a function name for operation endpoints.
- URL ergonomics: external routes use URL-safe function aliases instead of Lisp earmuff/punctuation names.
- Predictability: error responses should be normalized and typed.
- Evolvability: changes go through versioned routes (`/api/v1/...`).
- Observability: structured logs, request IDs, and upstream timing should be first-class.
- Metrics emission: the gateway should expose Prometheus-format metrics directly from the process.

## Proposed Route Surface (Draft v1)

Base path: `/api/v1`

General namespace: `/api/v1/general`

- `GET /api/v1/general/size` (public)
- `GET /api/v1/general/information` (public)
- `GET /api/v1/general/bridges` (restricted)
- `POST /api/v1/general/get`
- `POST /api/v1/general/set` -> journal `set!`
- `POST /api/v1/general/pin` -> journal `pin!`
- `POST /api/v1/general/unpin` -> journal `unpin!`
- `POST /api/v1/general/synchronize`
- `POST /api/v1/general/resolve`
- `POST /api/v1/general/bridge` -> journal `bridge!`
- `POST /api/v1/general/general-bridge` -> journal `general-bridge!`
- `POST /api/v1/general/configuration`
- `POST /api/v1/general/step-generate`
- `POST /api/v1/general/step-chain` -> journal `step-chain!`
- `POST /api/v1/general/step-bridge` -> journal `step-bridge!`
- `POST /api/v1/general/set-secret` -> journal `*secret*`

Control namespace: `/api/v1/control` (admin only, disable by default)

- `POST /api/v1/control/eval` -> journal `*eval*`
- `POST /api/v1/control/call` -> journal `*call*`
- `POST /api/v1/control/step` -> journal `*step*`
- `POST /api/v1/control/set-secret` -> journal `*set-secret*`
- `POST /api/v1/control/set-step` -> journal `*set-step*`
- `POST /api/v1/control/set-query` -> journal `*set-query*`

### Route Alias Mapping

Gateway route aliases are URL-safe and map to canonical journal function names:

- `set` -> `set!`
- `pin` -> `pin!`
- `unpin` -> `unpin!`
- `bridge` -> `bridge!`
- `general-bridge` -> `general-bridge!`
- `step-chain` -> `step-chain!`
- `step-bridge` -> `step-bridge!`
- `general/set-secret` -> `*secret*`
- `control/eval` -> `*eval*`
- `control/call` -> `*call*`
- `control/step` -> `*step*`
- `control/set-secret` -> `*set-secret*`
- `control/set-step` -> `*set-step*`
- `control/set-query` -> `*set-query*`

## Content Negotiation (Arguments)

Gateway keeps endpoint/function contract fixed and uses `Content-Type` to interpret argument format:

- `application/json`: arguments supplied in JSON body.
- `text/plain` (or `application/lisp`): arguments supplied as Lisp text.

Forwarding behavior:

- JSON requests are forwarded to `/interface/json`.
- Lisp requests are composed by gateway into full Lisp expressions (function + auth + args) and forwarded to `/interface`.

This keeps authentication/function routing consistent while allowing both JSON and Lisp client ergonomics.

## Translation Layer

Gateway request -> journal request translation responsibilities:

- HTTP method + route + params/body -> `{ function, arguments, authentication }` (JSON mode) or full Lisp expression (Lisp mode)
- Path normalization and validation (including `*state*`/bridge path conventions).
- Type normalization for Lisp-sensitive values (`*type/string*`, etc.) when needed.
- Upstream error mapping into consistent HTTP status + error body.

## Versioning Strategy

- URI versioning: `/api/v1`.
- Breaking changes only in new major path versions (`/api/v2`).
- Additive changes allowed within version where backwards compatible.
- OpenAPI doc per version.

## OpenAPI / Swagger

- Generate OpenAPI spec from route schemas.
- Serve Swagger UI at a stable docs endpoint (example: `/api/docs`).
- Include representative request/response examples mapped to journal behavior.

## Operational Requirements

- Configurable upstream journal URL(s) via environment variables.
- Timeouts, retries (bounded), and upstream circuit-breaker behavior.
- Structured logs with request ID propagation.
- Public metrics endpoint:
  - `GET /metrics`
- Emitted metrics should include:
  - default Node.js/process metrics
  - gateway request totals, durations, and in-flight count
  - upstream journal request totals and durations
- Health endpoints:
  - `GET /healthz` (liveness)
  - `GET /readyz` (readiness + upstream check)

## Testing Expectations

- Unit tests for request validation and translation logic.
- Integration tests against a real journal endpoint (compose-based).
- Contract tests for OpenAPI examples and response schemas.
- Security tests for auth bypass and restricted-route protection.

## Open Decisions

1. Whether both `bridge!` and `general-bridge!` should be exposed in v1 or one should be preferred.
2. Whether control routes should ship in the first release or remain feature-flagged.
3. Whether gateway should support fan-out to multiple journals or a single upstream per deployment.

## Working Notes

- Keep this file as the living design source for gateway decisions.
- As decisions are made, move items from **Open Decisions** into concrete sections above.

### Decisions Made

- Stack selected: TypeScript + Fastify.
- Default auth input supports both `Authorization: Bearer <secret>` and `X-Sync-Auth: <secret>`.
- Route model selected: function-final endpoints under `/api/v1/general/*` and `/api/v1/control/*`.
- `size` and `information` remain `GET`; operation endpoints are canonical `POST`.
- `synchronize` is `POST` (for future argument expansion).
- Content negotiation approach selected: JSON and Lisp support by `Content-Type`.
