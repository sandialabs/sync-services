import assert from "node:assert/strict";
import { test } from "node:test";
import type { FastifyRequest } from "fastify";
import { getAuthSecret } from "../src/auth";

const req = (headers: Record<string, string | undefined>): FastifyRequest =>
  ({ headers } as unknown as FastifyRequest);

test("extracts Bearer token from Authorization header", () => {
  const secret = getAuthSecret(req({ authorization: "Bearer password" }));
  assert.equal(secret, "password");
});

test("accepts raw Authorization token value", () => {
  const secret = getAuthSecret(req({ authorization: "password" }));
  assert.equal(secret, "password");
});

test("falls back to X-Sync-Auth when Authorization missing", () => {
  const secret = getAuthSecret(req({ "x-sync-auth": "password" }));
  assert.equal(secret, "password");
});

test("returns null when no supported auth headers are present", () => {
  const secret = getAuthSecret(req({}));
  assert.equal(secret, null);
});
