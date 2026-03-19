import assert from "node:assert/strict";
import { test } from "node:test";
import Fastify from "fastify";
import { gatewayRoutes } from "../src/routes";
import { JournalSemanticError } from "../src/journal";
import type { JournalCall, JournalClient } from "../src/journal";

interface MockJournal {
  client: JournalClient;
  jsonCalls: JournalCall[];
  lispCalls: Array<{ expression: string; functionName: string }>;
}

const createMockJournal = (): MockJournal => {
  const jsonCalls: JournalCall[] = [];
  const lispCalls: Array<{ expression: string; functionName: string }> = [];

  return {
    jsonCalls,
    lispCalls,
    client: {
      async callJson(input: JournalCall): Promise<unknown> {
        jsonCalls.push(input);
        return { ok: true, mode: "json", function: input.functionName };
      },
      async callLisp(input: {
        expression: string;
        functionName: string;
      }): Promise<unknown> {
        lispCalls.push(input);
        return { ok: true, mode: "lisp", function: input.functionName };
      },
    },
  };
};

const createApp = async (input: {
  allowAdminRoutes: boolean;
  journal?: JournalClient;
}) => {
  const app = Fastify();
  app.addContentTypeParser("text/plain", { parseAs: "string" }, (_req, body, done) =>
    done(null, body)
  );
  app.addContentTypeParser("application/lisp", { parseAs: "string" }, (_req, body, done) =>
    done(null, body)
  );
  await app.register(gatewayRoutes, {
    journal: input.journal || createMockJournal().client,
    allowAdminRoutes: input.allowAdminRoutes,
  });
  await app.ready();
  return app;
};

test("GET /api/v1/general/size forwards to size without auth", async (t) => {
  const mock = createMockJournal();
  const app = await createApp({ allowAdminRoutes: false, journal: mock.client });
  t.after(async () => app.close());

  const res = await app.inject({ method: "GET", url: "/api/v1/general/size" });
  assert.equal(res.statusCode, 200);
  assert.equal(mock.jsonCalls.length, 1);
  assert.deepEqual(mock.jsonCalls[0], { functionName: "size" });
});

test("restricted route returns 401 without auth", async (t) => {
  const app = await createApp({ allowAdminRoutes: false });
  t.after(async () => app.close());

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/get",
    headers: { "content-type": "application/json" },
    payload: [],
  });
  assert.equal(res.statusCode, 401);
  const body = res.json();
  assert.equal(body.error, "unauthorized");
});

test("POST /api/v1/general/get accepts JSON keyword-object payload", async (t) => {
  const mock = createMockJournal();
  const app = await createApp({ allowAdminRoutes: false, journal: mock.client });
  t.after(async () => app.close());

  const args = {
    path: [["*state*", "docs"]],
    "pinned?": true,
    "proof?": true,
  };
  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/get",
    headers: {
      authorization: "Bearer password",
      "content-type": "application/json",
    },
    payload: args,
  });

  assert.equal(res.statusCode, 200);
  assert.equal(mock.jsonCalls.length, 1);
  assert.deepEqual(mock.jsonCalls[0], {
    functionName: "get",
    args,
    authentication: "password",
  });
});

test("POST /api/v1/general/get accepts legacy JSON array payload", async (t) => {
  const mock = createMockJournal();
  const app = await createApp({ allowAdminRoutes: false, journal: mock.client });
  t.after(async () => app.close());

  const args = [[["path", [["*state*", "docs"]]], ["pinned?", true], ["proof?", true]]];
  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/get",
    headers: {
      authorization: "Bearer password",
      "content-type": "application/json",
    },
    payload: args,
  });

  assert.equal(res.statusCode, 200);
  assert.equal(mock.jsonCalls.length, 1);
  assert.deepEqual(mock.jsonCalls[0], {
    functionName: "get",
    args,
    authentication: "password",
  });
});

test("POST /api/v1/general/get accepts Lisp payload and wraps expression", async (t) => {
  const mock = createMockJournal();
  const app = await createApp({ allowAdminRoutes: false, journal: mock.client });
  t.after(async () => app.close());

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/get",
    headers: {
      authorization: "Bearer password",
      "content-type": "text/plain",
    },
    payload: "(((path ((*state* docs))) (pinned? #t) (proof? #t)))",
  });

  assert.equal(res.statusCode, 200);
  assert.equal(mock.lispCalls.length, 1);
  assert.equal(mock.lispCalls[0].functionName, "get");
  assert.match(mock.lispCalls[0].expression, /^\(\(function get\) /);
  assert.match(
    mock.lispCalls[0].expression,
    /\(arguments \(\(\(path \(\(\*state\* docs\)\)\) \(pinned\? #t\) \(proof\? #t\)\)\)\)/
  );
  assert.match(mock.lispCalls[0].expression, /\(authentication "password"\)/);
});

test("POST /api/v1/general/general-batch accepts JSON payload", async (t) => {
  const mock = createMockJournal();
  const app = await createApp({ allowAdminRoutes: false, journal: mock.client });
  t.after(async () => app.close());

  const args = {
    requests: [
      {
        function: "get",
        arguments: {
          path: [["*state*", "docs"]],
          "pinned?": true,
          "proof?": false,
        },
      },
      {
        function: "configuration",
      },
    ],
  };

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/general-batch",
    headers: {
      authorization: "Bearer password",
      "content-type": "application/json",
    },
    payload: args,
  });

  assert.equal(res.statusCode, 200);
  assert.equal(mock.jsonCalls.length, 1);
  assert.deepEqual(mock.jsonCalls[0], {
    functionName: "general-batch!",
    args,
    authentication: "password",
  });
});

test("POST /api/v1/general/general-batch accepts Lisp payload and wraps expression", async (t) => {
  const mock = createMockJournal();
  const app = await createApp({ allowAdminRoutes: false, journal: mock.client });
  t.after(async () => app.close());

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/general-batch",
    headers: {
      authorization: "Bearer password",
      "content-type": "text/plain",
    },
    payload:
      "(((requests (((function get) (arguments ((path ((*state* docs))) (pinned? #t) (proof? #f))) ((function configuration))))))",
  });

  assert.equal(res.statusCode, 200);
  assert.equal(mock.lispCalls.length, 1);
  assert.equal(mock.lispCalls[0].functionName, "general-batch!");
  assert.match(mock.lispCalls[0].expression, /^\(\(function general-batch!\) /);
  assert.ok(mock.lispCalls[0].expression.includes("(requests "));
  assert.ok(
    mock.lispCalls[0].expression.includes(
      "((function get) (arguments ((path ((*state* docs))) (pinned? #t) (proof? #f)))"
    )
  );
  assert.ok(
    mock.lispCalls[0].expression.includes(
      "((function configuration))"
    )
  );
  assert.match(mock.lispCalls[0].expression, /\(authentication "password"\)/);
});

test("returns 415 for unsupported content type", async (t) => {
  const app = await createApp({ allowAdminRoutes: false });
  t.after(async () => app.close());

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/get",
    headers: {
      authorization: "Bearer password",
      "content-type": "application/xml",
    },
    payload: "<x/>",
  });

  assert.equal(res.statusCode, 415);
  assert.equal(res.json().error, "unsupported_media_type");
});

test("returns 400 for JSON arguments wrapper", async (t) => {
  const app = await createApp({ allowAdminRoutes: false });
  t.after(async () => app.close());

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/get",
    headers: {
      authorization: "Bearer password",
      "content-type": "application/json",
    },
    payload: { arguments: "bad" },
  });

  assert.equal(res.statusCode, 400);
  assert.equal(res.json().error, "invalid_request");
});

test("admin control routes are disabled unless explicitly enabled", async (t) => {
  const app = await createApp({ allowAdminRoutes: false });
  t.after(async () => app.close());

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/control/step",
    headers: { authorization: "Bearer password", "content-type": "application/json" },
    payload: [],
  });

  assert.equal(res.statusCode, 404);
});

test("admin control routes forward when enabled", async (t) => {
  const mock = createMockJournal();
  const app = await createApp({ allowAdminRoutes: true, journal: mock.client });
  t.after(async () => app.close());

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/control/step",
    headers: {
      authorization: "Bearer password",
      "content-type": "application/json",
    },
    payload: [],
  });

  assert.equal(res.statusCode, 200);
  assert.equal(mock.jsonCalls.length, 1);
  assert.deepEqual(mock.jsonCalls[0], {
    functionName: "*step*",
    args: [],
    authentication: "password",
  });
});

test("relays journal semantic error payloads as HTTP errors (JSON mode)", async (t) => {
  const journal: JournalClient = {
    async callJson(): Promise<unknown> {
      throw new JournalSemanticError({
        code: "authentication-error",
        message: "Could not authenticate restricted interface call",
        details: [
          "error",
          { "*type/quoted*": "authentication-error" },
          { "*type/string*": "Could not authenticate restricted interface call" },
        ],
      });
    },
    async callLisp(): Promise<unknown> {
      return { ok: true };
    },
  };

  const app = await createApp({ allowAdminRoutes: false, journal });
  t.after(async () => app.close());

  const res = await app.inject({
    method: "POST",
    url: "/api/v1/general/get",
    headers: {
      authorization: "Bearer password",
      "content-type": "application/json",
    },
    payload: { path: [["*state*", "docs"]], "pinned?": true, "proof?": true },
  });

  assert.equal(res.statusCode, 400);
  assert.deepEqual(res.json(), {
    error: "authentication-error",
    message: "Could not authenticate restricted interface call",
    details: [
      "error",
      { "*type/quoted*": "authentication-error" },
      { "*type/string*": "Could not authenticate restricted interface call" },
    ],
    source: "journal",
  });
});
