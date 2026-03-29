import type { FastifyPluginAsync, FastifyRequest } from "fastify";
import { getAuthSecret } from "./auth";
import type { JournalClient } from "./journal";
import { JournalSemanticError } from "./journal";

export interface GatewayRoutesOptions {
  journal: JournalClient;
  allowAdminRoutes: boolean;
}

type OpenApiSecurityRequirement = { [securityLabel: string]: readonly string[] };

const restrictedSecurity: readonly OpenApiSecurityRequirement[] = [
  { bearerAuth: [] },
  { syncHeader: [] },
];

const getContentType = (request: FastifyRequest): string =>
  String(request.headers["content-type"] || "")
    .split(";")[0]
    .trim()
    .toLowerCase();

const isSchemeContentType = (contentType: string): boolean =>
  contentType === "text/plain" || contentType === "application/scheme";

const isJsonContentType = (contentType: string): boolean =>
  contentType === "application/json" || contentType === "";

const requireAuth = (request: FastifyRequest): string => {
  const secret = getAuthSecret(request);
  if (!secret) throw new Error("Missing authentication header");
  return secret;
};

const escapeLispString = (value: string): string =>
  value.replace(/\\/g, "\\\\").replace(/"/g, '\\"');

const extractJsonArguments = (body: unknown): unknown => {
  if (body === undefined) {
    return undefined;
  }
  if (Array.isArray(body)) {
    return body;
  }
  if (!body || typeof body !== "object") {
    throw new Error(
      "JSON body must provide an argument object/array."
    );
  }
  const record = body as Record<string, unknown>;

  if ("arguments" in record) {
    throw new Error(
      "Gateway JSON bodies must provide operation arguments directly, not under an arguments wrapper."
    );
  }

  if ("function" in record || "authentication" in record) {
    throw new Error(
      "Gateway JSON bodies should provide only operation arguments."
    );
  }

  // Treat plain object bodies as direct keyword argument objects.
  return record;
};

const extractSchemeArguments = (body: unknown): string => {
  if (typeof body === "string") return body;
  if (Buffer.isBuffer(body)) return body.toString("utf8");
  throw new Error("Scheme requests must provide plain text argument expression body");
};

const buildSchemeExpression = (
  functionName: string,
  argsExpression: string,
  authSecret?: string
): string => {
  const parts = [`(function ${functionName})`, `(arguments ${argsExpression})`];
  if (authSecret) {
    parts.push(`(authentication "${escapeLispString(authSecret)}")`);
  }
  return `(${parts.join(" ")})`;
};

const buildControlSchemeExpression = (
  functionName: string,
  argsExpression: string,
  authSecret: string
): string => {
  const trimmed = argsExpression.trim();
  if (trimmed === "" || trimmed === "()") {
    return `(${functionName} "${escapeLispString(authSecret)}")`;
  }
  return `(${functionName} "${escapeLispString(authSecret)}" ${trimmed})`;
};

const callWithNegotiation = async (input: {
  request: FastifyRequest;
  journal: JournalClient;
  functionName: string;
  requiresAuth: boolean;
  control?: boolean;
}): Promise<unknown> => {
  const { request, journal, functionName, requiresAuth, control = false } = input;
  const authSecret = requiresAuth ? requireAuth(request) : undefined;
  const contentType = getContentType(request);

  if (isSchemeContentType(contentType)) {
    const argsExpression = extractSchemeArguments(request.body);
    const expression =
      control && authSecret
        ? buildControlSchemeExpression(functionName, argsExpression, authSecret)
        : buildSchemeExpression(functionName, argsExpression, authSecret);
    return control
      ? journal.callControlScheme({ expression, functionName })
      : journal.callScheme({ expression, functionName });
  }

  if (!isJsonContentType(contentType)) {
    throw new Error(
      "Unsupported content-type. Use application/json or text/plain (or application/scheme)."
    );
  }

  const args = extractJsonArguments(request.body);
  return control
    ? journal.callControlJson({
        functionName,
        args,
        authentication: authSecret,
      })
    : journal.callJson({
        functionName,
        args,
        authentication: authSecret,
      });
};

const generalAliases = {
  get: "get",
  set: "set!",
  pin: "pin!",
  unpin: "unpin!",
  batch: "batch!",
  "general-batch": "batch!",
  info: "info",
  synchronize: "synchronize",
  resolve: "resolve",
  trace: "trace",
  bridge: "bridge!",
  "general-bridge": "bridge!",
  config: "config",
  "set-secret": "*secret*",
} as const;

const controlAliases = {
  eval: "*eval*",
  call: "*call*",
  step: "*step*",
  "set-secret": "*set-secret*",
  "set-step": "*set-step*",
  "set-query": "*set-query*",
} as const;

const publicGeneralFunctions = new Set<string>(["synchronize", "trace"]);
const requestModeDescription =
  "JSON mode: Content-Type application/json with a keyword argument object. Legacy array arguments are also accepted for compatibility. Scheme mode: Content-Type text/plain or application/scheme with a raw Scheme arguments expression (the gateway wraps it into the full journal transport call).";

const generalOperationDocs: Record<string, { summary: string; description: string }> = {
  get: {
    summary: "Read staged state",
    description:
      "Calls general function `get`. Reads the current staged view only.",
  },
  set: {
    summary: "Stage a state write",
    description:
      "Calls general function `set!`. Writes to staged state; pair with control `step` for durable chain progression.",
  },
  pin: {
    summary: "Pin state/proof into permanent history",
    description:
      "Calls general function `pin!`. Keeps selected path/proof material across retention windows.",
  },
  unpin: {
    summary: "Remove a previously pinned path/proof",
    description:
      "Calls general function `unpin!`. Returns selected content to normal retention behavior.",
  },
  batch: {
    summary: "Execute multiple general requests in order",
    description:
      "Calls general function `batch!`. Accepts a `queries` list of request-shaped entries and executes them in order against the ledger.",
  },
  info: {
    summary: "Get public info",
    description:
      "Calls public general function `info`. Returns public node metadata.",
  },
  "general-batch": {
    summary: "Execute multiple general requests in order",
    description:
      "Legacy alias for `batch` that still calls general function `batch!`. Accepts a `queries` list of request-shaped entries and executes them in order against the ledger.",
  },
  synchronize: {
    summary: "Generate synchronization payload",
    description:
      "Calls public general function `synchronize`. Used by bridges/services to fetch digest/proof material for anti-entropy synchronization.",
  },
  resolve: {
    summary: "Resolve committed chain content",
    description:
      "Calls general function `resolve`. Reads indexed/committed content with optional pinned/proof metadata.",
  },
  trace: {
    summary: "Trace remote content against a chain index",
    description:
      "Calls public general function `trace`. Used by bridges/services to fetch a serialized remote path view from a committed chain index.",
  },
  bridge: {
    summary: "Register or update a bridge",
    description:
      "Calls general function `bridge!` with a bridge name and interface URL so the journal can wire its standard info/synchronize/trace handlers.",
  },
  "general-bridge": {
    summary: "Register a bridge using general defaults",
    description:
      "Legacy alias for `bridge` that still calls general function `bridge!` with the standard general interface URL pattern.",
  },
  config: {
    summary: "Read full node config",
    description:
      "Calls general function `config`. Includes private/runtime fields and should be treated as sensitive output.",
  },
  "set-secret": {
    summary: "Rotate the general interface secret",
    description:
      "Calls general function `*secret*`. Updates the shared interface secret used for restricted general operations.",
  },
};

const controlOperationDocs: Record<string, { summary: string; description: string }> = {
  eval: {
    summary: "Evaluate Scheme in admin context",
    description:
      "Calls control function `*eval*`. Highly privileged and intended for controlled operations only.",
  },
  call: {
    summary: "Invoke function against root object",
    description:
      "Calls control function `*call*`. Supports runtime-level updates and administrative transformations.",
  },
  step: {
    summary: "Execute full control step cycle",
    description:
      "Calls control function `*step*`. Triggers configured step handler pipeline.",
  },
  "set-secret": {
    summary: "Rotate admin/control secret",
    description:
      "Calls control function `*set-secret*`. Changes root control credential.",
  },
  "set-step": {
    summary: "Replace step handler",
    description:
      "Calls control function `*set-step*`. Updates control-plane step function at runtime.",
  },
  "set-query": {
    summary: "Replace query handler",
    description:
      "Calls control function `*set-query*`. Updates control-plane query function at runtime.",
  },
};

const argumentsBodySchema = {
  type: ["array", "object", "string"],
  description:
    "Object/array for JSON mode, or string for Scheme mode. Preferred JSON form uses keyword arguments as a direct object body.",
} as const;

export const gatewayRoutes: FastifyPluginAsync<GatewayRoutesOptions> = async (
  app,
  { journal, allowAdminRoutes }
) => {
  app.get("/", async (_request, reply) =>
    reply.type("text/html").send(`<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>Synchronic Gateway</title>
    <style>
      :root { color-scheme: light dark; }
      body {
        font-family: ui-sans-serif, system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif;
        margin: 0;
        padding: 2rem;
        max-width: 880px;
        line-height: 1.45;
      }
      code { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
      .card {
        border: 1px solid #c7c7c7;
        border-radius: 10px;
        padding: 1rem 1.2rem;
        margin: 1rem 0;
      }
      h1 { margin-top: 0; }
      ul { padding-left: 1.2rem; }
      a { text-decoration: none; }
      a:hover { text-decoration: underline; }
    </style>
  </head>
  <body>
    <h1>Synchronic Gateway</h1>
    <p>
      Web-facing gateway for Synchronic <code>general</code> and optional <code>control</code> operations.
      This service forwards operation calls to journal endpoints with header-based authentication.
    </p>
    <p>
      Use this service when you want stable, versioned HTTP endpoints that map directly to function-level journal calls
      while preserving authentication and request-shape consistency across clients.
    </p>

    <div class="card">
      <h2>API Docs</h2>
      <ul>
        <li><a href="/api/v1/docs">Swagger UI</a> (<code>/api/v1/docs</code>)</li>
      </ul>
      <p>
        Start there for route-by-route schemas, authentication requirements, and JSON/Scheme request-body guidance.
      </p>
    </div>

    <div class="card">
      <h2>Route Groups</h2>
      <ul>
        <li><code>/api/v1/general/*</code>: primary app-facing operations.</li>
        <li><code>/api/v1/control/*</code>: admin operations (only when enabled).</li>
        <li><code>/healthz</code> and <code>/readyz</code>: container and dependency probes.</li>
      </ul>
    </div>

    <div class="card">
      <h2>Common Patterns</h2>
      <ul>
        <li>Public reads: <code>GET /api/v1/general/size</code>, <code>GET /api/v1/general/info</code>.</li>
        <li>Restricted operations: send <code>Authorization: Bearer &lt;secret&gt;</code> or <code>X-Sync-Auth: &lt;secret&gt;</code>.</li>
        <li>Mutating calls are <code>POST</code> and accept either JSON or Scheme argument bodies.</li>
      </ul>
    </div>

    <div class="card">
      <h2>Health</h2>
      <ul>
        <li><a href="/healthz"><code>/healthz</code></a></li>
        <li><a href="/readyz"><code>/readyz</code></a></li>
      </ul>
    </div>

    <div class="card">
      <h2>Quick Start</h2>
      <p>Public size call:</p>
      <pre><code>curl http://127.0.0.1:8180/api/v1/general/size</code></pre>
      <p>Authenticated call:</p>
      <pre><code>curl -X POST http://127.0.0.1:8180/api/v1/general/get \\
  -H "Authorization: Bearer password" \\
  -H "Content-Type: application/json" \\
  -d '{"path":[["*state*","docs"]]}'</code></pre>
      <p>Scheme body call:</p>
      <pre><code>curl -X POST http://127.0.0.1:8180/api/v1/general/get \\
  -H "Authorization: Bearer password" \\
  -H "Content-Type: text/plain" \\
  -d '((path ((*state* docs))))'</code></pre>
      <p>Batch call:</p>
      <pre><code>curl -X POST http://127.0.0.1:8180/api/v1/general/batch \\
  -H "Authorization: Bearer password" \\
  -H "Content-Type: application/json" \\
  -d '{"queries":[{"function":"get","arguments":{"path":[["*state*","docs"]]}},{"function":"config"}]}'</code></pre>
    </div>
  </body>
</html>`)
  );

  app.get("/docs", async (_request, reply) => reply.redirect("/api/v1/docs"));
  app.get("/api/docs", async (_request, reply) => reply.redirect("/api/v1/docs"));

  app.get(
    "/healthz",
    {
      schema: {
        tags: ["Health"],
        summary: "Liveness probe",
        description:
          "Returns process liveness only. Does not verify journal connectivity.",
        response: {
          200: {
            type: "object",
            properties: { ok: { type: "boolean" } },
            required: ["ok"],
          },
        },
      },
    },
    async () => ({ ok: true })
  );

  app.get(
    "/readyz",
    {
      schema: {
        tags: ["Health"],
        summary: "Readiness probe",
        description:
          "Verifies the gateway can successfully execute a lightweight upstream call (`size`) against the journal JSON endpoint.",
        response: {
          200: {
            type: "object",
            properties: { ok: { type: "boolean" } },
            required: ["ok"],
          },
          503: {
            type: "object",
            properties: { ok: { type: "boolean" }, error: { type: "string" } },
            required: ["ok", "error"],
          },
        },
      },
    },
    async (_request, reply) => {
      try {
        await journal.callJson({ functionName: "size" });
        return { ok: true };
      } catch {
        return reply.code(503).send({ ok: false, error: "journal_unavailable" });
      }
    }
  );

  app.get(
    "/api/v1/general/size",
    {
      schema: {
        tags: ["General API (Public)"],
        summary: "Get ledger size (public)",
        description:
          "Public convenience endpoint for general function `size`. Useful for quick health/chain progression checks.",
      },
    },
    async () => journal.callJson({ functionName: "size" })
  );

  app.get(
    "/api/v1/general/info",
    {
      schema: {
        tags: ["General API (Public)"],
        summary: "Get public info (public)",
        description:
          "Public convenience endpoint for general function `info`. Returns public node metadata.",
      },
    },
    async () => journal.callJson({ functionName: "info" })
  );

  for (const [operation, functionName] of Object.entries(generalAliases)) {
    const requiresAuth = !publicGeneralFunctions.has(functionName);
    app.post(
      `/api/v1/general/${operation}`,
      {
        schema: {
          tags: [
            requiresAuth ? "General API (Restricted)" : "General API (Public)",
          ],
          summary:
            generalOperationDocs[operation]?.summary ||
            `General operation '${operation}'`,
          description: `${generalOperationDocs[operation]?.description || "General operation."} ${requestModeDescription}`,
          consumes: ["application/json", "text/plain", "application/scheme"],
          ...(requiresAuth ? { security: restrictedSecurity } : {}),
          body: argumentsBodySchema,
        },
      },
      async (request) =>
        callWithNegotiation({
          request,
          journal,
          functionName,
          requiresAuth,
        })
    );
  }

  if (allowAdminRoutes) {
    for (const [operation, functionName] of Object.entries(controlAliases)) {
      app.post(
        `/api/v1/control/${operation}`,
        {
          schema: {
            tags: ["Control API (Admin)"],
            summary:
              controlOperationDocs[operation]?.summary ||
              `Control operation '${operation}'`,
            description: `${controlOperationDocs[operation]?.description || "Control operation."} ${requestModeDescription}`,
            consumes: ["application/json", "text/plain", "application/scheme"],
            security: restrictedSecurity,
            body: argumentsBodySchema,
          },
        },
        async (request) =>
          callWithNegotiation({
            request,
            journal,
            functionName,
            requiresAuth: true,
            control: true,
          })
      );
    }
  }

  app.setErrorHandler((error, request, reply) => {
    const asRecord =
      typeof error === "object" && error !== null
        ? (error as Record<string, unknown>)
        : {};
    const errorMessage =
      error instanceof Error ? error.message : String(asRecord.message || error);

    // Fastify validation errors should be surfaced as 400, not generic gateway failures.
    if ("validation" in asRecord && asRecord.validation) {
      return reply.code(400).send({
        error: "invalid_request",
        message: errorMessage,
      });
    }
    // Unsupported media types can be raised by Fastify before handler logic runs.
    if (asRecord.code === "FST_ERR_CTP_INVALID_MEDIA_TYPE") {
      return reply.code(415).send({
        error: "unsupported_media_type",
        message: errorMessage,
      });
    }
    if (errorMessage.includes("Missing authentication header")) {
      return reply.code(401).send({
        error: "unauthorized",
        message: "Provide Authorization: Bearer <secret> or X-Sync-Auth header",
        hints: {
          hasAuthorizationHeader: Boolean(request.headers.authorization),
          hasSyncHeader: Boolean(request.headers["x-sync-auth"]),
        },
      });
    }
    if (errorMessage.includes("Unsupported content-type")) {
      return reply.code(415).send({
        error: "unsupported_media_type",
        message: errorMessage,
      });
    }
    if (
      errorMessage.includes("JSON body must provide") ||
      errorMessage.includes("JSON body must use") ||
      errorMessage.includes("Gateway JSON bodies must provide") ||
      errorMessage.includes("Gateway JSON bodies should provide") ||
      errorMessage.includes("Scheme requests must provide")
    ) {
      return reply.code(400).send({
        error: "invalid_request",
        message: errorMessage,
      });
    }
    if (error instanceof JournalSemanticError) {
      return reply.code(error.statusCode).send({
        error: error.code || "journal_error",
        message: error.message,
        details: error.details,
        source: "journal",
      });
    }
    request.log.error({ err: error }, "Unhandled gateway error");
    return reply.code(502).send({
      error: "gateway_error",
      message: errorMessage,
    });
  });
};
