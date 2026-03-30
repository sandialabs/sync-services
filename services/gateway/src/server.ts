import Fastify from "fastify";
import fastifySwagger from "@fastify/swagger";
import fastifySwaggerUi from "@fastify/swagger-ui";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { getConfig } from "./config";
import { createJournalClient } from "./journal";
import { instrumentGatewayRequests } from "./metrics";
import { gatewayRoutes } from "./routes";

const apiDescription = `
Versioned, function-oriented HTTP gateway over Synchronic journal transport endpoints.

Start here:
- Use GET routes for simple read-only checks: /api/v1/general/size and /api/v1/general/info.
- Use POST /api/v1/general/{operation} for function calls that take arguments.
- Use POST /api/v1/general/batch for ordered multi-request workflows under one authenticated call.
- For restricted routes, provide either Authorization bearer token or X-Sync-Auth header.

Request bodies:
- JSON mode uses application/json with keyword arguments as a direct object body.
- Scheme mode uses text/plain or application/scheme with a raw Scheme arguments expression.

Operational notes:
- General routes are intended for normal app integrations.
- Root routes are admin-level and only exposed when ALLOW_ADMIN_ROUTES=1.
`.trim();

const swaggerUiTheme = `
.swagger-ui .topbar { background-color: #101318; border-bottom: 1px solid #2c3440; }
.swagger-ui .topbar .topbar-wrapper img { max-height: 36px; width: auto; }
.swagger-ui .topbar .topbar-wrapper a.link {
  display: inline-flex;
  align-items: center;
  gap: 0.6rem;
}
.swagger-ui .topbar .topbar-wrapper a.link::after {
  content: "Synchronic Web Gateway";
  color: #ffffff;
  font-weight: 700;
  font-size: 1rem;
  letter-spacing: 0.01em;
  white-space: nowrap;
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
}
.swagger-ui a { color: #0076a9; }
.swagger-ui .info .title { color: #1f2937; }
.swagger-ui .btn.authorize { border-color: #00add0; color: #00add0; }
.swagger-ui .btn.authorize svg { fill: #00add0; }
.swagger-ui .opblock.opblock-get { border-color: #0076a9; background: #f2f9fc; }
.swagger-ui .opblock.opblock-get .opblock-summary-method { background: #0076a9; }
.swagger-ui .opblock.opblock-post { border-color: #008e74; background: #f1fbf8; }
.swagger-ui .opblock.opblock-post .opblock-summary-method { background: #008e74; }
.swagger-ui .opblock-tag { color: #002b4c; }
.swagger-ui .scheme-container { box-shadow: none; }

/* Keep toolbar/search usable and clean */
.swagger-ui .topbar .download-url-wrapper { display: flex; align-items: center; gap: 0.5rem; }
.swagger-ui .topbar .download-url-wrapper input[type=text] { min-width: 18rem; }

/* Dark-mode friendly docs surface */
@media (prefers-color-scheme: dark) {
  .swagger-ui { color: #d4d4d4; }
  .swagger-ui .info .title,
  .swagger-ui .opblock-tag,
  .swagger-ui .info p,
  .swagger-ui .info li,
  .swagger-ui .info h1,
  .swagger-ui .info h2,
  .swagger-ui .info h3,
  .swagger-ui .info h4 { color: #d4d4d4; }
  .swagger-ui .scheme-container,
  .swagger-ui .opblock-description-wrapper,
  .swagger-ui .opblock-body,
  .swagger-ui .responses-inner h4,
  .swagger-ui .responses-inner h5 { background: #1f252b; color: #d4d4d4; }
  .swagger-ui .topbar { background-color: #101318; border-bottom-color: #2c3440; }
  .swagger-ui .opblock.opblock-get { background: #163041; border-color: #0076a9; }
  .swagger-ui .opblock.opblock-post { background: #16382f; border-color: #008e74; }
  .swagger-ui table thead tr td,
  .swagger-ui table thead tr th,
  .swagger-ui .parameter__name,
  .swagger-ui .parameter__type,
  .swagger-ui .response-col_status,
  .swagger-ui .response-col_description { color: #d4d4d4; }
}
`;

const readGatewayLogo = (): Buffer | null => {
  try {
    return readFileSync(resolve(__dirname, "..", "assets", "sync-web-logo.png"));
  } catch {
    return null;
  }
};

const main = async (): Promise<void> => {
  const config = getConfig();
  const app = Fastify({
    logger: true,
    bodyLimit: 64 * 1024 * 1024,
  });
  instrumentGatewayRequests(app);
  const logo = readGatewayLogo();

  app.addContentTypeParser("text/plain", { parseAs: "string" }, (_req, body, done) =>
    done(null, body)
  );
  app.addContentTypeParser("application/scheme", { parseAs: "string" }, (_req, body, done) =>
    done(null, body)
  );

  await app.register(fastifySwagger, {
    openapi: {
      info: {
        title: "Synchronic Gateway API",
        description: apiDescription,
        version: "1.0.0",
      },
      servers: [{ url: "/" }],
      tags: [
        {
          name: "Health",
          description: "Process and upstream readiness endpoints.",
        },
        {
          name: "General API (Public)",
          description: "Public general operations that do not require auth.",
        },
        {
          name: "General API (Restricted)",
          description:
            "Authenticated general operations for reads/writes, bridge config, and stepping.",
        },
        {
          name: "Root API (Admin)",
          description:
            "High-privilege root-plane operations for runtime management and updates.",
        },
      ],
      components: {
        securitySchemes: {
          bearerAuth: {
            type: "http",
            scheme: "bearer",
            bearerFormat: "Password",
          },
          syncHeader: {
            type: "apiKey",
            in: "header",
            name: "X-Sync-Auth",
          },
        },
      },
    },
  });

  await app.register(fastifySwaggerUi, {
    routePrefix: "/api/v1/docs",
    staticCSP: true,
    ...(logo
      ? {
          logo: {
            type: "image/png",
            content: logo,
            href: "/",
            target: "_self",
          },
        }
      : {}),
    theme: {
      css: [{ filename: "sync-gateway-swagger-theme.css", content: swaggerUiTheme }],
    },
    uiConfig: {
      docExpansion: "list",
      deepLinking: true,
      persistAuthorization: true,
      displayRequestDuration: true,
    },
  });

  const journal = createJournalClient(
    config.journalJsonEndpoint,
    config.journalSchemeEndpoint,
    config.rootJsonEndpoint,
    config.rootSchemeEndpoint,
    config.requestTimeoutMs,
    app.log,
    {
      debugForwarding: config.debugForwarding,
      debugForwardingIncludeAuth: config.debugForwardingIncludeAuth,
    }
  );

  await app.register(gatewayRoutes, {
    journal,
    allowAdminRoutes: config.allowAdminRoutes,
  });

  await app.listen({
    host: config.host,
    port: config.port,
  });
};

main().catch((error) => {
  // eslint-disable-next-line no-console
  console.error(error);
  process.exit(1);
});
