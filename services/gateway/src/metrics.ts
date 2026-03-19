import type { FastifyInstance, FastifyReply, FastifyRequest } from "fastify";
import {
  Registry,
  Counter,
  Gauge,
  Histogram,
  collectDefaultMetrics,
} from "prom-client";

const registry = new Registry();

collectDefaultMetrics({ register: registry });

const httpRequestsTotal = new Counter({
  name: "sync_gateway_requests_total",
  help: "Total HTTP requests handled by the gateway.",
  labelNames: ["method", "route", "status_code"] as const,
  registers: [registry],
});

const httpRequestDurationSeconds = new Histogram({
  name: "sync_gateway_request_duration_seconds",
  help: "HTTP request duration in seconds for gateway routes.",
  labelNames: ["method", "route", "status_code"] as const,
  buckets: [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 30],
  registers: [registry],
});

const httpInFlightRequests = new Gauge({
  name: "sync_gateway_in_flight_requests",
  help: "Number of in-flight HTTP requests currently being handled by the gateway.",
  registers: [registry],
});

const journalRequestsTotal = new Counter({
  name: "sync_gateway_journal_requests_total",
  help: "Total upstream journal requests issued by the gateway.",
  labelNames: ["mode", "function_name", "result"] as const,
  registers: [registry],
});

const journalRequestDurationSeconds = new Histogram({
  name: "sync_gateway_journal_request_duration_seconds",
  help: "Duration in seconds of upstream journal requests issued by the gateway.",
  labelNames: ["mode", "function_name", "result"] as const,
  buckets: [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 30],
  registers: [registry],
});

const getRouteLabel = (request: FastifyRequest): string => {
  const route = typeof request.routeOptions?.url === "string" ? request.routeOptions.url : null;
  if (route) {
    return route;
  }

  const rawUrl = typeof request.url === "string" ? request.url : "";
  if (!rawUrl) {
    return "unknown";
  }

  const queryIndex = rawUrl.indexOf("?");
  return queryIndex >= 0 ? rawUrl.slice(0, queryIndex) : rawUrl;
};

export const instrumentGatewayRequests = (app: FastifyInstance): void => {
  app.addHook("onRequest", async (request) => {
    httpInFlightRequests.inc();
    (request as FastifyRequest & {
      __syncGatewayMetricsStartNs?: bigint;
    }).__syncGatewayMetricsStartNs = process.hrtime.bigint();
  });

  app.addHook("onResponse", async (request, reply) => {
    const startedAt = (request as FastifyRequest & {
      __syncGatewayMetricsStartNs?: bigint;
    }).__syncGatewayMetricsStartNs;
    httpInFlightRequests.dec();

    if (!startedAt) {
      return;
    }

    const durationSeconds = Number(process.hrtime.bigint() - startedAt) / 1_000_000_000;
    const labels = {
      method: request.method,
      route: getRouteLabel(request),
      status_code: String(reply.statusCode),
    };

    httpRequestsTotal.inc(labels);
    httpRequestDurationSeconds.observe(labels, durationSeconds);
  });

  app.get("/metrics", async (_request: FastifyRequest, reply: FastifyReply) => {
    reply.header("content-type", registry.contentType);
    return registry.metrics();
  });
};

export const recordJournalCall = (input: {
  mode: "json" | "lisp";
  functionName: string;
  result: "success" | "error";
  durationSeconds: number;
}): void => {
  const labels = {
    mode: input.mode,
    function_name: input.functionName,
    result: input.result,
  };

  journalRequestsTotal.inc(labels);
  journalRequestDurationSeconds.observe(labels, input.durationSeconds);
};
