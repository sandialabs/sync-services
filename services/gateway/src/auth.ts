import type { FastifyRequest } from "fastify";

const BEARER_PREFIX = "Bearer ";

export const getAuthSecret = (request: FastifyRequest): string | null => {
  const authorization = request.headers.authorization;
  if (authorization) {
    const trimmed = authorization.trim();
    if (trimmed.toLowerCase().startsWith(BEARER_PREFIX.toLowerCase())) {
      return trimmed.slice(BEARER_PREFIX.length).trim();
    }
    // Fallback: accept raw Authorization token value.
    if (trimmed.length > 0) {
      return trimmed;
    }
  }

  const custom = request.headers["x-sync-auth"];
  if (typeof custom === "string" && custom.trim().length > 0) {
    return custom.trim();
  }

  return null;
};
