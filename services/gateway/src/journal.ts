import type { FastifyBaseLogger } from "fastify";
import { recordJournalCall } from "./metrics";

export interface JournalCall {
  functionName: string;
  args?: unknown;
  authentication?: string;
}

export interface JournalClient {
  callJson(input: JournalCall): Promise<unknown>;
  callScheme(input: { expression: string; functionName: string }): Promise<unknown>;
  callControlJson(input: JournalCall): Promise<unknown>;
  callControlScheme(input: { expression: string; functionName: string }): Promise<unknown>;
}

export interface JournalClientOptions {
  debugForwarding?: boolean;
  debugForwardingIncludeAuth?: boolean;
}

export class JournalSemanticError extends Error {
  readonly statusCode: number;
  readonly code: string;
  readonly details: unknown;

  constructor(input: {
    code: string;
    message: string;
    details?: unknown;
    statusCode?: number;
  }) {
    super(input.message);
    this.name = "JournalSemanticError";
    this.code = input.code;
    this.details = input.details;
    this.statusCode = input.statusCode ?? 400;
  }
}

const asErrorMessage = (value: unknown): string => {
  if (value instanceof Error) return value.message;
  if (typeof value === "string") return value;
  return "Unknown error";
};

const asRecord = (value: unknown): Record<string, unknown> | null =>
  value && typeof value === "object" && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : null;

const parseQuotedSymbol = (value: unknown): string | null => {
  const record = asRecord(value);
  if (!record) return null;
  const quoted = record["*type/quoted*"] ?? record["*type/quoted"];
  return typeof quoted === "string" ? quoted : null;
};

const parseSchemeString = (value: unknown): string | null => {
  const record = asRecord(value);
  if (!record) return null;
  const wrapped = record["*type/string*"];
  return typeof wrapped === "string" ? wrapped : null;
};

const toSemanticError = (value: unknown): JournalSemanticError | null => {
  if (!Array.isArray(value) || value.length < 3 || value[0] !== "error") return null;
  const code = parseQuotedSymbol(value[1]) || "journal_error";
  const message = parseSchemeString(value[2]) || "Journal returned an error";
  return new JournalSemanticError({
    code,
    message,
    details: value,
    statusCode: 400,
  });
};

const cloneBody = (body: Record<string, unknown>): Record<string, unknown> =>
  JSON.parse(JSON.stringify(body)) as Record<string, unknown>;

const redactAuth = (body: Record<string, unknown>): Record<string, unknown> => {
  const cloned = cloneBody(body);
  if ("authentication" in cloned) {
    cloned.authentication = "***REDACTED***";
  }
  return cloned;
};

const preview = (value: unknown, maxLen = 1200): string => {
  let serialized: string;
  try {
    serialized = JSON.stringify(value);
  } catch {
    serialized = String(value);
  }
  if (serialized.length <= maxLen) return serialized;
  return `${serialized.slice(0, maxLen)}...<truncated>`;
};

const parseResponse = (text: string): unknown => {
  try {
    return JSON.parse(text);
  } catch {
    return text;
  }
};

const buildControlJsonBody = (input: JournalCall): unknown[] => {
  const body: unknown[] = [input.functionName];
  if (input.authentication) {
    body.push({ "*type/string*": input.authentication });
  }

  if (input.args === undefined) {
    return body;
  }

  if (Array.isArray(input.args)) {
    body.push(...input.args);
    return body;
  }

  body.push(input.args);
  return body;
};

export const createJournalClient = (
  journalJsonEndpoint: string,
  journalSchemeEndpoint: string,
  controlJsonEndpoint: string,
  controlSchemeEndpoint: string,
  requestTimeoutMs: number,
  logger: FastifyBaseLogger,
  options: JournalClientOptions = {}
): JournalClient => {
  const callJsonEndpoint = async (
    endpoint: string,
    input: JournalCall
  ): Promise<unknown> => {
    const requestBody: Record<string, unknown> = {
      function: input.functionName,
    };

    if (input.args !== undefined) {
      requestBody.arguments = input.args;
    }

    if (input.authentication) {
      requestBody.authentication = { "*type/string*": input.authentication };
    }

    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), requestTimeoutMs);
    const startedAt = Date.now();

    if (options.debugForwarding) {
      const loggedBody = options.debugForwardingIncludeAuth
        ? cloneBody(requestBody)
        : redactAuth(requestBody);
      logger.info(
        {
          upstream: endpoint,
          mode: "json",
          outboundBody: loggedBody,
        },
        "Gateway -> journal forward request"
      );
    }

    try {
      const response = await fetch(endpoint, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(requestBody),
        signal: controller.signal,
      });

      const text = await response.text();
      let parsed: unknown = text;

      try {
        parsed = JSON.parse(text);
      } catch {
        // Keep raw text as-is if upstream did not return JSON.
      }

      const semanticError = toSemanticError(parsed);
      if (semanticError) {
        logger.warn(
          {
            statusCode: response.status,
            code: semanticError.code,
            message: semanticError.message,
          },
          "Journal returned semantic error payload"
        );
        throw semanticError;
      }

      if (options.debugForwarding) {
        logger.info(
          {
            upstream: endpoint,
            mode: "json",
            statusCode: response.status,
            durationMs: Date.now() - startedAt,
            responsePreview: preview(parsed),
          },
          "Journal -> gateway forward response"
        );
      }

      if (!response.ok) {
        logger.error(
          {
            statusCode: response.status,
            body: parsed,
          },
          "Upstream journal returned non-OK response"
        );
        throw new Error(`Journal error (${response.status})`);
      }

      recordJournalCall({
        mode: "json",
        functionName: input.functionName,
        result: "success",
        durationSeconds: (Date.now() - startedAt) / 1000,
      });
      return parsed;
    } catch (error) {
      recordJournalCall({
        mode: "json",
        functionName: input.functionName,
        result: "error",
        durationSeconds: (Date.now() - startedAt) / 1000,
      });
      if (error instanceof JournalSemanticError) {
        throw error;
      }
      const message = asErrorMessage(error);
      logger.error({ err: message }, "Journal call failed");
      throw new Error(`Failed to call journal: ${message}`);
    } finally {
      clearTimeout(timeout);
    }
  };

  const callSchemeEndpoint = async (
    endpoint: string,
    input: { expression: string; functionName: string }
  ): Promise<unknown> => {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), requestTimeoutMs);
    const startedAt = Date.now();

    if (options.debugForwarding) {
      logger.info(
          {
            upstream: endpoint,
            mode: "scheme",
            function: input.functionName,
            outboundExpression: input.expression,
          },
        "Gateway -> journal forward request"
      );
    }

    try {
      const response = await fetch(endpoint, {
        method: "POST",
        headers: { "content-type": "text/plain; charset=utf-8" },
        body: input.expression,
        signal: controller.signal,
      });

      const text = await response.text();
      const parsed = parseResponse(text);

      const semanticError = toSemanticError(parsed);
      if (semanticError) {
        logger.warn(
          {
            statusCode: response.status,
            code: semanticError.code,
            message: semanticError.message,
          },
          "Journal returned semantic error payload"
        );
        throw semanticError;
      }

      if (options.debugForwarding) {
        logger.info(
          {
            upstream: endpoint,
            mode: "scheme",
            function: input.functionName,
            statusCode: response.status,
            durationMs: Date.now() - startedAt,
            responsePreview: preview(parsed),
          },
          "Journal -> gateway forward response"
        );
      }

      if (!response.ok) {
        logger.error(
          {
            statusCode: response.status,
            body: parsed,
          },
          "Upstream journal returned non-OK response"
        );
        throw new Error(`Journal error (${response.status})`);
      }

      recordJournalCall({
        mode: "scheme",
        functionName: input.functionName,
        result: "success",
        durationSeconds: (Date.now() - startedAt) / 1000,
      });
      return parsed;
    } catch (error) {
      recordJournalCall({
        mode: "scheme",
        functionName: input.functionName,
        result: "error",
        durationSeconds: (Date.now() - startedAt) / 1000,
      });
      if (error instanceof JournalSemanticError) {
        throw error;
      }
      const message = asErrorMessage(error);
      logger.error({ err: message }, "Journal call failed");
      throw new Error(`Failed to call journal: ${message}`);
    } finally {
      clearTimeout(timeout);
    }
  };

  return {
    async callJson(input: JournalCall): Promise<unknown> {
      return callJsonEndpoint(journalJsonEndpoint, input);
    },
    async callScheme(input: {
      expression: string;
      functionName: string;
    }): Promise<unknown> {
      return callSchemeEndpoint(journalSchemeEndpoint, input);
    },
    async callControlJson(input: JournalCall): Promise<unknown> {
      const requestBody = buildControlJsonBody(input);
      const controller = new AbortController();
      const timeout = setTimeout(() => controller.abort(), requestTimeoutMs);
      const startedAt = Date.now();

      if (options.debugForwarding) {
        logger.info(
          {
            upstream: controlJsonEndpoint,
            mode: "json",
            outboundBody: options.debugForwardingIncludeAuth
              ? requestBody
              : [
                  input.functionName,
                  input.authentication ? "***REDACTED***" : undefined,
                  ...(Array.isArray(input.args)
                    ? input.args
                    : input.args === undefined
                      ? []
                      : [input.args]),
                ].filter((x) => x !== undefined),
          },
          "Gateway -> journal forward request"
        );
      }

      try {
        const response = await fetch(controlJsonEndpoint, {
          method: "POST",
          headers: { "content-type": "application/json" },
          body: JSON.stringify(requestBody),
          signal: controller.signal,
        });

        const text = await response.text();
        const parsed = parseResponse(text);
        const semanticError = toSemanticError(parsed);
        if (semanticError) {
          logger.warn(
            {
              statusCode: response.status,
              code: semanticError.code,
              message: semanticError.message,
            },
            "Journal returned semantic error payload"
          );
          throw semanticError;
        }

        if (options.debugForwarding) {
          logger.info(
            {
              upstream: controlJsonEndpoint,
              mode: "json",
              statusCode: response.status,
              durationMs: Date.now() - startedAt,
              responsePreview: preview(parsed),
            },
            "Journal -> gateway forward response"
          );
        }

        if (!response.ok) {
          logger.error(
            {
              statusCode: response.status,
              body: parsed,
            },
            "Upstream journal returned non-OK response"
          );
          throw new Error(`Journal error (${response.status})`);
        }

        recordJournalCall({
          mode: "json",
          functionName: input.functionName,
          result: "success",
          durationSeconds: (Date.now() - startedAt) / 1000,
        });
        return parsed;
      } catch (error) {
        recordJournalCall({
          mode: "json",
          functionName: input.functionName,
          result: "error",
          durationSeconds: (Date.now() - startedAt) / 1000,
        });
        if (error instanceof JournalSemanticError) {
          throw error;
        }
        const message = asErrorMessage(error);
        logger.error({ err: message }, "Journal call failed");
        throw new Error(`Failed to call journal: ${message}`);
      } finally {
        clearTimeout(timeout);
      }
    },
    async callControlScheme(input: {
      expression: string;
      functionName: string;
    }): Promise<unknown> {
      return callSchemeEndpoint(controlSchemeEndpoint, input);
    },
  };
};
