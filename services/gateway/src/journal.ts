import type { FastifyBaseLogger } from "fastify";

export interface JournalCall {
  functionName: string;
  args?: unknown[];
  authentication?: string;
}

export interface JournalClient {
  callJson(input: JournalCall): Promise<unknown>;
  callLisp(input: { expression: string; functionName: string }): Promise<unknown>;
}

export interface JournalClientOptions {
  debugForwarding?: boolean;
  debugForwardingIncludeAuth?: boolean;
}

const asErrorMessage = (value: unknown): string => {
  if (value instanceof Error) return value.message;
  if (typeof value === "string") return value;
  return "Unknown error";
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

export const createJournalClient = (
  journalJsonEndpoint: string,
  journalLispEndpoint: string,
  requestTimeoutMs: number,
  logger: FastifyBaseLogger,
  options: JournalClientOptions = {}
): JournalClient => ({
  async callJson(input: JournalCall): Promise<unknown> {
    const requestBody: Record<string, unknown> = {
      function: input.functionName,
    };

    if (input.args && input.args.length > 0) {
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
          upstream: journalJsonEndpoint,
          mode: "json",
          outboundBody: loggedBody,
        },
        "Gateway -> journal forward request"
      );
    }

    try {
      const response = await fetch(journalJsonEndpoint, {
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

      if (options.debugForwarding) {
        logger.info(
          {
            upstream: journalJsonEndpoint,
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

      return parsed;
    } catch (error) {
      const message = asErrorMessage(error);
      logger.error({ err: message }, "Journal call failed");
      throw new Error(`Failed to call journal: ${message}`);
    } finally {
      clearTimeout(timeout);
    }
  },
  async callLisp(input: { expression: string; functionName: string }): Promise<unknown> {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), requestTimeoutMs);
    const startedAt = Date.now();

    if (options.debugForwarding) {
      logger.info(
        {
          upstream: journalLispEndpoint,
          mode: "lisp",
          function: input.functionName,
          outboundExpression: input.expression,
        },
        "Gateway -> journal forward request"
      );
    }

    try {
      const response = await fetch(journalLispEndpoint, {
        method: "POST",
        headers: { "content-type": "text/plain; charset=utf-8" },
        body: input.expression,
        signal: controller.signal,
      });

      const text = await response.text();
      const parsed = parseResponse(text);

      if (options.debugForwarding) {
        logger.info(
          {
            upstream: journalLispEndpoint,
            mode: "lisp",
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

      return parsed;
    } catch (error) {
      const message = asErrorMessage(error);
      logger.error({ err: message }, "Journal call failed");
      throw new Error(`Failed to call journal: ${message}`);
    } finally {
      clearTimeout(timeout);
    }
  },
});
