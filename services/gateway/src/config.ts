export interface GatewayConfig {
  host: string;
  port: number;
  journalJsonEndpoint: string;
  journalSchemeEndpoint: string;
  controlJsonEndpoint: string;
  controlSchemeEndpoint: string;
  requestTimeoutMs: number;
  allowAdminRoutes: boolean;
  debugForwarding: boolean;
  debugForwardingIncludeAuth: boolean;
}

const toNumber = (value: string | undefined, fallback: number): number => {
  if (!value) return fallback;
  const parsed = Number(value);
  return Number.isFinite(parsed) ? parsed : fallback;
};

const toBoolean = (value: string | undefined, fallback: boolean): boolean => {
  if (!value) return fallback;
  const normalized = value.trim().toLowerCase();
  if (["1", "true", "yes", "on"].includes(normalized)) return true;
  if (["0", "false", "no", "off"].includes(normalized)) return false;
  return fallback;
};

export const getConfig = (): GatewayConfig => ({
  host: process.env.HOST || "0.0.0.0",
  port: toNumber(process.env.PORT, 8180),
  journalJsonEndpoint:
    process.env.JOURNAL_JSON_ENDPOINT || "http://127.0.0.1:8192/interface/json",
  journalSchemeEndpoint:
    process.env.JOURNAL_SCHEME_ENDPOINT || "http://127.0.0.1:8192/interface",
  controlJsonEndpoint:
    process.env.CONTROL_JSON_ENDPOINT || "http://127.0.0.1:8192/interface/json",
  controlSchemeEndpoint:
    process.env.CONTROL_SCHEME_ENDPOINT || "http://127.0.0.1:8192/interface",
  requestTimeoutMs: toNumber(process.env.REQUEST_TIMEOUT_MS, 30000),
  allowAdminRoutes: toBoolean(process.env.ALLOW_ADMIN_ROUTES, false),
  debugForwarding: toBoolean(process.env.DEBUG_FORWARDING, false),
  debugForwardingIncludeAuth: toBoolean(
    process.env.DEBUG_FORWARDING_INCLUDE_AUTH,
    false
  ),
});
