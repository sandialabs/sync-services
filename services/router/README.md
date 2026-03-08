# Synchronic Router

Nginx edge router for the Synchronic compose stack.

## Runtime Modes

- HTTP mode (default): serves routes on port `80`
- TLS mode (automatic): enabled when both cert and key files exist

Router checks these files at startup:

- `TLS_CERT_FILE` (default `/etc/nginx/certs/tls.crt`)
- `TLS_KEY_FILE` (default `/etc/nginx/certs/tls.key`)

If both files exist, router uses TLS config with:

- `80` for ACME HTTP-01 path + HTTPS redirect
- `443` for proxied application routes

If either file is missing, router uses HTTP-only config.
