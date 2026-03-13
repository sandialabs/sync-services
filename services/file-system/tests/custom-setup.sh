#!/bin/sh
set -eu

# Generator script: outputs the build-time setup script to stdout.
# Set SSL_CERT_FILE to inject a local PEM certificate without storing it in this repo.
CERT_SOURCE="${SSL_CERT_FILE:-}"

if [ -z "$CERT_SOURCE" ]; then
    cat <<'SCRIPT'
#!/bin/sh
set -eu
# No SSL_CERT_FILE provided; no custom certificate injected.
SCRIPT
    exit 0
fi

if [ ! -f "$CERT_SOURCE" ]; then
    echo "SSL_CERT_FILE does not exist: $CERT_SOURCE" >&2
    exit 1
fi

CERT_B64="$(base64 < "$CERT_SOURCE" | tr -d '\n')"

cat <<SCRIPT
#!/bin/sh
set -eu

CERT_PATH="/usr/local/share/ca-certificates/org-root-ca.crt"
mkdir -p /usr/local/share/ca-certificates

echo '$CERT_B64' | base64 -d > "\$CERT_PATH"

if command -v update-ca-certificates >/dev/null 2>&1; then
    update-ca-certificates
elif [ -f /etc/ssl/certs/ca-certificates.crt ]; then
    cat "\$CERT_PATH" >> /etc/ssl/certs/ca-certificates.crt
elif [ -f /etc/ssl/cert.pem ]; then
    cat "\$CERT_PATH" >> /etc/ssl/cert.pem
else
    echo "No known CA bundle path found. Wrote certificate to \$CERT_PATH only." >&2
fi
SCRIPT
