#!/bin/sh
set -eu

CERT_FILE="${TLS_CERT_FILE:-/etc/nginx/certs/tls.crt}"
KEY_FILE="${TLS_KEY_FILE:-/etc/nginx/certs/tls.key}"

if [ -f "$CERT_FILE" ] \
    && [ -f "$KEY_FILE" ] \
    && grep -q "BEGIN CERTIFICATE" "$CERT_FILE" \
    && grep -Eq "BEGIN (EC |RSA |)PRIVATE KEY" "$KEY_FILE"; then
    cp /etc/nginx/templates/nginx.tls.conf /etc/nginx/conf.d/default.conf
    echo "Router mode: TLS (cert: $CERT_FILE, key: $KEY_FILE)"
else
    cp /etc/nginx/templates/nginx.http.conf /etc/nginx/conf.d/default.conf
    echo "Router mode: HTTP (TLS cert/key not found)"
fi

exec "$@"
