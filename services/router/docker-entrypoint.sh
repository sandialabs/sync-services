#!/bin/sh
set -eu

CERT_FILE="${TLS_CERT_FILE:-/etc/nginx/certs/tls.crt}"
KEY_FILE="${TLS_KEY_FILE:-/etc/nginx/certs/tls.key}"
JOURNAL_HOST="${ROUTER_JOURNAL_HOST:-journal}"
GATEWAY_HOST="${ROUTER_GATEWAY_HOST:-gateway}"
EXPLORER_HOST="${ROUTER_EXPLORER_HOST:-explorer}"
WORKBENCH_HOST="${ROUTER_WORKBENCH_HOST:-workbench}"

cat > /etc/nginx/includes/nginx.routes.inc <<EOF
location /interface {
    proxy_pass http://${JOURNAL_HOST}/interface;
}

location /api/ {
    proxy_pass http://${GATEWAY_HOST};
}

location = /docs {
    proxy_pass http://${GATEWAY_HOST}/docs;
}

location = /healthz {
    proxy_pass http://${GATEWAY_HOST}/healthz;
}

location = /readyz {
    proxy_pass http://${GATEWAY_HOST}/readyz;
}

location = /metrics {
    proxy_pass http://${GATEWAY_HOST}/metrics;
}

location /explorer {
    proxy_pass http://${EXPLORER_HOST}/;
}

location /workbench {
    proxy_pass http://${WORKBENCH_HOST}/;
}
EOF

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
