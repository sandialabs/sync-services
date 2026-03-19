#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
IMAGE_TAG="${IMAGE_TAG:-sync-services/file-system:dev}"
CONTAINER_NAME="${CONTAINER_NAME:-sync-services-file-system-dev}"
DOCKER_PLATFORM="${DOCKER_PLATFORM:-}"
SYNC_FS_MODE="${SYNC_FS_MODE:-static-smb}"
SYNC_FS_BACKEND="${SYNC_FS_BACKEND:-}"
SYNC_FS_EXIT_AFTER_STARTUP="${SYNC_FS_EXIT_AFTER_STARTUP:-false}"
SYNC_FS_GATEWAY_BASE_URL="${SYNC_FS_GATEWAY_BASE_URL:-}"
SYNC_FS_GATEWAY_AUTH_TOKEN="${SYNC_FS_GATEWAY_AUTH_TOKEN:-}"
SYNC_FS_JOURNAL_JSON_URL="${SYNC_FS_JOURNAL_JSON_URL:-}"
CONTAINER_GATEWAY_BASE_URL="$SYNC_FS_GATEWAY_BASE_URL"
CONTAINER_JOURNAL_JSON_URL="$SYNC_FS_JOURNAL_JSON_URL"

if [ -z "$SYNC_FS_BACKEND" ] && [ -n "$SYNC_FS_GATEWAY_BASE_URL" ]; then
    if [ -z "$SYNC_FS_GATEWAY_AUTH_TOKEN" ]; then
        echo "FAIL: SYNC_FS_GATEWAY_AUTH_TOKEN is required when auto-selecting a gateway backend." >&2
        echo "Set SYNC_FS_BACKEND explicitly to override, or provide both SYNC_FS_GATEWAY_BASE_URL and SYNC_FS_GATEWAY_AUTH_TOKEN." >&2
        exit 1
    fi

    SYNC_FS_BACKEND="http-journal-stage"
fi

case "$CONTAINER_GATEWAY_BASE_URL" in
    http://127.0.0.1*|https://127.0.0.1*)
        CONTAINER_GATEWAY_BASE_URL="$(printf '%s' "$CONTAINER_GATEWAY_BASE_URL" | sed 's#://127\.0\.0\.1#://host.docker.internal#')"
        ;;
    http://localhost*|https://localhost*)
        CONTAINER_GATEWAY_BASE_URL="$(printf '%s' "$CONTAINER_GATEWAY_BASE_URL" | sed 's#://localhost#://host.docker.internal#')"
        ;;
esac

case "$CONTAINER_JOURNAL_JSON_URL" in
    http://127.0.0.1*|https://127.0.0.1*)
        CONTAINER_JOURNAL_JSON_URL="$(printf '%s' "$CONTAINER_JOURNAL_JSON_URL" | sed 's#://127\.0\.0\.1#://host.docker.internal#')"
        ;;
    http://localhost*|https://localhost*)
        CONTAINER_JOURNAL_JSON_URL="$(printf '%s' "$CONTAINER_JOURNAL_JSON_URL" | sed 's#://localhost#://host.docker.internal#')"
        ;;
esac

"$ROOT_DIR/tests/docker-build.sh"

docker rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true

if [ -n "$DOCKER_PLATFORM" ]; then
    docker run \
        --platform "$DOCKER_PLATFORM" \
        --name "$CONTAINER_NAME" \
        --detach \
        -p 445:445 \
        --add-host host.docker.internal:host-gateway \
        -v "$ROOT_DIR/tests:/workspace/tests" \
        -e SYNC_FS_Mode="$SYNC_FS_MODE" \
        ${SYNC_FS_BACKEND:+-e SYNC_FS_Backend="$SYNC_FS_BACKEND"} \
        -e SYNC_FS_ExitAfterStartup="$SYNC_FS_EXIT_AFTER_STARTUP" \
        ${CONTAINER_GATEWAY_BASE_URL:+-e SYNC_FS_GatewayBaseUrl="$CONTAINER_GATEWAY_BASE_URL"} \
        ${CONTAINER_JOURNAL_JSON_URL:+-e SYNC_FS_JournalJsonUrl="$CONTAINER_JOURNAL_JSON_URL"} \
        ${SYNC_FS_GATEWAY_AUTH_TOKEN:+-e SYNC_FS_GatewayAuthToken="$SYNC_FS_GATEWAY_AUTH_TOKEN"} \
        "$IMAGE_TAG" >/dev/null
else
    docker run \
        --name "$CONTAINER_NAME" \
        --detach \
        -p 445:445 \
        --add-host host.docker.internal:host-gateway \
        -v "$ROOT_DIR/tests:/workspace/tests" \
        -e SYNC_FS_Mode="$SYNC_FS_MODE" \
        ${SYNC_FS_BACKEND:+-e SYNC_FS_Backend="$SYNC_FS_BACKEND"} \
        -e SYNC_FS_ExitAfterStartup="$SYNC_FS_EXIT_AFTER_STARTUP" \
        ${CONTAINER_GATEWAY_BASE_URL:+-e SYNC_FS_GatewayBaseUrl="$CONTAINER_GATEWAY_BASE_URL"} \
        ${CONTAINER_JOURNAL_JSON_URL:+-e SYNC_FS_JournalJsonUrl="$CONTAINER_JOURNAL_JSON_URL"} \
        ${SYNC_FS_GATEWAY_AUTH_TOKEN:+-e SYNC_FS_GatewayAuthToken="$SYNC_FS_GATEWAY_AUTH_TOKEN"} \
        "$IMAGE_TAG" >/dev/null
fi

echo "Container started: $CONTAINER_NAME"
if [ -n "$SYNC_FS_BACKEND" ]; then
    echo "Backend: $SYNC_FS_BACKEND"
fi
if [ -n "$SYNC_FS_GATEWAY_BASE_URL" ] && [ "$CONTAINER_GATEWAY_BASE_URL" != "$SYNC_FS_GATEWAY_BASE_URL" ]; then
    echo "Gateway URL inside container: $CONTAINER_GATEWAY_BASE_URL"
fi
if [ -n "$SYNC_FS_JOURNAL_JSON_URL" ] && [ "$CONTAINER_JOURNAL_JSON_URL" != "$SYNC_FS_JOURNAL_JSON_URL" ]; then
    echo "Journal JSON URL inside container: $CONTAINER_JOURNAL_JSON_URL"
fi
echo "Inspect logs with: docker logs -f $CONTAINER_NAME"
