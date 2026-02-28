#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
COMPOSE_DIR="$ROOT_DIR/compose/general"
LOCAL_OVERRIDE_FILE="$ROOT_DIR/tests/docker-compose.local.yml"
LOCAL_UI_OVERRIDE_FILE="$ROOT_DIR/tests/docker-compose.local-ui.yml"
PORT="${PORT:-8192}"
SECRET="${SECRET:-smoke-test-secret}"
PERIOD="${PERIOD:-2}"
WINDOW="${WINDOW:-64}"
LOCAL_LISP_PATH="${LOCAL_LISP_PATH:-}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-60}"
CONNECT_TIMEOUT_SECONDS="${CONNECT_TIMEOUT_SECONDS:-2}"
REQUEST_TIMEOUT_SECONDS="${REQUEST_TIMEOUT_SECONDS:-5}"

COMPOSE_ARGS="-f $COMPOSE_DIR/docker-compose.yml"
if [ -n "$LOCAL_LISP_PATH" ]; then
    COMPOSE_ARGS="$COMPOSE_ARGS -f $LOCAL_OVERRIDE_FILE"
    COMPOSE_ARGS="$COMPOSE_ARGS -f $LOCAL_UI_OVERRIDE_FILE"
fi

dc() {
    # shellcheck disable=SC2086
    docker compose $COMPOSE_ARGS "$@"
}

cleanup() {
    set +e
    dc down -v --remove-orphans >/dev/null 2>&1
}
on_interrupt() {
    cleanup
    exit 130
}

trap cleanup EXIT
trap on_interrupt INT TERM

wait_for_http() {
    url="$1"
    elapsed=0

    while [ "$elapsed" -lt "$TIMEOUT_SECONDS" ]; do
        if curl -fsS \
          --connect-timeout "$CONNECT_TIMEOUT_SECONDS" \
          --max-time "$REQUEST_TIMEOUT_SECONDS" \
          "$url" >/dev/null 2>&1; then
            return 0
        fi
        sleep 2
        elapsed=$((elapsed + 2))
    done

    echo "Timed out waiting for $url"
    return 1
}

api_post() {
    body="$1"
    curl -fsS \
      -H "Content-Type: application/json" \
      -d "$body" \
      "http://127.0.0.1:$PORT/interface/json"
}

echo "Starting compose stack on port $PORT..."
export SECRET PERIOD WINDOW PORT LOCAL_LISP_PATH
dc up -d --build

echo "Waiting for routes..."
wait_for_http "http://127.0.0.1:$PORT/explorer/"
wait_for_http "http://127.0.0.1:$PORT/workbench/"

echo "Running API smoke checks..."
size_response="$(api_post '{"function":"size"}' | tr -d '[:space:]')"
case "$size_response" in
    ''|*[!0-9]*)
        echo "FAIL: size response is not a number: $size_response"
        exit 1
        ;;
esac

config_response="$(api_post "{\"function\":\"configuration\",\"authentication\":\"$SECRET\"}")"
if [ -z "$config_response" ]; then
    echo "FAIL: configuration response is empty"
    exit 1
fi

echo "PASS: compose stack is healthy and journal API responded."
