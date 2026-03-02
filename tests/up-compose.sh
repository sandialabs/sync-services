#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
COMPOSE_DIR="$ROOT_DIR/compose/general"
LOCAL_OVERRIDE_FILE="$ROOT_DIR/tests/docker-compose.local.yml"
LOCAL_UI_OVERRIDE_FILE="$ROOT_DIR/tests/docker-compose.local-ui.yml"
PORT="${PORT:-8192}"
SECRET="${SECRET:-password}"
PERIOD="${PERIOD:-2}"
WINDOW="${WINDOW:-1024}"
LOCAL_LISP_PATH="${LOCAL_LISP_PATH:-}"
WIPE_VOLUMES="${WIPE_VOLUMES:-0}"

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
    if [ "$WIPE_VOLUMES" = "1" ]; then
        dc down -v --remove-orphans >/dev/null 2>&1
    else
        dc down --remove-orphans >/dev/null 2>&1
    fi
}

on_interrupt() {
    cleanup
    exit 130
}

trap cleanup EXIT
trap on_interrupt INT TERM

export SECRET PERIOD WINDOW PORT LOCAL_LISP_PATH WIPE_VOLUMES

if [ "$WIPE_VOLUMES" = "1" ]; then
    echo "Starting compose stack on port $PORT (Ctrl+C to stop, volumes will be removed)..."
else
    echo "Starting compose stack on port $PORT (Ctrl+C to stop, volumes preserved)..."
fi
dc up --build
