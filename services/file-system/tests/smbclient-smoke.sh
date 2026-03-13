#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
HOST="${SMB_HOST:-127.0.0.1}"
SHARE_NAME="${SHARE_NAME:-sync}"
SMBCLIENT_BIN="${SMBCLIENT_BIN:-smbclient}"
AUTH_MODE="${AUTH_MODE:-guest}"
USER_NAME="${USER_NAME:-}"
PASSWORD="${PASSWORD:-}"
WORK_DIR="${WORK_DIR:-/tmp/sync-fs-smbclient-smoke}"
WAIT_TIMEOUT_SECONDS="${WAIT_TIMEOUT_SECONDS:-30}"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

cleanup() {
    rm -rf "$WORK_DIR"
}

trap cleanup EXIT INT TERM

UPLOAD_FILE="$WORK_DIR/upload.txt"
DOWNLOAD_FILE="$WORK_DIR/hello.txt"

printf "sync fs smoke upload\n" > "$UPLOAD_FILE"

build_auth_args() {
    if [ "$AUTH_MODE" = "guest" ]; then
        printf '%s\n' "-N"
        return 0
    fi

    if [ -z "$USER_NAME" ]; then
        echo "USER_NAME is required when AUTH_MODE is not guest" >&2
        exit 1
    fi

    printf '%s\n' "-U"
    printf '%s\n' "${USER_NAME}%${PASSWORD}"
}

run_command() {
    command_text="$1"
    auth_args="$(build_auth_args)"
    # shellcheck disable=SC2086
    "$SMBCLIENT_BIN" "//$HOST/$SHARE_NAME" $auth_args -c "$command_text"
}

wait_for_share() {
    elapsed=0
    while [ "$elapsed" -lt "$WAIT_TIMEOUT_SECONDS" ]; do
        if run_command "ls" >/dev/null 2>&1; then
            return 0
        fi

        sleep 2
        elapsed=$((elapsed + 2))
    done

    echo "FAIL: SMB share //$HOST/$SHARE_NAME did not become reachable within ${WAIT_TIMEOUT_SECONDS}s" >&2
    return 1
}

echo "Waiting for share..."
wait_for_share

echo "Listing share..."
run_command "ls"

echo "Downloading stage/hello.txt..."
run_command "get stage/hello.txt $DOWNLOAD_FILE"

if ! grep -q "Synchronic file-system" "$DOWNLOAD_FILE"; then
    echo "FAIL: downloaded hello.txt did not match expected content" >&2
    exit 1
fi

echo "Uploading smoke file into stage/..."
run_command "put $UPLOAD_FILE stage/smoke-upload.txt"

echo "Creating smoke directory in stage/..."
run_command "mkdir stage/smoke-dir"

echo "Renaming uploaded file in stage/..."
run_command "rename stage/smoke-upload.txt stage/smoke-renamed.txt"

echo "Deleting renamed file in stage/..."
run_command "del stage/smoke-renamed.txt"

echo "Removing smoke directory in stage/..."
run_command "rmdir stage/smoke-dir"

echo "Final listing..."
run_command "ls"

echo "PASS: smbclient smoke checks succeeded."
