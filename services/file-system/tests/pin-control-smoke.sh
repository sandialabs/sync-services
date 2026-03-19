#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
SMBCLIENT_BIN="${SMBCLIENT_BIN:-smbclient}"
HOST="${SMB_HOST:-127.0.0.1}"
SHARE_NAME="${SHARE_NAME:-sync}"
WAIT_TIMEOUT_SECONDS="${WAIT_TIMEOUT_SECONDS:-30}"
WORK_DIR="${WORK_DIR:-/tmp/sync-fs-pin-control-smoke}"

SYNC_FS_GATEWAY_BASE_URL="${SYNC_FS_GATEWAY_BASE_URL:-}"
SYNC_FS_GATEWAY_AUTH_TOKEN="${SYNC_FS_GATEWAY_AUTH_TOKEN:-}"
PIN_TARGET_PATH="${PIN_TARGET_PATH:-}"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

cleanup() {
    rm -rf "$WORK_DIR"
}

trap cleanup EXIT INT TERM

run_command() {
    command_text="$1"
    "$SMBCLIENT_BIN" "//$HOST/$SHARE_NAME" -N -c "$command_text"
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

assert_contains() {
    file_path="$1"
    expected="$2"
    description="$3"

    if ! grep -Fq "$expected" "$file_path"; then
        echo "FAIL: expected $description to contain: $expected" >&2
        cat "$file_path" >&2 || true
        exit 1
    fi
}

assert_not_contains() {
    file_path="$1"
    unexpected="$2"
    description="$3"

    if grep -Fq "$unexpected" "$file_path"; then
        echo "FAIL: expected $description to not contain: $unexpected" >&2
        cat "$file_path" >&2 || true
        exit 1
    fi
}

run_backend() {
    if [ -n "$SYNC_FS_GATEWAY_BASE_URL" ]; then
        SYNC_FS_BACKEND=http-journal-readonly \
        SYNC_FS_GATEWAY_BASE_URL="$SYNC_FS_GATEWAY_BASE_URL" \
        SYNC_FS_GATEWAY_AUTH_TOKEN="$SYNC_FS_GATEWAY_AUTH_TOKEN" \
        "$ROOT_DIR/tests/docker-run.sh" >/dev/null
    else
        "$ROOT_DIR/tests/reset-static-tree.sh" >/dev/null
        SYNC_FS_BACKEND=mock-gateway-readonly \
        "$ROOT_DIR/tests/docker-run.sh" >/dev/null
    fi
}

ROOT_LISTING="$WORK_DIR/root-ls.txt"
CONTROL_PIN="$WORK_DIR/control-pin.txt"
WRITE_FILE="$WORK_DIR/pin-write.txt"
DISCOVER_TODO="$WORK_DIR/discover-todo.txt"
DISCOVER_HELLO="$WORK_DIR/discover-hello.txt"

run_backend

echo "Waiting for share..."
wait_for_share

echo "Listing root..."
run_command "ls" > "$ROOT_LISTING"
assert_contains "$ROOT_LISTING" "control" "root listing"

echo "Reading control/pin..."

if [ -z "$SYNC_FS_GATEWAY_BASE_URL" ]; then
    echo "Discovering ledger paths..."
    run_command "get ledger/state/notes/todo.txt $DISCOVER_TODO"
    run_command "get ledger/state/hello.txt $DISCOVER_HELLO"
    run_command "get control/pin $CONTROL_PIN"
    assert_contains "$CONTROL_PIN" "pinned /ledger/state/notes/todo.txt" "mock pin control file"
    assert_contains "$CONTROL_PIN" "unpinned /ledger/state/hello.txt" "mock pin control file"
    printf 'pinned /ledger/state/hello.txt\nunpinned /ledger/state/notes/todo.txt\n' > "$WRITE_FILE"
    echo "Writing updated control/pin directives..."
    run_command "put $WRITE_FILE control/pin"
    run_command "get control/pin $CONTROL_PIN"
    assert_contains "$CONTROL_PIN" "pinned /ledger/state/hello.txt" "updated mock pin control file"
    assert_contains "$CONTROL_PIN" "unpinned /ledger/state/notes/todo.txt" "updated mock pin control file"
    echo "PASS: pin control file smoke checks succeeded against mock gateway."
    exit 0
fi

if [ -n "$PIN_TARGET_PATH" ]; then
    DISCOVER_COMMAND="get ${PIN_TARGET_PATH#/} $DISCOVER_HELLO"
    echo "Discovering live target $PIN_TARGET_PATH..."
    run_command "$DISCOVER_COMMAND"
fi
run_command "get control/pin $CONTROL_PIN"

echo "Live gateway control/pin is readable."
if [ -z "$PIN_TARGET_PATH" ]; then
    echo "PASS: control/pin read succeeded against live gateway."
    echo "No explicit live pin target was provided; skipping write mutation."
    exit 0
fi

printf 'pinned %s\n' "$PIN_TARGET_PATH" > "$WRITE_FILE"
echo "Pinning $PIN_TARGET_PATH via control/pin..."
run_command "put $WRITE_FILE control/pin"
run_command "get control/pin $CONTROL_PIN"
assert_contains "$CONTROL_PIN" "pinned $PIN_TARGET_PATH" "live pin control file after pin"

printf 'unpinned %s\n' "$PIN_TARGET_PATH" > "$WRITE_FILE"
echo "Unpinning $PIN_TARGET_PATH via control/pin..."
run_command "put $WRITE_FILE control/pin"
run_command "get control/pin $CONTROL_PIN"
assert_contains "$CONTROL_PIN" "unpinned $PIN_TARGET_PATH" "live pin control file after unpin"

echo "PASS: control/pin read/write succeeded against live gateway."
