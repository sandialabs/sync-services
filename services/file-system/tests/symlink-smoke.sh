#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
HOST="${SMB_HOST:-127.0.0.1}"
SHARE_NAME="${SHARE_NAME:-sync}"
SMBCLIENT_BIN="${SMBCLIENT_BIN:-smbclient}"
WAIT_TIMEOUT_SECONDS="${WAIT_TIMEOUT_SECONDS:-30}"
WORK_DIR="${WORK_DIR:-/tmp/sync-fs-symlink-smoke}"
LINK_PATH="${LINK_PATH:-stage/guide-link}"
TARGET_PATH="${TARGET_PATH:-stage/docs/guide.txt}"
rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

cleanup() {
    rm -rf "$WORK_DIR"
}

trap cleanup EXIT INT TERM

"$ROOT_DIR/tests/reset-static-tree.sh" >/dev/null

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
        echo "File checked: $file_path" >&2
        exit 1
    fi
}

LINK_INFO="$WORK_DIR/link-allinfo.txt"
LINK_DOWNLOAD="$WORK_DIR/link.txt"
TARGET_DOWNLOAD="$WORK_DIR/target.txt"
ROOT_STAGE_LISTING="$WORK_DIR/stage-ls.txt"

echo "Waiting for share..."
wait_for_share

echo "Listing stage/ for symlink entry..."
run_command "cd stage; ls" > "$ROOT_STAGE_LISTING"
assert_contains "$ROOT_STAGE_LISTING" "guide-link" "stage listing"

echo "Reading link info..."
run_command "allinfo $LINK_PATH" > "$LINK_INFO"
assert_contains "$LINK_INFO" "guide-link" "symlink allinfo output"

echo "Downloading link target through symlink path..."
run_command "get $LINK_PATH $LINK_DOWNLOAD"
run_command "get $TARGET_PATH $TARGET_DOWNLOAD"

assert_contains "$LINK_DOWNLOAD" "This file lives under docs/." "symlink-followed content"

if ! cmp -s "$LINK_DOWNLOAD" "$TARGET_DOWNLOAD"; then
    echo "FAIL: symlink download did not match target content" >&2
    exit 1
fi

echo "PASS: symlink smoke checks succeeded."
