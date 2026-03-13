#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
HOST="${SMB_HOST:-127.0.0.1}"
SHARE_NAME="${SHARE_NAME:-sync}"
SMBCLIENT_BIN="${SMBCLIENT_BIN:-smbclient}"
WAIT_TIMEOUT_SECONDS="${WAIT_TIMEOUT_SECONDS:-30}"
WORK_DIR="${WORK_DIR:-/tmp/sync-fs-json-projection-smoke}"

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

ROOT_LISTING="$WORK_DIR/root-ls.txt"
STAGE_LISTING="$WORK_DIR/stage-ls.txt"
LEDGER_STATE_LISTING="$WORK_DIR/ledger-state-ls.txt"
DOCS_LISTING="$WORK_DIR/docs-ls.txt"
NOTES_LISTING="$WORK_DIR/notes-ls.txt"
HELLO_INFO="$WORK_DIR/hello-allinfo.txt"
HELLO_DOWNLOAD="$WORK_DIR/hello.txt"
GUIDE_DOWNLOAD="$WORK_DIR/guide.txt"
TODO_DOWNLOAD="$WORK_DIR/todo.txt"

echo "Waiting for share..."
wait_for_share

echo "Listing root fixture entries..."
run_command "ls" > "$ROOT_LISTING"
assert_contains "$ROOT_LISTING" "stage" "root listing"
assert_contains "$ROOT_LISTING" "ledger" "root listing"

echo "Listing stage/..."
run_command "cd stage; ls" > "$STAGE_LISTING"
assert_contains "$STAGE_LISTING" "hello.txt" "stage listing"
assert_contains "$STAGE_LISTING" "README.txt" "stage listing"
assert_contains "$STAGE_LISTING" "docs" "stage listing"
assert_contains "$STAGE_LISTING" "notes" "stage listing"

echo "Listing ledger/state/..."
run_command "cd ledger/state; ls" > "$LEDGER_STATE_LISTING"
assert_contains "$LEDGER_STATE_LISTING" "hello.txt" "ledger state listing"
assert_contains "$LEDGER_STATE_LISTING" "README.txt" "ledger state listing"
assert_contains "$LEDGER_STATE_LISTING" "docs" "ledger state listing"
assert_contains "$LEDGER_STATE_LISTING" "notes" "ledger state listing"

echo "Listing docs/..."
run_command "cd stage/docs; ls" > "$DOCS_LISTING"
assert_contains "$DOCS_LISTING" "guide.txt" "docs listing"

echo "Listing notes/..."
run_command "cd stage/notes; ls" > "$NOTES_LISTING"
assert_contains "$NOTES_LISTING" "todo.txt" "notes listing"

echo "Reading file info for stage/hello.txt..."
run_command "allinfo stage/hello.txt" > "$HELLO_INFO"
assert_contains "$HELLO_INFO" "hello.txt" "allinfo output"

echo "Downloading projected files..."
run_command "get stage/hello.txt $HELLO_DOWNLOAD"
run_command "get stage/docs/guide.txt $GUIDE_DOWNLOAD"
run_command "get stage/notes/todo.txt $TODO_DOWNLOAD"

assert_contains "$HELLO_DOWNLOAD" "Synchronic file-system JSON fixture." "hello.txt contents"
assert_contains "$GUIDE_DOWNLOAD" "This file lives under docs/." "docs/guide.txt contents"
assert_contains "$TODO_DOWNLOAD" "validate projection metadata" "notes/todo.txt contents"

echo "Checking ledger/previous/ and ledger/peer/ fixture entries..."
run_command "get ledger/previous/3/state/archive.txt $WORK_DIR/archive.txt"
run_command "get ledger/peer/alice/state/current-remote.txt $WORK_DIR/current-remote.txt"
run_command "get ledger/peer/alice/previous/2/state/remote-note.txt $WORK_DIR/remote-note.txt"
assert_contains "$WORK_DIR/archive.txt" "mock previous snapshot entry" "previous fixture contents"
assert_contains "$WORK_DIR/current-remote.txt" "mock current related-peer entry" "peer fixture contents"
assert_contains "$WORK_DIR/remote-note.txt" "mock historical related-peer entry" "historical peer fixture contents"

echo "Checking read-only namespace enforcement..."
LEDGER_PREVIOUS_PUT_ERROR="$WORK_DIR/ledger-previous-put.err"
LEDGER_PEER_PUT_ERROR="$WORK_DIR/ledger-peer-put.err"

if run_command "put $HELLO_DOWNLOAD ledger/previous/3/state/should-fail.txt" > "$LEDGER_PREVIOUS_PUT_ERROR" 2>&1; then
    echo "FAIL: write to ledger/previous/ unexpectedly succeeded" >&2
    exit 1
fi
assert_contains "$LEDGER_PREVIOUS_PUT_ERROR" "NT_STATUS_ACCESS_DENIED" "ledger previous write error"

if run_command "put $HELLO_DOWNLOAD ledger/peer/alice/state/should-fail.txt" > "$LEDGER_PEER_PUT_ERROR" 2>&1; then
    echo "FAIL: write to ledger/peer/ unexpectedly succeeded" >&2
    exit 1
fi
assert_contains "$LEDGER_PEER_PUT_ERROR" "NT_STATUS_ACCESS_DENIED" "ledger peer write error"

echo "PASS: JSON projection smoke checks succeeded."
