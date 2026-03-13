#!/bin/sh
set -eu

MODE="${1:-up}"
if [ "$MODE" != "up" ] && [ "$MODE" != "smoke" ]; then
    echo "Usage: $0 [up|smoke]"
    exit 1
fi

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
COMPOSE_DIR="$ROOT_DIR/compose/general"
COMPOSE_FILE="$COMPOSE_DIR/docker-compose.yml"
CUSTOM_SETUP_FILE="$ROOT_DIR/tests/custom-setup.sh"

PORT="${PORT:-8192}"
SMB_PORT="${SMB_PORT:-445}"
SECRET="${SECRET:-password}"
PERIOD="${PERIOD:-2}"
WINDOW="${WINDOW:-1024}"
LISP_HTTP_PORT="${LISP_HTTP_PORT:-8765}"
LOCAL_LISP_DIRECTORY="${LOCAL_LISP_DIRECTORY:-}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-60}"
CONNECT_TIMEOUT_SECONDS="${CONNECT_TIMEOUT_SECONDS:-2}"
REQUEST_TIMEOUT_SECONDS="${REQUEST_TIMEOUT_SECONDS:-5}"
COMPOSE_PROJECT_NAME="${COMPOSE_PROJECT_NAME:-$(basename "$COMPOSE_DIR")}"
LOCAL_COMPOSE_FORCE_HTTP="${LOCAL_COMPOSE_FORCE_HTTP:-1}"
ENABLE_FILE_SYSTEM="${ENABLE_FILE_SYSTEM:-1}"
FILE_SYSTEM_IMAGE="${FILE_SYSTEM_IMAGE:-sync-services/file-system:dev}"
DOCKER_PLATFORM="${DOCKER_PLATFORM:-}"
GENERAL_DOCKER_PLATFORM="${GENERAL_DOCKER_PLATFORM:-linux/amd64}"
GENERAL_PLATFORM="${GENERAL_PLATFORM:-linux/amd64}"
USE_REMOTE_GENERAL="${USE_REMOTE_GENERAL:-0}"

cleanup_mode="down"
server_pid=""

GENERAL_VERSION="$(cat "$ROOT_DIR/compose/general/version.txt")"
GATEWAY_VERSION="$(cat "$ROOT_DIR/services/gateway/version.txt")"
EXPLORER_VERSION="$(cat "$ROOT_DIR/services/explorer/version.txt")"
WORKBENCH_VERSION="$(cat "$ROOT_DIR/services/workbench/version.txt")"
ROUTER_VERSION="$(cat "$ROOT_DIR/services/router/version.txt")"

GENERAL_REMOTE_TAG="ghcr.io/sandialabs/sync-services/general:$GENERAL_VERSION"
GATEWAY_REMOTE_TAG="ghcr.io/sandialabs/sync-services/gateway:$GATEWAY_VERSION"
EXPLORER_REMOTE_TAG="ghcr.io/sandialabs/sync-services/explorer:$EXPLORER_VERSION"
WORKBENCH_REMOTE_TAG="ghcr.io/sandialabs/sync-services/workbench:$WORKBENCH_VERSION"
ROUTER_REMOTE_TAG="ghcr.io/sandialabs/sync-services/router:$ROUTER_VERSION"

GENERAL_LOCAL_TAG="sync-services/local-general:$GENERAL_VERSION"
GATEWAY_LOCAL_TAG="sync-services/local-gateway:$GATEWAY_VERSION"
EXPLORER_LOCAL_TAG="sync-services/local-explorer:$EXPLORER_VERSION"
WORKBENCH_LOCAL_TAG="sync-services/local-workbench:$WORKBENCH_VERSION"
ROUTER_LOCAL_TAG="sync-services/local-router:$ROUTER_VERSION"

resolve_directory() {
    input_path="$1"
    if [ -z "$input_path" ]; then
        return 1
    fi

    case "$input_path" in
        /*)
            candidate="$input_path"
            ;;
        *)
            candidate="$PWD/$input_path"
            ;;
    esac

    if [ -d "$candidate" ]; then
        (CDPATH= cd -- "$candidate" && pwd -P)
        return 0
    fi

    return 1
}

validate_local_lisp_directory() {
    directory="$1"
    missing=""
    for required in control.scm standard.scm log-chain.scm linear-chain.scm tree.scm configuration.scm ledger.scm; do
        if [ ! -f "$directory/$required" ]; then
            missing="$missing $required"
        fi
    done
    if [ -n "$missing" ]; then
        echo "Missing required Lisp files in $directory:$missing" >&2
        exit 1
    fi
}

dc() {
    if [ "$ENABLE_FILE_SYSTEM" = "1" ]; then
        docker compose -f "$COMPOSE_FILE" --profile filesystem "$@"
    else
        docker compose -f "$COMPOSE_FILE" "$@"
    fi
}

has_existing_named_volumes() {
    names="$(dc config --volumes 2>/dev/null || true)"
    if [ -z "$names" ]; then
        return 1
    fi
    for logical_name in $names; do
        full_name="${COMPOSE_PROJECT_NAME}_${logical_name}"
        if docker volume inspect "$full_name" >/dev/null 2>&1; then
            return 0
        fi
    done
    return 1
}

confirm_volume_wipe_if_needed() {
    if ! has_existing_named_volumes; then
        return 0
    fi
    echo "Existing compose volumes were found for project '$COMPOSE_PROJECT_NAME'."
    while :; do
        printf "Wipe existing volumes and continue? [y/n]: "
        IFS= read -r answer || true
        case "$(printf "%s" "$answer" | tr '[:upper:]' '[:lower:]')" in
            y|yes) return 0 ;;
            n|no)
                echo "Aborting."
                exit 1
                ;;
            *) echo "Please answer y or n." ;;
        esac
    done
}

cleanup() {
    set +e
    if [ -n "$server_pid" ]; then
        kill "$server_pid" >/dev/null 2>&1 || true
        wait "$server_pid" >/dev/null 2>&1 || true
    fi
    if [ "$cleanup_mode" = "down" ]; then
        dc down -v --remove-orphans >/dev/null 2>&1
    fi
}

on_interrupt() {
    cleanup
    exit 130
}

trap cleanup EXIT
trap on_interrupt INT TERM

CUSTOM_SETUP=""
if [ -x "$CUSTOM_SETUP_FILE" ]; then
    CUSTOM_SETUP_SCRIPT="$("$CUSTOM_SETUP_FILE")"
    CUSTOM_SETUP="$(printf "%s" "$CUSTOM_SETUP_SCRIPT" | base64 | tr -d '\n')"
fi

LISP_REPOSITORY_ARG=""
if [ -n "$LOCAL_LISP_DIRECTORY" ]; then
    if ! LOCAL_LISP_DIRECTORY="$(resolve_directory "$LOCAL_LISP_DIRECTORY")"; then
        echo "LOCAL_LISP_DIRECTORY does not exist: ${LOCAL_LISP_DIRECTORY}" >&2
        exit 1
    fi

    validate_local_lisp_directory "$LOCAL_LISP_DIRECTORY"
    echo "Using local Lisp directory: $LOCAL_LISP_DIRECTORY"

    echo "Starting temporary local Lisp HTTP server on port $LISP_HTTP_PORT..."
    python3 -m http.server "$LISP_HTTP_PORT" --bind 127.0.0.1 --directory "$LOCAL_LISP_DIRECTORY" >/tmp/sync-local-lisp-http.log 2>&1 &
    server_pid=$!
    sleep 1
    if ! kill -0 "$server_pid" >/dev/null 2>&1; then
        echo "Failed to start local Lisp HTTP server. See /tmp/sync-local-lisp-http.log" >&2
        exit 1
    fi

    LISP_REPOSITORY_ARG="http://host.docker.internal:${LISP_HTTP_PORT}/"
fi

if [ "$LOCAL_COMPOSE_FORCE_HTTP" = "1" ]; then
    TLS_STUB_DIR="/tmp/sync-services-local-compose"
    TLS_STUB_CERT="$TLS_STUB_DIR/http-only.crt"
    TLS_STUB_KEY="$TLS_STUB_DIR/http-only.key"
    mkdir -p "$TLS_STUB_DIR"
    printf "HTTP-only local-compose placeholder cert.\n" > "$TLS_STUB_CERT"
    printf "HTTP-only local-compose placeholder key.\n" > "$TLS_STUB_KEY"
    TLS_CERT_HOST_PATH="$TLS_STUB_CERT"
    TLS_KEY_HOST_PATH="$TLS_STUB_KEY"
fi

build_and_retag() {
    context="$1"
    local_tag="$2"
    remote_tag="$3"
    lisp_repository="${4:-}"
    build_platform="${5:-$DOCKER_PLATFORM}"

    echo "Building $local_tag ..."
    if docker buildx version >/dev/null 2>&1; then
        if [ -n "$lisp_repository" ]; then
            if [ -n "$build_platform" ]; then
                docker buildx build \
                    --load \
                    --platform "$build_platform" \
                    --add-host host.docker.internal:host-gateway \
                    --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                    --build-arg LISP_REPOSITORY="$lisp_repository" \
                    -t "$local_tag" \
                    "$context"
            else
                docker buildx build \
                    --load \
                    --add-host host.docker.internal:host-gateway \
                    --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                    --build-arg LISP_REPOSITORY="$lisp_repository" \
                    -t "$local_tag" \
                    "$context"
            fi
        else
            if [ -n "$build_platform" ]; then
                docker buildx build \
                    --load \
                    --platform "$build_platform" \
                    --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                    -t "$local_tag" \
                    "$context"
            else
                docker buildx build \
                    --load \
                    --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                    -t "$local_tag" \
                    "$context"
            fi
        fi
    else
        if [ -n "$lisp_repository" ]; then
            if [ -n "$build_platform" ]; then
                docker build \
                    --platform "$build_platform" \
                    --add-host host.docker.internal:host-gateway \
                    --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                    --build-arg LISP_REPOSITORY="$lisp_repository" \
                    -t "$local_tag" \
                    "$context"
            else
                docker build \
                    --add-host host.docker.internal:host-gateway \
                    --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                    --build-arg LISP_REPOSITORY="$lisp_repository" \
                    -t "$local_tag" \
                    "$context"
            fi
        else
            if [ -n "$build_platform" ]; then
                docker build \
                    --platform "$build_platform" \
                    --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                    -t "$local_tag" \
                    "$context"
            else
                docker build \
                    --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                    -t "$local_tag" \
                    "$context"
            fi
        fi
    fi

    echo "Tagging $local_tag as $remote_tag ..."
    docker tag "$local_tag" "$remote_tag"
}

if [ "$USE_REMOTE_GENERAL" = "1" ]; then
    echo "Using remote general image: $GENERAL_REMOTE_TAG"
    docker pull "$GENERAL_REMOTE_TAG"
else
    build_and_retag "$COMPOSE_DIR" "$GENERAL_LOCAL_TAG" "$GENERAL_REMOTE_TAG" "$LISP_REPOSITORY_ARG" "$GENERAL_DOCKER_PLATFORM"
fi
build_and_retag "$ROOT_DIR/services/gateway" "$GATEWAY_LOCAL_TAG" "$GATEWAY_REMOTE_TAG"
build_and_retag "$ROOT_DIR/services/explorer" "$EXPLORER_LOCAL_TAG" "$EXPLORER_REMOTE_TAG"
build_and_retag "$ROOT_DIR/services/workbench" "$WORKBENCH_LOCAL_TAG" "$WORKBENCH_REMOTE_TAG"
build_and_retag "$ROOT_DIR/services/router" "$ROUTER_LOCAL_TAG" "$ROUTER_REMOTE_TAG"

if [ "$ENABLE_FILE_SYSTEM" = "1" ]; then
    echo "Building $FILE_SYSTEM_IMAGE ..."
    if docker buildx version >/dev/null 2>&1; then
        if [ -n "$DOCKER_PLATFORM" ]; then
            docker buildx build \
                --load \
                --platform "$DOCKER_PLATFORM" \
                --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                -t "$FILE_SYSTEM_IMAGE" \
                "$ROOT_DIR/services/file-system"
        else
            docker buildx build \
                --load \
                --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                -t "$FILE_SYSTEM_IMAGE" \
                "$ROOT_DIR/services/file-system"
        fi
    else
        if [ -n "$DOCKER_PLATFORM" ]; then
            docker build \
                --platform "$DOCKER_PLATFORM" \
                --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                -t "$FILE_SYSTEM_IMAGE" \
                "$ROOT_DIR/services/file-system"
        else
            docker build \
                --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
                -t "$FILE_SYSTEM_IMAGE" \
                "$ROOT_DIR/services/file-system"
        fi
    fi
fi

export SECRET PERIOD WINDOW PORT SMB_PORT COMPOSE_PROJECT_NAME TLS_CERT_HOST_PATH TLS_KEY_HOST_PATH FILE_SYSTEM_IMAGE GENERAL_PLATFORM

confirm_volume_wipe_if_needed
echo "Starting from scratch: removing compose stack + volumes..."
dc down -v --remove-orphans >/dev/null 2>&1

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
    echo "Timed out waiting for $url" >&2
    return 1
}

api_post() {
    body="$1"
    curl -fsS \
      -H "Content-Type: application/json" \
      -d "$body" \
      "http://127.0.0.1:$PORT/interface/json"
}

gateway_status() {
    method="$1"
    path="$2"
    shift 2
    curl -sS -o /dev/null -w "%{http_code}" -X "$method" "$@" "http://127.0.0.1:$PORT$path"
}

gateway_get() {
    path="$1"
    shift
    curl -fsS "$@" "http://127.0.0.1:$PORT$path"
}

if [ "$MODE" = "up" ]; then
    echo "Starting compose stack on port $PORT in up mode (Ctrl+C to stop)..."
    dc --ansi always up --pull never
    exit 0
fi

echo "Starting compose stack on port $PORT in smoke mode..."
dc --ansi always up -d --pull never

echo "Waiting for routes..."
wait_for_http "http://127.0.0.1:$PORT/explorer/"
wait_for_http "http://127.0.0.1:$PORT/workbench/"
wait_for_http "http://127.0.0.1:$PORT/api/v1/docs"

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

gateway_size="$(gateway_get "/api/v1/general/size" | tr -d '[:space:]')"
case "$gateway_size" in
    ''|*[!0-9]*)
        echo "FAIL: gateway size response is not a number: $gateway_size"
        exit 1
        ;;
esac

control_unauthorized_status="$(gateway_status POST "/api/v1/control/step" -H "Content-Type: application/json" -d '[]')"
if [ "$control_unauthorized_status" != "401" ]; then
    echo "FAIL: expected gateway control route to require auth (401), got $control_unauthorized_status"
    exit 1
fi

control_authorized_status="$(gateway_status POST "/api/v1/control/step" -H "Authorization: Bearer $SECRET" -H "Content-Type: application/json" -d '[]')"
if [ "$control_authorized_status" != "200" ]; then
    echo "FAIL: expected authenticated gateway control route to succeed (200), got $control_authorized_status"
    exit 1
fi

if [ "$ENABLE_FILE_SYSTEM" = "1" ]; then
    if ! command -v smbclient >/dev/null 2>&1; then
        echo "FAIL: ENABLE_FILE_SYSTEM=1 requires smbclient to be installed"
        exit 1
    fi

    echo "Waiting for SMB file-system service on port $SMB_PORT..."
    elapsed=0
    while [ "$elapsed" -lt "$TIMEOUT_SECONDS" ]; do
        if smbclient //127.0.0.1/sync -N -p "$SMB_PORT" -c 'ls' >/tmp/sync-services-fs-root-ls.log 2>&1; then
            break
        fi
        sleep 2
        elapsed=$((elapsed + 2))
    done
    if [ "$elapsed" -ge "$TIMEOUT_SECONDS" ]; then
        echo "FAIL: timed out waiting for SMB file-system service on port $SMB_PORT"
        cat /tmp/sync-services-fs-root-ls.log 2>/dev/null || true
        exit 1
    fi

    fs_root_listing="$(cat /tmp/sync-services-fs-root-ls.log)"
    for required in stage ledger control; do
        if ! printf "%s" "$fs_root_listing" | grep -q " $required "; then
            echo "FAIL: expected SMB root listing to contain '$required'"
            printf "%s\n" "$fs_root_listing"
            exit 1
        fi
    done

    fs_tmp_dir="/tmp/sync-services-fs-smoke"
    fs_local_file="$fs_tmp_dir/local.txt"
    fs_download_file="$fs_tmp_dir/downloaded.txt"
    mkdir -p "$fs_tmp_dir"
    printf "compose filesystem smoke\n" > "$fs_local_file"
    rm -f "$fs_download_file"

    if ! smbclient //127.0.0.1/sync -N -p "$SMB_PORT" -c "cd stage; put $fs_local_file compose-smoke.txt; get compose-smoke.txt $fs_download_file; del compose-smoke.txt" >/tmp/sync-services-fs-stage.log 2>&1; then
        echo "FAIL: SMB stage smoke failed"
        cat /tmp/sync-services-fs-stage.log
        exit 1
    fi

    if ! cmp -s "$fs_local_file" "$fs_download_file"; then
        echo "FAIL: SMB stage round-trip content mismatch"
        exit 1
    fi
fi

echo "PASS: smoke checks succeeded."
