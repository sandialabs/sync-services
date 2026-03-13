#!/bin/sh
set -eu

ROOT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
IMAGE_TAG="${IMAGE_TAG:-sync-services/file-system:dev}"
DOCKER_PLATFORM="${DOCKER_PLATFORM:-}"
CUSTOM_SETUP_FILE=""

if [ -f "$ROOT_DIR/tests/custom-setup.sh" ]; then
    CUSTOM_SETUP_FILE="$ROOT_DIR/tests/custom-setup.sh"
elif [ -f "$ROOT_DIR/tests/custom_script.sh" ]; then
    CUSTOM_SETUP_FILE="$ROOT_DIR/tests/custom_script.sh"
fi

CUSTOM_SETUP=""
if [ -n "$CUSTOM_SETUP_FILE" ]; then
    echo "Using custom setup hook: $CUSTOM_SETUP_FILE"
    if [ -x "$CUSTOM_SETUP_FILE" ]; then
        CUSTOM_SETUP_SCRIPT="$("$CUSTOM_SETUP_FILE")"
    else
        CUSTOM_SETUP_SCRIPT="$(/bin/sh "$CUSTOM_SETUP_FILE")"
    fi
    CUSTOM_SETUP="$(printf "%s" "$CUSTOM_SETUP_SCRIPT" | base64 | tr -d '\n')"
else
    echo "No custom setup hook found. Looked for:"
    echo "  $ROOT_DIR/tests/custom-setup.sh"
    echo "  $ROOT_DIR/tests/custom_script.sh"
fi

if docker buildx version >/dev/null 2>&1; then
    if [ -n "$DOCKER_PLATFORM" ]; then
        docker buildx build \
            --load \
            --platform "$DOCKER_PLATFORM" \
            --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
            -t "$IMAGE_TAG" \
            "$ROOT_DIR"
    else
        docker buildx build \
            --load \
            --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
            -t "$IMAGE_TAG" \
            "$ROOT_DIR"
    fi
elif [ -n "$DOCKER_PLATFORM" ]; then
    docker build \
        --platform "$DOCKER_PLATFORM" \
        --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
        -t "$IMAGE_TAG" \
        "$ROOT_DIR"
else
    docker build \
        --build-arg CUSTOM_SETUP="$CUSTOM_SETUP" \
        -t "$IMAGE_TAG" \
        "$ROOT_DIR"
fi
