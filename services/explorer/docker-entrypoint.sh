#!/bin/sh
set -e

# Generate runtime environment configuration
cat > /app/build/env-config.js << EOF
window._env_ = {
  SYNC_EXPLORER_ENDPOINT: "${SYNC_EXPLORER_ENDPOINT:-}",
  SYNC_EXPLORER_PASSWORD: "${SYNC_EXPLORER_PASSWORD:-}"
};
EOF

# Execute the CMD
exec "$@"
