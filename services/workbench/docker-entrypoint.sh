#!/bin/sh
set -e

# Generate runtime environment configuration
cat > /app/build/env-config.js << EOF
window._env_ = {
  SYNC_WORKBENCH_ENDPOINT: "${SYNC_WORKBENCH_ENDPOINT:-}",
};
EOF

# Execute the CMD
exec "$@"
