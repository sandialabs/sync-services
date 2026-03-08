# Build stage
FROM node:20-alpine3.20 AS builder

WORKDIR /app

ARG CUSTOM_SETUP=""

RUN set -eu; \
    if [ -n "$CUSTOM_SETUP" ]; then \
      echo "$CUSTOM_SETUP" | base64 -d > /tmp/custom-setup.sh; \
      chmod +x /tmp/custom-setup.sh; \
      /bin/sh -eu /tmp/custom-setup.sh; \
      rm -f /tmp/custom-setup.sh; \
    fi

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm install

# Copy source code
COPY . .

# Run tests
RUN npm test -- --watchAll=false --passWithNoTests

# Build the application
RUN npm run build

# Production stage
FROM node:20-alpine3.20

WORKDIR /app

ARG CUSTOM_SETUP=""

RUN set -eu; \
    if [ -n "$CUSTOM_SETUP" ]; then \
      echo "$CUSTOM_SETUP" | base64 -d > /tmp/custom-setup.sh; \
      chmod +x /tmp/custom-setup.sh; \
      /bin/sh -eu /tmp/custom-setup.sh; \
      rm -f /tmp/custom-setup.sh; \
    fi

# Install serve globally
RUN npm install -g serve

# Copy built assets from builder stage
COPY --from=builder /app/build ./build

# Copy entrypoint script
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh

EXPOSE 80

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["serve", "-s", "build", "-l", "80"]
