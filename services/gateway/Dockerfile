FROM node:20-alpine AS build

WORKDIR /app

ARG CUSTOM_SETUP=""

RUN set -eu; \
    if [ -n "$CUSTOM_SETUP" ]; then \
      echo "$CUSTOM_SETUP" | base64 -d > /tmp/custom-setup.sh; \
      chmod +x /tmp/custom-setup.sh; \
      /bin/sh -eu /tmp/custom-setup.sh; \
      rm -f /tmp/custom-setup.sh; \
    fi

COPY package*.json ./
RUN npm install

COPY tsconfig.json ./
COPY assets ./assets
COPY src ./src
RUN npm run build

FROM node:20-alpine AS runtime

WORKDIR /app
ENV NODE_ENV=production

ARG CUSTOM_SETUP=""

RUN set -eu; \
    if [ -n "$CUSTOM_SETUP" ]; then \
      echo "$CUSTOM_SETUP" | base64 -d > /tmp/custom-setup.sh; \
      chmod +x /tmp/custom-setup.sh; \
      /bin/sh -eu /tmp/custom-setup.sh; \
      rm -f /tmp/custom-setup.sh; \
    fi

COPY package*.json ./
RUN npm install --omit=dev

COPY assets ./assets
COPY --from=build /app/dist ./dist

EXPOSE 8180

CMD ["node", "dist/server.js"]
