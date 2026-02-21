# Build stage
FROM node:20-alpine3.20 AS builder

WORKDIR /app

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

# Install serve globally
RUN npm install -g serve

# Copy built assets from builder stage
COPY --from=builder /app/build ./build

# Copy entrypoint script
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh

EXPOSE 3000

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["serve", "-s", "build", "-l", "3000"]
