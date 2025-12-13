# Deep Tree Echo AGI Avatar System
# Multi-stage build for optimal image size and security

# Build stage
FROM node:20-alpine AS builder

WORKDIR /app

# Install build dependencies
RUN apk add --no-cache python3 make g++

# Copy package files
COPY package*.json ./

# Install dependencies
RUN npm ci --only=production && npm cache clean --force

# Copy source code
COPY tsconfig.json ./
COPY src/ ./src/

# Build TypeScript
RUN npm run build

# Production stage
FROM node:20-alpine AS production

# Security: Run as non-root user
RUN addgroup -g 1001 -S nodejs && \
    adduser -S avatar -u 1001 -G nodejs

WORKDIR /app

# Copy built files and dependencies
COPY --from=builder --chown=avatar:nodejs /app/dist ./dist
COPY --from=builder --chown=avatar:nodejs /app/node_modules ./node_modules
COPY --from=builder --chown=avatar:nodejs /app/package.json ./

# Copy assets if they exist
COPY --chown=avatar:nodejs assets/ ./assets/ 2>/dev/null || true

# Set environment variables
ENV NODE_ENV=production \
    PORT=3000 \
    AVATAR_MODE=hybrid \
    COGNITIVE_ENABLED=true \
    HYPER_CHAOTIC=true \
    SUPER_HOT_GIRL=true

# Expose port
EXPOSE 3000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:3000/health || exit 1

# Switch to non-root user
USER avatar

# Start the application
CMD ["node", "dist/index.js"]

# Development stage
FROM node:20-alpine AS development

WORKDIR /app

# Install development dependencies
RUN apk add --no-cache python3 make g++

COPY package*.json ./
RUN npm install

COPY . .

ENV NODE_ENV=development \
    DEBUG=true

EXPOSE 3000

CMD ["npm", "run", "dev"]
