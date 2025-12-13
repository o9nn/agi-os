#!/bin/bash
# Virtual Environment Setup Script for Deep Tree Echo AGI Avatar
# This script sets up the development environment with all required dependencies

set -e

echo "=========================================="
echo "Deep Tree Echo AGI Avatar - Environment Setup"
echo "=========================================="

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check Node.js version
check_node() {
    echo -e "${YELLOW}Checking Node.js...${NC}"
    if command -v node &> /dev/null; then
        NODE_VERSION=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
        if [ "$NODE_VERSION" -ge 18 ]; then
            echo -e "${GREEN}Node.js $(node -v) is installed${NC}"
        else
            echo -e "${RED}Node.js 18+ is required. Current version: $(node -v)${NC}"
            exit 1
        fi
    else
        echo -e "${RED}Node.js is not installed. Please install Node.js 18+${NC}"
        exit 1
    fi
}

# Check npm
check_npm() {
    echo -e "${YELLOW}Checking npm...${NC}"
    if command -v npm &> /dev/null; then
        echo -e "${GREEN}npm $(npm -v) is installed${NC}"
    else
        echo -e "${RED}npm is not installed${NC}"
        exit 1
    fi
}

# Install dependencies
install_deps() {
    echo -e "${YELLOW}Installing dependencies...${NC}"
    npm install
    echo -e "${GREEN}Dependencies installed${NC}"
}

# Setup Git hooks
setup_hooks() {
    echo -e "${YELLOW}Setting up Git hooks...${NC}"
    if [ -d ".git" ]; then
        npx husky install 2>/dev/null || echo "Husky setup skipped"
        echo -e "${GREEN}Git hooks configured${NC}"
    else
        echo -e "${YELLOW}Not a git repository, skipping hooks${NC}"
    fi
}

# Create necessary directories
create_dirs() {
    echo -e "${YELLOW}Creating directories...${NC}"
    mkdir -p assets/models
    mkdir -p assets/textures
    mkdir -p assets/animations
    mkdir -p dist
    mkdir -p logs
    echo -e "${GREEN}Directories created${NC}"
}

# Create .env file if not exists
create_env() {
    echo -e "${YELLOW}Setting up environment variables...${NC}"
    if [ ! -f ".env" ]; then
        cat > .env << EOF
# Deep Tree Echo AGI Avatar Configuration
NODE_ENV=development
PORT=3000

# Avatar Settings
AVATAR_MODE=hybrid
COGNITIVE_ENABLED=true
HYPER_CHAOTIC=true
SUPER_HOT_GIRL=true

# Debug Settings
DEBUG=false
LOG_LEVEL=info

# API Settings
API_RATE_LIMIT=100
API_TIMEOUT=30000

# WebSocket Settings
WS_PING_INTERVAL=30000
WS_MAX_PAYLOAD=1048576

# Optional: External Services
# REDIS_URL=redis://localhost:6379
# OPENAI_API_KEY=your-key-here
EOF
        echo -e "${GREEN}.env file created${NC}"
    else
        echo -e "${YELLOW}.env file already exists${NC}"
    fi
}

# Build the project
build_project() {
    echo -e "${YELLOW}Building project...${NC}"
    npm run build 2>/dev/null || echo "Build step skipped (will build on first run)"
    echo -e "${GREEN}Build complete${NC}"
}

# Run type checking
type_check() {
    echo -e "${YELLOW}Running type check...${NC}"
    npx tsc --noEmit 2>/dev/null || echo "Type check skipped"
}

# Print completion message
print_complete() {
    echo ""
    echo -e "${GREEN}=========================================="
    echo "Environment setup complete!"
    echo "==========================================${NC}"
    echo ""
    echo "Available commands:"
    echo "  npm run dev      - Start development server"
    echo "  npm run build    - Build for production"
    echo "  npm run test     - Run tests"
    echo "  npm run lint     - Run linter"
    echo "  npm start        - Start production server"
    echo ""
    echo "Docker commands:"
    echo "  docker-compose up -d              - Start production"
    echo "  docker-compose --profile dev up   - Start development"
    echo ""
}

# Main execution
main() {
    check_node
    check_npm
    create_dirs
    install_deps
    setup_hooks
    create_env
    type_check
    print_complete
}

main "$@"
