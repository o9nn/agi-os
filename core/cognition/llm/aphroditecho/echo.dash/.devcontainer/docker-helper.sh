#!/bin/bash
set -e
echo "=== Docker Environment Diagnostics ==="
if [ -S /var/run/docker.sock ]; then
    echo "✅ Docker socket exists at /var/run/docker.sock"
    if ls -la /var/run/docker.sock | grep -q "$(id -g)"; then
        echo "✅ You have permission to access the Docker socket"
    else
        echo "❌ Permission issue detected with Docker socket"
        echo "Fixing permissions..."
        sudo chmod 666 /var/run/docker.sock || {
            echo "Could not change permissions. You might need to run as root."
            echo "Try: sudo chmod 666 /var/run/docker.sock"
        }
    fi
else
    echo "❌ Docker socket not found at /var/run/docker.sock"
    echo "Please ensure Docker is installed and running"
fi
if command -v docker &> /dev/null; then
    echo "✅ Docker client is installed"
    echo "Docker version: $(docker --version)"
else
    echo "❌ Docker client not found"
    echo "Installing Docker client..."
    sudo apt-get update && sudo apt-get install -y docker.io
fi
if command -v docker-compose &> /dev/null; then
    echo "✅ Docker Compose is installed"
    echo "Docker Compose version: $(docker-compose --version)"
elif command -v docker &> /dev/null && docker compose version &> /dev/null; then
    echo "✅ Docker Compose V2 plugin is installed"
    echo "Docker Compose version: $(docker compose version)"
else
    echo "❌ Docker Compose not found"
    echo "Installing Docker Compose..."
    sudo curl -L "https://github.com/docker/compose/releases/download/v2.20.3/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
    sudo chmod +x /usr/local/bin/docker-compose
fi
if [ -f /.dockerenv ]; then
    echo "✅ Running inside a Docker container"
    if [ -S /var/run/docker.sock ] || [ -n "$DOCKER_HOST" ]; then
        echo "✅ Docker-in-Docker is properly configured"
    else
        echo "❌ Docker-in-Docker is not properly configured"
        echo "Make sure the Docker socket is mounted or DOCKER_HOST is set"
    fi
else
    echo "⚠️ Not running inside a Docker container"
fi
if [ -f /workspaces/echosurface/docker-compose.yml ] && [ -f /workspaces/echosurface/.devcontainer/docker-compose.yml ]; then
    echo "⚠️ Found multiple docker-compose.yml files"
    echo "This is normal but be aware of potential conflicts."
    echo "- Project docker-compose.yml: for running your application"
    echo "- Devcontainer docker-compose.yml: for your development environment"
    PROJECT_PORTS=$(grep -o "port.*:" /workspaces/echosurface/docker-compose.yml | grep -o '[0-9]*:' | tr -d ':')
    DEVCONTAINER_PORTS=$(grep -o "port.*:" /workspaces/echosurface/.devcontainer/docker-compose.yml | grep -o '[0-9]*:' | tr -d ':')
    for port in $PROJECT_PORTS; do
        if echo "$DEVCONTAINER_PORTS" | grep -q "$port"; then
            echo "⚠️ Port conflict detected: $port is used in both files"
        fi
    done
fi
echo ""
echo "=== How to use Docker Compose ==="
echo "1. For your project services:"
echo "   cd /workspaces/echosurface"
echo "   docker-compose up -d"
echo ""
echo "2. For devcontainer-related services:"
echo "   cd /workspaces/echosurface/.devcontainer"
echo "   docker-compose up -d"
echo ""
echo "Docker environment check completed!"