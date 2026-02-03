#!/bin/bash
# Time Crystal Daemon Runner
# Usage: ./run_daemon.sh [socket_path]

SOCKET_PATH="${1:-/tmp/tc_daemon.sock}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DAEMON_PATH="$SCRIPT_DIR/../templates/daemon/time_crystal_daemon.py"

echo "Starting Time Crystal Daemon..."
echo "Socket: $SOCKET_PATH"
echo "Press Ctrl+C to stop"
echo ""

python3 "$DAEMON_PATH" --socket "$SOCKET_PATH"
