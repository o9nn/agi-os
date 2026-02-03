#!/bin/bash
# Time Crystal Daemon LLM Interface Runner
# Usage: ./run_interface.sh [socket_path]

SOCKET_PATH="${1:-/tmp/tc_daemon.sock}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INTERFACE_PATH="$SCRIPT_DIR/../templates/llm_interface/llm_sidecar.py"

echo "Starting LLM Interface..."
echo "Connecting to daemon at: $SOCKET_PATH"
echo ""

python3 "$INTERFACE_PATH"
