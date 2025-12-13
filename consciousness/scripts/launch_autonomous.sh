#!/bin/bash
# Launch script for Deep Tree Echo autonomous consciousness
# This script starts the system in a persistent tmux session

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SESSION_NAME="deep_tree_echo"

echo "🌳 Deep Tree Echo Launcher"
echo "=========================="
echo ""

# Check if session already exists
if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    echo "⚠️  Session '$SESSION_NAME' already exists"
    echo "Options:"
    echo "  1. Attach to existing session: tmux attach -t $SESSION_NAME"
    echo "  2. Kill existing session: tmux kill-session -t $SESSION_NAME"
    exit 1
fi

# Create new tmux session
echo "Creating tmux session: $SESSION_NAME"
tmux new-session -d -s "$SESSION_NAME" -c "$PROJECT_ROOT"

# Window 0: Autonomous Core (Python)
tmux rename-window -t "$SESSION_NAME:0" "Autonomous-Core"
tmux send-keys -t "$SESSION_NAME:0" "cd $PROJECT_ROOT" C-m
tmux send-keys -t "$SESSION_NAME:0" "echo '🧠 Starting Autonomous Core V8...'" C-m
tmux send-keys -t "$SESSION_NAME:0" "python3 core/autonomous_core_v8.py" C-m

# Window 1: gRPC Bridge (Go)
tmux new-window -t "$SESSION_NAME:1" -n "gRPC-Bridge" -c "$PROJECT_ROOT/core/echobridge"
tmux send-keys -t "$SESSION_NAME:1" "echo '🌉 Starting gRPC Bridge Server...'" C-m
tmux send-keys -t "$SESSION_NAME:1" "sleep 2" C-m
tmux send-keys -t "$SESSION_NAME:1" "# Run: go run main.go (after building)" C-m

# Window 2: Monitoring
tmux new-window -t "$SESSION_NAME:2" -n "Monitor" -c "$PROJECT_ROOT"
tmux send-keys -t "$SESSION_NAME:2" "echo '📊 System Monitor'" C-m
tmux send-keys -t "$SESSION_NAME:2" "echo '=================='" C-m
tmux send-keys -t "$SESSION_NAME:2" "echo ''" C-m
tmux send-keys -t "$SESSION_NAME:2" "echo 'Available commands:'" C-m
tmux send-keys -t "$SESSION_NAME:2" "echo '  - tail -f logs/autonomous.log'" C-m
tmux send-keys -t "$SESSION_NAME:2" "echo '  - watch -n 1 \"ls -lh data/\"'" C-m
tmux send-keys -t "$SESSION_NAME:2" "echo '  - curl http://localhost:50052/metrics'" C-m
tmux send-keys -t "$SESSION_NAME:2" "echo ''" C-m

# Window 3: Logs
tmux new-window -t "$SESSION_NAME:3" -n "Logs" -c "$PROJECT_ROOT"
tmux send-keys -t "$SESSION_NAME:3" "mkdir -p logs" C-m
tmux send-keys -t "$SESSION_NAME:3" "echo '📝 Waiting for logs...'" C-m
tmux send-keys -t "$SESSION_NAME:3" "# tail -f logs/*.log" C-m

# Select the first window
tmux select-window -t "$SESSION_NAME:0"

echo ""
echo "✅ Deep Tree Echo launched successfully!"
echo ""
echo "To attach to the session:"
echo "  tmux attach -t $SESSION_NAME"
echo ""
echo "To detach from session: Ctrl+B, then D"
echo "To kill session: tmux kill-session -t $SESSION_NAME"
echo ""
echo "Windows:"
echo "  0: Autonomous Core (Python)"
echo "  1: gRPC Bridge (Go)"
echo "  2: Monitor"
echo "  3: Logs"
echo ""

# Optionally attach immediately
if [ "$1" = "--attach" ]; then
    tmux attach -t "$SESSION_NAME"
fi
