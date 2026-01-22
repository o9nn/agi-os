#!/bin/bash
set -e
echo "Starting simplified container services..."
echo "Starting Xvfb..."
Xvfb :99 -screen 0 1920x1080x24 &
sleep 2
echo "Starting VNC server..."
x11vnc -display :99 -nopw -forever -xkb &
VNC_PID=$!
echo "Container setup complete!"
echo "VNC server running on port 5900"
echo "Use a VNC viewer to connect to the container display"
trap "echo 'Stopping services'; kill $VNC_PID; exit" TERM INT
tail -f /dev/null & wait