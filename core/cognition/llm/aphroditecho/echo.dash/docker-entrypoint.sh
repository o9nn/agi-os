#!/bin/bash
set -e
echo "Starting container services..."
echo "Starting Xvfb..."
Xvfb :99 -screen 0 1920x1080x24 &
sleep 2
if ! xdpyinfo -display :99 >/dev/null 2>&1; then
echo "Error: Xvfb failed to start properly"
fi
echo "Starting VNC server..."
x11vnc -display :99 -nopw -forever -xkb &
VNC_PID=$!
if [ -e "/var/run/docker.sock" ]; then
echo "Docker socket found, checking permissions..."
if [ -w "/var/run/docker.sock" ]; then
echo "Docker socket is writable"
else
echo "Warning: Docker socket is not writable. You may need to fix permissions."
echo "Try running: sudo chmod 666 /var/run/docker.sock"
fi
else
echo "Docker socket not found. Docker commands will not work."
fi
echo "Container setup complete!"
echo "VNC server running on port 5900"
echo "Use a VNC viewer to connect to the container display"
trap "echo 'Stopping services'; kill $VNC_PID; exit" TERM INT
tail -f /dev/null & wait