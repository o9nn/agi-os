#!/bin/bash
set -e
echo "Starting container services..."
Xvfb :99 -screen 0 1920x1080x24 &
sleep 2
x11vnc -display :99 -nopw -forever -xkb &
exec "$@" || tail -f /dev/null