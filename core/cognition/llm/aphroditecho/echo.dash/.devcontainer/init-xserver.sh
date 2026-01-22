#!/bin/bash
echo "===== Initializing X server environment ====="
if [ -e /tmp/.X11-unix/X1 ]; then
echo "X server is already running"
exit 0
fi
echo "Starting Xvfb..."
Xvfb :1 -screen 0 1280x1024x24 &
sleep 2
echo "Starting Fluxbox..."
fluxbox -display :1 &
sleep 1
echo "Starting x11vnc..."
x11vnc -display :1 -forever -nopw -shared -bg -o /tmp/x11vnc.log
chmod 1777 /tmp/.X11-unix
touch /tmp/.Xauthority
chmod 644 /tmp/.Xauthority
echo "X server environment initialized successfully"
echo "You can connect to VNC on port 5901 if needed"