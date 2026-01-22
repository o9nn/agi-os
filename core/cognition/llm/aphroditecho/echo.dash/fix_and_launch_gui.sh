#!/bin/bash
export LC_ALL=C.UTF-8
export LANG=C.UTF-8
if [ -z "$DISPLAY" ]; then
echo "No DISPLAY set, using :99"
export DISPLAY=:99
fi
xdpyinfo -display $DISPLAY >/dev/null 2>&1
if [ $? -ne 0 ]; then
echo "X server not running on $DISPLAY. Starting Xvfb..."
Xvfb $DISPLAY -screen 0 1920x1080x24 &
XVFB_PID=$!
sleep 2
fi
echo "Starting Deep Tree Echo GUI Dashboard..."
python3 /workspaces/echosurface/launch_gui.py --debug
if [ ! -z "$XVFB_PID" ]; then
kill $XVFB_PID
fi