#!/bin/bash
if [ $
echo "Description: like diff but ignore logs timestamps"
echo "Wrong number of arguments"
echo "Usage: $0 FILE_A FILE_B"
exit 1
fi
PROG_PATH=$(readlink -f "$0")
PROG_DIR=$(dirname "$PROG_PATH")
fileA=$1
fileB=$2
tmp_fileA="$(mktemp)"
"$PROG_DIR/rm_timestamps.sh" "$fileA" > "$tmp_fileA"
tmp_fileB="$(mktemp)"
"$PROG_DIR/rm_timestamps.sh" "$fileA" > "$tmp_fileA"
diff "$tmp_fileA" "$tmp_fileB"