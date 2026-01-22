#!/bin/bash
# C/C++ code minifier for Smol Protocol
# Removes comments and unnecessary whitespace while preserving functionality

INPUT="$1"
OUTPUT="$2"

if [ -z "$INPUT" ]; then
    echo "Usage: $0 <input.c> [output.c]" >&2
    exit 1
fi

# Remove C-style comments (/* ... */) and C++ style comments (//)
# Then remove empty lines and compress whitespace

process_c() {
    # Remove multi-line comments
    perl -0777 -pe 's{/\*.*?\*/}{}gs' "$INPUT" | \
    # Remove single-line comments
    sed 's|//.*$||g' | \
    # Remove leading/trailing whitespace from lines
    sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | \
    # Remove empty lines
    grep -v '^$' | \
    # Compress multiple spaces to single space
    sed 's/[[:space:]]\+/ /g'
}

ORIGINAL_SIZE=$(wc -c < "$INPUT")

if [ -n "$OUTPUT" ]; then
    process_c > "$OUTPUT"
    NEW_SIZE=$(wc -c < "$OUTPUT")
    SAVED=$((ORIGINAL_SIZE - NEW_SIZE))
    echo "Minified: $ORIGINAL_SIZE → $NEW_SIZE bytes ($SAVED saved)"
else
    process_c
fi
