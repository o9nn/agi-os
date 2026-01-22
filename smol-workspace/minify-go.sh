#!/bin/bash
# Go code minifier for Smol Protocol
# Removes comments while preserving functionality

INPUT="$1"
OUTPUT="$2"

if [ -z "$INPUT" ]; then
    echo "Usage: $0 <input.go> [output.go]" >&2
    exit 1
fi

# Remove Go comments and compress
process_go() {
    # Remove multi-line comments
    perl -0777 -pe 's{/\*.*?\*/}{}gs' "$INPUT" | \
    # Remove single-line comments
    sed 's|//.*$||g' | \
    # Remove leading/trailing whitespace from lines
    sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | \
    # Remove empty lines
    grep -v '^$'
}

ORIGINAL_SIZE=$(wc -c < "$INPUT")

if [ -n "$OUTPUT" ]; then
    process_go > "$OUTPUT"
    # Format with gofmt to ensure validity
    gofmt -w "$OUTPUT" 2>/dev/null || true
    NEW_SIZE=$(wc -c < "$OUTPUT")
    SAVED=$((ORIGINAL_SIZE - NEW_SIZE))
    echo "Minified: $ORIGINAL_SIZE → $NEW_SIZE bytes ($SAVED saved)"
else
    process_go
fi
