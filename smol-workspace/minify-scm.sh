#!/bin/bash
# Scheme code minifier for Smol Protocol
# Removes comments while preserving functionality

INPUT="$1"
OUTPUT="$2"

if [ -z "$INPUT" ]; then
    echo "Usage: $0 <input.scm> [output.scm]" >&2
    exit 1
fi

# Remove Scheme comments (;; and ;) and compress
process_scm() {
    # Remove comment lines (lines starting with ;)
    sed '/^[[:space:]]*;/d' "$INPUT" | \
    # Remove inline comments
    sed 's/;[^"]*$//' | \
    # Remove leading/trailing whitespace from lines
    sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | \
    # Remove empty lines
    grep -v '^$'
}

ORIGINAL_SIZE=$(wc -c < "$INPUT")

if [ -n "$OUTPUT" ]; then
    process_scm > "$OUTPUT"
    NEW_SIZE=$(wc -c < "$OUTPUT")
    SAVED=$((ORIGINAL_SIZE - NEW_SIZE))
    echo "Minified: $ORIGINAL_SIZE → $NEW_SIZE bytes ($SAVED saved)"
else
    process_scm
fi
