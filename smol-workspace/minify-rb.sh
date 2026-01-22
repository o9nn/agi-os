#!/bin/bash
# Ruby code minifier for Smol Protocol
# Removes comments while preserving functionality

INPUT="$1"
OUTPUT="$2"

if [ -z "$INPUT" ]; then
    echo "Usage: $0 <input.rb> [output.rb]" >&2
    exit 1
fi

# Remove Ruby comments (#) and compress
process_rb() {
    # Remove comment lines (lines starting with #, but not shebang)
    sed '2,$ { /^[[:space:]]*#/d }' "$INPUT" | \
    # Remove inline comments (but be careful with strings)
    perl -pe 's/(?<!["'"'"'])#[^"'"'"']*$//' | \
    # Remove leading/trailing whitespace from lines
    sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | \
    # Remove empty lines
    grep -v '^$'
}

ORIGINAL_SIZE=$(wc -c < "$INPUT")

if [ -n "$OUTPUT" ]; then
    process_rb > "$OUTPUT"
    NEW_SIZE=$(wc -c < "$OUTPUT")
    SAVED=$((ORIGINAL_SIZE - NEW_SIZE))
    echo "Minified: $ORIGINAL_SIZE → $NEW_SIZE bytes ($SAVED saved)"
else
    process_rb
fi
