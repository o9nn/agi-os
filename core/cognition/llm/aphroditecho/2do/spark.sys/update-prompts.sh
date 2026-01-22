#!/bin/bash
set -e
PROJECT_DIR="/workspaces/spark-template"
SOURCE_DIR="$PROJECT_DIR/src"
OUTPUT_FILE="$SOURCE_DIR/prompts-content.ts"
SYSTEM_PROMPT_FILE="$SOURCE_DIR/system_prompt.md"
TOOLS_FILE="$SOURCE_DIR/tools.md"
echo "🔄 Updating prompts-content.ts..."
if [ ! -f "$SYSTEM_PROMPT_FILE" ]; then
    echo "❌ Error: $SYSTEM_PROMPT_FILE not found"
    exit 1
fi
if [ ! -f "$TOOLS_FILE" ]; then
    echo "❌ Error: $TOOLS_FILE not found"
    exit 1
fi
escape_for_js() {
    cat "$1" | jq -Rs . | sed 's/^"//' | sed 's/"$//'
}
echo "📖 Reading and escaping system_prompt.md..."
SYSTEM_PROMPT_CONTENT=$(escape_for_js "$SYSTEM_PROMPT_FILE")
echo "🔧 Reading and escaping tools.md..."
TOOLS_CONTENT=$(escape_for_js "$TOOLS_FILE")
echo "⚡ Generating prompts-content.ts..."
cat > "$OUTPUT_FILE" << EOF
// This file is auto-generated. Do not edit manually.
// Generated from system_prompt.md and tools.md
export const SYSTEM_PROMPT_CONTENT = "$SYSTEM_PROMPT_CONTENT";
export const TOOLS_CONTENT = "$TOOLS_CONTENT";
EOF
echo "✅ Successfully updated $OUTPUT_FILE"
echo "📊 System prompt size: $(echo "$SYSTEM_PROMPT_CONTENT" | wc -c) characters"
echo "📊 Tools content size: $(echo "$TOOLS_CONTENT" | wc -c) characters"
echo ""
echo "💡 The prompts-content.ts file now contains the latest content from:"
echo "   - system_prompt.md"
echo "   - tools.md"
echo ""
echo "🚀 You can now use the updated constants in your React app!"