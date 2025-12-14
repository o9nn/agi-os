#!/bin/bash
# Execute Deep Tree Echo Hypergraph Schema on Neon Database

PROJECT_ID="lively-recipe-23926980"
SCHEMA_FILE="/home/ubuntu/aphroditecho/cognitive_architectures/create_hypergraph_schemas.sql"

echo "================================"
echo "Executing Schema on Neon Database"
echo "================================"
echo "Project: deep-tree-echo-hypergraph"
echo "Project ID: $PROJECT_ID"
echo ""

# Read the schema file
SCHEMA_SQL=$(cat "$SCHEMA_FILE")

# Execute via MCP
echo "Executing schema SQL..."
manus-mcp-cli tool call run_sql --server neon --input "{
  \"params\": {
    \"projectId\": \"$PROJECT_ID\",
    \"sql\": $(echo "$SCHEMA_SQL" | jq -Rs .)
  }
}"

echo ""
echo "Schema execution complete!"
