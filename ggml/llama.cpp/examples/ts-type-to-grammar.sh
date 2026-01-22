#!/usr/bin/env bash
set -euo pipefail
readonly type="$1"
TMPDIR=""
trap 'rm -fR "$TMPDIR"' EXIT
TMPDIR=$(mktemp -d)
DTS_FILE="$TMPDIR/type.d.ts"
SCHEMA_FILE="$TMPDIR/schema.json"
echo "export type MyType = $type" > "$DTS_FILE"
npx ts-json-schema-generator --unstable --no-top-ref --path "$DTS_FILE" --type MyType -e none > "$SCHEMA_FILE"
./examples/json_schema_to_grammar.py "$SCHEMA_FILE"