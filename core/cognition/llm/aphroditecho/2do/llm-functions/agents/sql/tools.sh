#!/usr/bin/env bash
set -e
ROOT_DIR="${LLM_ROOT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
read_query() {
if ! grep -qi '^select' <<<"$argc_query"; then
echo "error: only SELECT query is allowed" >&2
exit 1
fi
_run_sql "$argc_query"
}
write_query() {
"$ROOT_DIR/utils/guard_operation.sh" "Execute SQL?"
_run_sql "$argc_query"
}
list_tables() {
_run_sql "\dt+"
}
describe_table() {
_run_sql "\d $argc_table_name"
}
_run_sql() {
usql "$LLM_AGENT_VAR_DSN" -c "$1" >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"