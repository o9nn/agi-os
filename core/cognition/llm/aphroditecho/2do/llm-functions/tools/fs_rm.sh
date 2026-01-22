#!/usr/bin/env bash
set -e
ROOT_DIR="${LLM_ROOT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
main() {
if [[ -f "$argc_path" ]]; then
"$ROOT_DIR/utils/guard_path.sh" "$argc_path" "Remove '$argc_path'?"
rm -rf "$argc_path"
fi
echo "Path removed: $argc_path" >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"