#!/usr/bin/env bash
set -e
ROOT_DIR="${LLM_ROOT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
main() {
    if [ ! -f "$argc_path" ]; then
        echo "Not found file: $argc_path"
        exit 1
    fi
    new_contents="$(awk -f "$ROOT_DIR/utils/patch.awk" "$argc_path" <(printf "%s" "$argc_contents"))"
    printf "%s" "$new_contents" | git diff --no-index "$argc_path" - || true
    "$ROOT_DIR/utils/guard_operation.sh" "Apply changes?"
    printf "%s" "$new_contents" > "$argc_path"
    echo "The patch applied to: $argc_path" >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"