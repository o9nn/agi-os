#!/usr/bin/env bash
set -e
main() {
    cat "$argc_path" >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"