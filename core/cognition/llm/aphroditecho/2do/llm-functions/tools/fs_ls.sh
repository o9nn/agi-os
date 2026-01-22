#!/usr/bin/env bash
set -e
main() {
ls -1 "$argc_path" >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"