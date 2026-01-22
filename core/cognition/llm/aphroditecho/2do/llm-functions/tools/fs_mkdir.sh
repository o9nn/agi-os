#!/usr/bin/env bash
set -e
main() {
mkdir -p "$argc_path"
echo "Directory created: $argc_path" >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"