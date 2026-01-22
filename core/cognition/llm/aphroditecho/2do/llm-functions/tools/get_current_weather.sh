#!/usr/bin/env bash
set -e
main() {
    curl -fsSL "https://wttr.in/$(echo "$argc_location" | sed 's/ /+/g')?format=4&M" \
    >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"