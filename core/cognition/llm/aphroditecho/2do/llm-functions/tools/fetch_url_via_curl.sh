#!/usr/bin/env bash
set -e
main() {
    curl -fsSL "$argc_url" | \
        pandoc -f html-native_divs-native_spans -t gfm-raw_html --wrap=none | \
        sed -E 's/!\[[^]]*\]\([^)]*\)//g' \
        >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"