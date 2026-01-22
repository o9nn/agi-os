#!/usr/bin/env bash
set -e
main() {
    encoded_query="$(jq -nr --arg q "$argc_query" '$q|@uri')"
    url="https://api.wolframalpha.com/v2/query?appid=$WOLFRAM_API_ID&input=$encoded_query&output=json&format=plaintext"
    curl -fsSL "$url" | jq '[.queryresult | .pods[] | {title:.title, values:[.subpods[].plaintext | select(. != "")]}]' \
    >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"