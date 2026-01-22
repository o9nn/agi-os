#!/usr/bin/env bash
set -e
main() {
    encoded_query="$(jq -nr --arg q "$argc_query" '$q|@uri')"
    url="http://export.arxiv.org/api/query?search_query=all:$encoded_query&max_results=$ARXIV_MAX_RESULTS"
    curl -fsSL "$url" >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"