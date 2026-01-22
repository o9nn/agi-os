#!/usr/bin/env bash
set -e
main() {
    encoded_query="$(jq -nr --arg q "$argc_query" '$q|@uri')"
    base_url="https://en.wikipedia.org/w/api.php"
    url="$base_url?action=query&list=search&srprop=&srlimit=1&limit=1&srsearch=$encoded_query&srinfo=suggestion&format=json"
    json="$(curl -fsSL "$url")"
    suggestion="$(echo "$json" | jq -r '.query.searchinfo.suggestion // empty')"
    title="$(echo "$json" | jq -r '.query.search[0].title // empty')"
    pageid="$(echo "$json" | jq -r '.query.search[0].pageid // empty')"
    if [[ -z "$title" || -z "$pageid" ]]; then
        echo "error: no results for '$argc_query'" >&2
        exit 1
    fi
    title="$(echo "$title" | tr ' ' '_')"
    url="$base_url?action=query&prop=extracts&explaintext=&titles=$title&exintro=&format=json"
    curl -fsSL "$url" | jq -r '.query.pages["'"$pageid"'"].extract' >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"