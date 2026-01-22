#!/usr/bin/env bash
set -e
get_ipinfo() {
curl -fsSL https://httpbin.org/ip >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"