#!/usr/bin/env bash
set -e
main() {
curl -fsSL -X POST https://api.tavily.com/search \
-H "content-type: application/json" \
-d '
{
"api_key": "'"$TAVILY_API_KEY"'",
"query": "'"$argc_query"'",
"include_answer": true
}' | \
jq -r '.answer' >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"