#!/usr/bin/env bash
set -e
main() {
    curl -fsS -X POST https://api.perplexity.ai/chat/completions \
     -H "authorization: Bearer $PERPLEXITY_API_KEY" \
     -H "accept: application/json" \
     -H "content-type: application/json" \
     --data '
{
  "model": "'"$PERPLEXITY_WEB_SEARCH_MODEL"'",
  "messages": [
    {
      "role": "user",
      "content": "'"$argc_query"'"
    }
  ]
}
'  | \
        jq -r '.choices[0].message.content' \
        >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"