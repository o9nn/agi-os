#!/usr/bin/env bash
set -e
main() {
from_number="$TWILIO_FROM_NUMBER"
to_number="$argc_to_number"
if [[ "$to_number" == 'whatsapp:'* ]]; then
from_number="whatsapp:$from_number"
fi
if [[ "$to_number" != 'whatsapp:'* && "$to_number" != '+'*  ]]; then
to_number="+$to_number"
fi
res="$(curl -s -X POST "https://api.twilio.com/2010-04-01/Accounts/$TWILIO_ACCOUNT_SID/Messages.json" \
-u "$TWILIO_ACCOUNT_SID:$TWILIO_AUTH_TOKEN" \
-w "\n%{http_code}" \
--data-urlencode "From=$from_number" \
--data-urlencode "To=$to_number" \
--data-urlencode "Body=$argc_message")"
status="$(echo "$res" | tail -n 1)"
body="$(echo "$res" | head -n -1)"
if [[ "$status" -ge 200 && "$status" -lt 300 ]]; then
if [[ "$(echo "$body" | jq -r 'has("sid")')" == "true" ]]; then
echo "Message sent successfully" >> "$LLM_OUTPUT"
else
_die "error: $body"
fi
else
_die "error: $body"
fi
}
_die() {
echo "$*" >&2
exit 1
}
eval "$(argc --argc-eval "$0" "$@")"