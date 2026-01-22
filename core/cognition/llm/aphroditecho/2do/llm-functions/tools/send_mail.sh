#!/usr/bin/env bash
set -e
main() {
    sender_name="${EMAIL_SENDER_NAME:-$(echo "$EMAIL_SMTP_USER" | awk -F'@' '{print $1}')}"
    printf "%s\n" "From: $sender_name <$EMAIL_SMTP_USER>
To: $argc_recipient 
Subject: $argc_subject
$argc_body" | \
    curl -fsS --ssl-reqd \
        --url "$EMAIL_SMTP_ADDR" \
        --user "$EMAIL_SMTP_USER:$EMAIL_SMTP_PASS" \
        --mail-from "$EMAIL_SMTP_USER" \
        --mail-rcpt "$argc_recipient" \
        --upload-file -
    echo "Email sent successfully" >> "$LLM_OUTPUT"
}
eval "$(argc --argc-eval "$0" "$@")"