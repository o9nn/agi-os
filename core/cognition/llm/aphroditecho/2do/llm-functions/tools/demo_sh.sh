#!/usr/bin/env bash
set -e
main() {
    cat <<EOF >> "$LLM_OUTPUT"
string: ${argc_string}
string_enum: ${argc_string_enum}
string_optional: ${argc_string_optional}
boolean: ${argc_boolean}
integer: ${argc_integer}
number: ${argc_number}
array: ${argc_array[@]}
array_optional: ${argc_array_optional[@]}
$(printenv | grep '^LLM_')
EOF
}
eval "$(argc --argc-eval "$0" "$@")"