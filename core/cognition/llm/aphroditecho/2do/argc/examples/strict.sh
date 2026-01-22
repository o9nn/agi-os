#!/usr/bin/env bash
set -eu
main() {
    ( set -o posix ; set ) | grep ^argc_
    echo "${argc__fn:-}" "$@"
}
_choice_fn() {
    echo abc
    echo def
	echo ghi
}
eval "$(argc --argc-eval "$0" "$@")"