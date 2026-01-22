eval "$(argc --argc-eval "$0" "$@")"
_debug() {
( set -o posix ; set ) | grep ^argc_
echo "$argc__fn" "$@"
}
_debug