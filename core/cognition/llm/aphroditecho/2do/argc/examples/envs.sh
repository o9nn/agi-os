run() {
    _debug
}
main() {
    _debug
}
_debug() {
    printenv | grep ^TEST_ | sort
}
_default_fn() {
    echo argc
}
_choice_fn() {
    echo abc
    echo def
	echo ghi
}
eval "$(argc --argc-eval "$0" "$@")"