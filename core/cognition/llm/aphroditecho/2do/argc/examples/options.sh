options() {
    _debug "$@"
}
flags() {
    _debug "$@"
}
options-one-hyphen() {
    _debug "$@"
}
options-notation-modifier() {
    _debug "$@"
}
options-plus() {
    _debug "$@"
}
flags-plus() {
    _debug "$@"
}
options-mixed() {
    _debug "$@"
}
options-prefixed() {
    _debug "$@"
}
options-assigned() {
    _debug "$@"
}
test1() {
    _debug "$@"
}
test2() {
    _debug "$@"
}
test3() {
    _debug "$@"
}
_debug() {
    ( set -o posix ; set ) | grep ^argc_
    echo "$argc__fn" "$@"
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