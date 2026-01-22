flags() {
_debug "$@"
}
options() {
_debug "$@"
}
cmd_arg1() {
_debug "$@"
}
cmd_arg2() {
_debug "$@"
}
cmd_arg_with_default() {
_debug "$@"
}
cmd_arg_with_choice() {
_debug "$@"
}
cmd_arg_with_choice_fn() {
_debug "$@"
}
cmd_multi_arg_with_choice_fn_and_comma_sep() {
_debug "$@"
}
cmd_three_required_args() {
_debug "$@"
}
cmd_for_notation() {
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