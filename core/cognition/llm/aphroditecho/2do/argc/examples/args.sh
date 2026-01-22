cmd() {
    _debug "$@"
}
cmd_alias() {
    _debug "$@"
}
cmd_arg() {
    _debug "$@"
}
cmd_multi_arg() {
    _debug "$@"
}
cmd_required_multi_arg() {
    _debug "$@"
}
cmd_required_arg() {
    _debug "$@"
}
cmd_arg_with_default() {
    _debug "$@"
}
cmd_arg_with_default_fn() {
    _debug "$@"
}
cmd_arg_with_choices() {
    _debug "$@"
}
cmd_arg_with_choices_and_default() {
    _debug "$@"
}
cmd_multi_arg_with_choices() {
    _debug "$@"
}
cmd_required_multi_arg_with_choices() {
    _debug "$@"
}
cmd_arg_with_choice_fn() {
    _debug "$@"
}
cmd_arg_with_choice_fn_and_skip_check() {
    _debug "$@"
}
cmd_required_arg_with_choice_fn() {
    _debug "$@"
}
cmd_multi_arg_with_choice_fn() {
    _debug "$@"
}
cmd_required_multi_arg_with_choice_fn() {
    _debug "$@"
}
cmd_multi_arg_with_choice_fn_and_comma_sep() {
    _debug "$@"
}
cmd_terminaled() {
    _debug "$@"
}
cmd_arg_with_notation() {
    _debug "$@"
}
cmd_two_multi_args() {
    _debug "$@"
}
cmd_one_required_second_required_multi() {
    _debug "$@"
}
cmd_three_required_args() {
    _debug "$@"
}
_debug() {
    ( set -o posix ; set ) | grep ^argc_
    echo "$argc__fn" "$@"
}
_default_fn() {
	echo abc
}
_choice_fn() {
	echo abc
	echo def
	echo ghi
}
eval "$(argc --argc-eval "$0" "$@")"