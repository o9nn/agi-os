upload() {
    echo "cmd                       upload"
    echo "arg:  target              $argc_target"
}
download() {
    echo "cmd:                      download"
    echo "flag:   --force           $argc_force"
    echo "option: --tries           $argc_tries"
    echo "arg:    source            $argc_source"
    echo "arg:    target            $argc_target"
}
eval "$(argc --argc-eval "$0" "$@")"