upload() {
echo upload "$@"
}
download() {
echo download "$@"
}
eval "$(argc --argc-eval "$0" "$@")"