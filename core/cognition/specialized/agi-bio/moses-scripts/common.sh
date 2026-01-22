my_date() {
    date --rfc-3339=seconds
}
fatalError() {
    echo "[$(my_date)] [ERROR] $@" 1>&2
    exit 1
}
pad() {
    local pad_expression="%0${2}d"
    printf "$pad_expression" "$1"
}
nrows() {
    echo $(($(wc -l < "$1") - 1))
}