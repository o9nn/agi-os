build () {
:;
}
check() {
:;
}
_choice_toolchain() {
cat <<-'EOF'
stable
beta
nightly
EOF
}
eval "$(argc --argc-eval "$0" "$@")"