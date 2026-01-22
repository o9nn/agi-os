#!/usr/bin/env bash
set -e
test() {
    cargo test "$@"
}
test-features() {
    cargo hack --no-dev-deps check --feature-powerset --depth 2 --lib
}
check() {
    cargo fmt --all --check
    cargo clippy --all
    cargo test
}
fix() {
    cargo fmt --all
    cargo clippy --fix --all --allow-dirty
}
setup-shell() {
    case $argc_shell in
        bash) echo "source <(argc --argc-completions bash ${argc_cmds[@]})" ;;
        elvish) echo "eval (argc --argc-completions elvish ${argc_cmds[@]} | slurp)" ;;
        fish) echo "argc --argc-completions fish ${argc_cmds[@]} | source" ;;
        nushell) echo "argc --argc-completions nushell | save -f argc.nu"$'\n'"source argc.nu" ;;
        powershell) echo "argc --argc-completions powershell ${argc_cmds[@]} | Out-String | Invoke-Expression" ;;
        xonsh) echo "exec(\$(argc --argc-completions xonsh ${argc_cmds[@]}))" ;;
        zsh) echo "source <(argc --argc-completions zsh ${argc_cmds[@]})" ;;
        tcsh) echo "eval \`argc --argc-completions tcsh ${argc_cmds[@]}\`" ;;
    esac
}
eval "$(argc --argc-eval "$0" "$@")"