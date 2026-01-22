#!/usr/bin/env bash
set -eo pipefail
builtin cd "$(dirname "${BASH_SOURCE:-$0}")"
ROOT="$(git rev-parse --show-toplevel)"
builtin cd "$ROOT" || exit 1
RUFF_VERSION=$(ruff --version | awk '{print $2}')
MYPY_VERSION=$(mypy --version | awk '{print $2}')
CODESPELL_VERSION=$(codespell --version)
ISORT_VERSION=$(isort --vn)
CLANGFORMAT_VERSION=$(clang-format --version | awk '{print $3}')
tool_version_check() {
    if [[ $2 != $3 ]]; then
        echo "Wrong $1 version installed: $3 is required, not $2."
        exit 1
    fi
}
tool_version_check "ruff" $RUFF_VERSION "$(grep "ruff==" requirements/lint.txt | cut -d'=' -f3)"
tool_version_check "isort" "$ISORT_VERSION" "$(grep isort requirements/lint.txt | cut -d'=' -f3)"
tool_version_check "codespell" "$CODESPELL_VERSION" "$(grep codespell requirements/lint.txt | cut -d'=' -f3)"
tool_version_check "clang-format" "$CLANGFORMAT_VERSION" "$(grep clang-format requirements/lint.txt | cut -d'=' -f3)"
CODESPELL_EXCLUDES=(
    '--skip' './tests/benchmarks/sonnet.txt,build/**'
)
spell_check() {
    codespell "$@"
}
spell_check_all(){
  codespell --toml pyproject.toml "${CODESPELL_EXCLUDES[@]}"
}
spell_check_changed() {
    MERGEBASE="$(git merge-base origin/main HEAD)"
    if ! git diff --diff-filter=ACM --quiet --exit-code "$MERGEBASE" -- '*.py' '*.pyi' &>/dev/null; then
        git diff --name-only --diff-filter=ACM "$MERGEBASE" -- '*.py' '*.pyi' | xargs \
            codespell "${CODESPELL_EXCLUDES[@]}"
    fi
}
if [[ "$1" == '--files' ]]; then
   spell_check "${@:2}"
elif [[ "$1" == '--all' ]]; then
   spell_check_all
else
   spell_check_changed
fi
echo 'Aphrodite codespell: Done'
lint() {
    ruff "$@"
}
lint_changed() {
    MERGEBASE="$(git merge-base origin/main HEAD)"
    if ! git diff --diff-filter=ACM --quiet --exit-code "$MERGEBASE" -- '*.py' '*.pyi' &>/dev/null; then
        git diff --name-only --diff-filter=ACM "$MERGEBASE" -- '*.py' '*.pyi' | xargs \
             ruff
    fi
}
if [[ "$1" == '--files' ]]; then
   lint "${@:2}"
elif [[ "$1" == '--all' ]]; then
   lint aphrodite tests
else
   lint_changed
fi
echo 'Aphrodite ruff: Done'
isort_check() {
    isort "$@"
}
isort_check_all(){
  isort .
}
isort_check_changed() {
    MERGEBASE="$(git merge-base origin/main HEAD)"
    if ! git diff --diff-filter=ACM --quiet --exit-code "$MERGEBASE" -- '*.py' '*.pyi' &>/dev/null; then
        git diff --name-only --diff-filter=ACM "$MERGEBASE" -- '*.py' '*.pyi' | xargs \
             isort
    fi
}
if [[ "$1" == '--files' ]]; then
   isort_check "${@:2}"
elif [[ "$1" == '--all' ]]; then
   isort_check_all
else
   isort_check_changed
fi
echo 'Aphrodite isort: Done'
CLANG_FORMAT_EXCLUDES=(
    'kernels/moe/softmax.cu'
    'kernels/punica/bgmv/bgmv_bf16_bf16_bf16.cu'
    'kernels/punica/bgmv/bgmv_config.h'
    'kernels/punica/bgmv/bgmv_impl.cuh'
    'kernels/punica/bgmv/vec_dtypes.cuh'
    'kernels/punica/punica_ops.cu'
    'kernels/punica/type_convert.h'
    'kernels/quantization/gguf/ggml-common.h'
    'kernels/quantization/gguf/dequantize.cuh'
    'kernels/quantization/gguf/vecdotq.cuh'
    'kernels/quantization/gguf/mmq.cuh'
    'kernels/quantization/gguf/mmvq.cuh'
)
clang_format() {
    clang-format -i "$@"
}
clang_format_changed() {
    MERGEBASE="$(git merge-base origin/main HEAD)"
    changed_files=$(git diff --name-only --diff-filter=ACM "$MERGEBASE" -- '*.h' '*.cpp' '*.cu' '*.cuh' | grep -vFf <(printf "%s\n" "${CLANG_FORMAT_EXCLUDES[@]}"))
    if [ -n "$changed_files" ]; then
        echo "$changed_files" | xargs -P 5 clang-format -i
    fi
}
clang_format_all() {
    find kernels/ \( -name '*.h' -o -name '*.cpp' -o -name '*.cu' -o -name '*.cuh' \) -print \
        | grep -vFf <(printf "%s\n" "${CLANG_FORMAT_EXCLUDES[@]}") \
        | xargs clang-format -i
}
if [[ "$1" == '--files' ]]; then
   clang_format "${@:2}"
elif [[ "$1" == '--all' ]]; then
   clang_format_all
else
   clang_format_changed
fi
echo 'Aphrodite clang-format: Done'
if ! git diff --quiet &>/dev/null; then
    echo 'Reformatted files. Please review and stage the changes.'
    echo 'Changes not staged for commit:'
    echo
    git --no-pager diff --name-only
    exit 1
fi