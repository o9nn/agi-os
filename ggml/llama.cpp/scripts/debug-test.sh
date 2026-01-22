#!/usr/bin/env bash
PROG=${0
build_dir="build-ci-debug"
red=$(tput setaf 1)
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
magenta=$(tput setaf 5)
cyan=$(tput setaf 6)
normal=$(tput sgr0)
print_full_help() {
cat << EOF
Usage: $PROG [OPTION]... <test_regex> (test_number)
Debug specific ctest program.
Options:
-h, --help            display this help and exit
-g                    run in gdb mode
Arguments:
<test_regex>     (Mandatory) Supply one regex to the script to filter tests
(test_number)    (Optional) Test number to run a specific test
Example:
$PROG test-tokenizer
$PROG test-tokenizer 3
EOF
}
abort() {
echo "Error: $1" >&2
cat << EOF >&2
Usage: $PROG [OPTION]... <test_regex> (test_number)
Debug specific ctest program.
Refer to --help for full instructions.
EOF
exit 1
}
check_dependency() {
command -v "$1" >/dev/null 2>&1 || {
abort "$1 is required but not found. Please install it and try again."
}
}
check_dependency ctest
check_dependency cmake
if [ x"$1" = x"-h" ] || [ x"$1" = x"--help" ]; then
print_full_help >&2
exit 0
fi
gdb_mode=false
while getopts "g" opt; do
case $opt in
g)
gdb_mode=true
echo "gdb_mode Mode Enabled"
;;
esac
done
shift $((OPTIND - 1))
if [ -z "${1}" ]; then
abort "Test regex is required"
else
test_suite=${1:-}
fi
test_number=${2:-}
repo_root=$(git rev-parse --show-toplevel)
if [ ! -d "$repo_root" ]; then
abort "Not in a Git repository."
fi
pushd "$repo_root"
rm -rf "$build_dir" && mkdir "$build_dir" || abort "Failed to make $build_dir"
cmake -B "./$build_dir" -DCMAKE_BUILD_TYPE=Debug -DGGML_CUDA=1 -DLLAMA_CURL=1 || abort "Failed to build environment"
pushd "$build_dir"
make -j || abort "Failed to compile"
popd > /dev/null || exit 1
printf "\n\nGathering tests that fit REGEX: ${test_suite} ...\n"
pushd "$build_dir"
tests=($(ctest -R ${test_suite} -V -N | grep -E " +Test +
if [ ${
abort "No tests available... check your compilation process..."
fi
popd > /dev/null || exit 1
if [ -z $test_number ]; then
printf "Which test would you like to debug?\n"
id=0
for s in "${tests[@]}"
do
echo "Test
echo "  $s"
((id++))
done
printf "\nRun test
read test_number
else
printf "\nUser Already Requested
fi
pushd "$build_dir"
sIFS=$IFS
IFS=$'\n'
test_args=($(ctest -R ${test_suite} -V -N | grep "Test command" | cut -d':' -f3 | awk '{$1=$1};1' ))
IFS=$sIFS
popd > /dev/null || exit 1
single_test_name="${tests[test_number]}"
single_test_command="${test_args[test_number]}"
printf "${magenta}Running Test
printf "${cyan}single_test_command: ${single_test_command}${normal}\n"
if [ "$gdb_mode" = "true" ]; then
pushd "$repo_root" || exit 1
eval "gdb --args ${single_test_command}"
popd > /dev/null || exit 1
else
pushd "$repo_root" || exit 1
eval "${single_test_command}"
exit_code=$?
popd > /dev/null || exit 1
printf "${blue}Ran Test
printf "${yellow}Command: ${single_test_command}${normal}\n"
if [ $exit_code -eq 0 ]; then
printf "${green}TEST PASS${normal}\n"
else
printf "${red}TEST FAIL${normal}\n"
fi
fi
popd > /dev/null || exit 1