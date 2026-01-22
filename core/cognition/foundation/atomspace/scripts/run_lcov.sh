#!/bin/bash
builddir=$1
name=$3
dir=$2
echo "test $name" 1>&2
cd $builddir
mkdir -p coverage
rm coverage/$name.info
lcov --directory . --zerocounter
$dir/$name
ret_val=$?
lcov --directory . --capture --output-file coverage/$name.info --test-name $name
lcov --remove coverage/$name.info "/usr*" -o coverage/$name.info
lcov --remove coverage/$name.info "*/tests/*" -o coverage/$name.info
lcov --remove coverage/$name.info "*/build/*" -o coverage/$name.info
exit $ret_val