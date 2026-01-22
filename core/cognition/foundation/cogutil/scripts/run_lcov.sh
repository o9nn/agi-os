#!/bin/bash
bindir=$1
name=$3
dir=$2
echo "test $name" 1>&2
cd $bindir
mkdir -p coverage
rm coverage/$name.info
lcov --directory . --zerocounter
$dir/$name
ret_val=$?
lcov --directory . --capture --output-file coverage/$name.info --test-name $name
exit $ret_val