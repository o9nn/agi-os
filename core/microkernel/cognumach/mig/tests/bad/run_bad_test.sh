#!/bin/sh
. $SRCDIR/tests/test_lib.sh
file=$1
module="$(basename $file .defs)"
if run_mig $file $module; then
  failure "$module was supposed to fail"
  exit 1
fi