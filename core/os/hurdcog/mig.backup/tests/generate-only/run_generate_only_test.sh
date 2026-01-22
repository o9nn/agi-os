#!/bin/sh
. $SRCDIR/tests/test_lib.sh
file=$1
module="$(basename $file .defs)"
if ! run_mig $file $module; then
  failure "Could not generate stubs for $module"
  exit 1
fi