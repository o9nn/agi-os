#!/bin/sh
MIGCOM="$BUILDDIR/migcom"
TEST_DIR="$SRCDIR/tests"
CFLAGS="-I$TEST_DIR/includes"
failure () {
msg="$1"
echo "ERROR: $msg"
return 0
}
run_mig () {
file="$1"
module="$2"
echo "Generating stubs for $module..."
cpp $file -I$TEST_DIR | $MIGCOM -server $module-server.c -user $module-user.c -header $module-header.h
}
test_module () {
module="$1"
echo "Compiling stubs for $module..."
$CC $CFLAGS -c $module-server.c -o $module-server.o &&
$CC $CFLAGS -c $module-user.c -o $module-user.o
}