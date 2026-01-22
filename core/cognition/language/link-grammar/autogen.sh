#!/bin/sh
if [ ! -f "autogen.sh" ]; then
echo "$0: This script must be run in the top-level directory"
exit 1
fi
run_configure=true
while [ -n "$1" ]
do
case "$1" in
--help|-help|-h)
echo "Usage: $0 [--no-configure] [--clean]"
exit 0
;;
--no-configure)
run_configure=false
shift
;;
--clean)
rm -rf config.cache autom4te*.cache
shift
;;
*)
echo "$0: Error: Unknown flag \"$1\"."
exit 1
esac
done
automake --version >/dev/null &&
automake --version | test "`sed -En '/^automake \(GNU automake\) [^0]\.([4-9]|[1-9][0-9])/p'`"
if [ $? -ne 0 ]; then
echo "$0: Error: you need automake 1.4 or later.  Please upgrade."
exit 1
fi
rm -f autogen.err
OLD_CONFIGURE="${TMPDIR:-/tmp}/configure.$$.tmp"
if [ -f config.cache ] && [ -f configure ]; then
cp configure "$OLD_CONFIGURE"
fi
echo "$0: Creating 'configure'..."
autoreconf -fvi 2>autogen.err
status=$?
if [ $status -ne 0 ]; then
echo ""
echo "$0: * * * Warning: autoreconf returned bad status ($status) - check autogen.err"
echo ""
exit 1
fi
if [ -f config.cache ]; then
(
[ -f "$OLD_CONFIGURE" ] && cmp configure "$OLD_CONFIGURE" > /dev/null 2>&1
) || (
echo "$0: Tossing config.cache, since 'configure' has changed."
rm config.cache
)
rm -f "$OLD_CONFIGURE"
fi
if $run_configure; then
mkdir -p build
cd build
echo
echo "$0: Running 'configure'..."
../configure --enable-maintainer-mode "$@"
status=$?
if [ $status -eq 0 ]; then
echo
echo "$0: Now type 'make' to compile link-grammar (in the 'build' directory)."
else
echo
echo "$0: 'configure' returned a bad status ($status)."
fi
else
echo
echo "$0: Now run 'configure' and 'make' to compile link-grammar."
fi