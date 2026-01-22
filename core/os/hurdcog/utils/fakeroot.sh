#!/bin/sh
USAGE="Usage: $0 [OPTION...] [COMMAND...]"
DOC="Execute COMMAND in an environment where it appears to be root."
while :; do
case "$1" in
--help|"-?")
echo "$USAGE"
echo "$DOC"
echo ""
echo "  -?, --help                 Give this help list"
echo "      --usage                Give a short usage message"
echo "  -V, --version              Print program version"
exit 0;;
--usage)
echo "Usage: $0 [-V?] [--help] [--usage] [--version]"
exit 0;;
--version|-V)
echo "STANDARD_HURD_VERSION_fakeroot_"; exit 0;;
--)
shift
break;;
-*)
echo 1>&2 "$0: unrecognized option \`$1'"
echo 1>&2 "Try \`$0 --help' or \`$0 --usage' for more information";
exit 1;;
*)
break;;
esac
done
if [ $
set -- ${SHELL:-/bin/sh}
fi
FAKED_MODE="unknown-is-root"
export FAKED_MODE
exec /bin/settrans \
--chroot-chdir "$PWD" \
--chroot /bin/fakeauth "$@" -- \
/ /hurd/fakeroot