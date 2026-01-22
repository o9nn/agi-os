#!/bin/sh
USAGE="Usage: $0 [OPTION...] [FROM1 TO1 [FROM2 TO2 [...]] -- [COMMAND...]"
DOC="Execute COMMAND in an environment where some paths are remapped."
REMAPPED=""
while [ "$
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
      echo "STANDARD_HURD_VERSION_remap_"; exit 0;;
    --)
      shift
      break;;
    -*)
      echo 1>&2 "$0: unrecognized option \`$1'"
      echo 1>&2 "Try \`$0 --help' or \`$0 --usage' for more information";
      exit 1;;
    *)
      MAPPED="$MAPPED $1"
      shift;;
  esac
done
if [ $
  set -- ${SHELL:-/bin/sh}
fi
exec /bin/settrans \
     --chroot-chdir "$PWD" \
     --chroot "$@" -- \
     / /hurd/remap $MAPPED