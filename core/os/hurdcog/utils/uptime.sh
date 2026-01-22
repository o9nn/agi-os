#!/bin/sh
USAGE="Usage: $0 [OPTION...]"
DOC="Show system uptime, number of users, and load"
W=${W-/bin/w}
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
      echo "STANDARD_HURD_VERSION_uptime_"; exit 0;;
    -*)
      echo 1>&2 "$0: unrecognized option \`$1'"
      echo 1>&2 "Try \`$0 --help' or \`$0 --usage' for more information";
      exit 1;;
    *)
      break;;
  esac
done
case "$
  0) :;;
  *) 
      echo 1>&2 "$0: too many arguments"
      echo 1>&2 "Try \`$0 --help' or \`$0 --usage' for more information";
      exit 1;;
esac
$W -u