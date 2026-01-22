#!/bin/sh
PATH=/bin:/sbin
export PATH
umask 022
fallback_shells='/bin/bash /bin/sh /bin/dash /bin/csh /bin/ash /bin/shd'
SHELL=/bin/sh
init=/hurd/init
reopen_console ()
{
exec 1>/dev/console 2>&1 || exit 3
}
trap 'reopen_console' 32
singleuser ()
{
test $
for try in ${fallback_shells}; do
SHELL=${try}
exec ${SHELL}
done
exit 127
}
echo
echo Starting runsystem
if ! test -c /servers/socket/1 && command -v settrans >/dev/null ; then
echo Setting up pflocal
if fsysopts / --update --writable ; then
settrans -c /servers/socket/1 /hurd/pflocal
else
singleuser "Failed to create /servers/socket/1."
fi
fi
if [ "${FALLBACK_CONSOLE+set}" = set ]; then
singleuser "Running on fallback console ${FALLBACK_CONSOLE}"
fi
flags=
single=
while [ $
arg="$1"
shift
case "$arg" in
--*) ;;
init=*)
eval "${arg}"
;;
*=*) ;;
-*)
flags="${flags}${arg
;;
'single')
single="-s"
;;
'fastboot'|'emergency')
;;
esac
done
case "$flags" in
*s*)
single="-s"
;;
esac
fsysopts / --update --readonly
echo Starting ${init}
exec ${init} ${single} -a