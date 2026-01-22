#!/bin/sh
PATH=/bin:/sbin
export PATH
umask 022
fallback_shells='/bin/bash /bin/sh /bin/dash /bin/csh /bin/ash /bin/shd'
SHELL=/bin/sh
prefix=
exec_prefix=${prefix}
RUNCOM=${exec_prefix}/libexec/rc
RUNTTYS=${exec_prefix}/libexec/runttys
runttys_sigs='TERM INT HUP TSTP'
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
while [ $
  arg="$1"
  shift
  case "$arg" in
  --*) ;;
  *=*) ;;
  -*)
    flags="${flags}${arg
    ;;
  'single')
    flags="${flags}s"
    ;;
  'fastboot'|'emergency')
    flags="${flags}f"
    ;;
  esac
done
case "$flags" in
*s*)
  rc=false
  ;;
*f*)
  rc="${RUNCOM}"
  ;;
*)
  rc="${RUNCOM} autoboot"
  ;;
esac
while : ; do
  until $rc; do
    rc=${RUNCOM}
    until ${SHELL} || test $? -lt 128; do
      :
    done
  done
  runttys_pid=0
  for sig in $runttys_sigs; do
    trap "kill -$sig \${runttys_pid}" $sig
  done
  ${RUNTTYS} &
  runttys_pid=$!
  wait
  rc=false
done