#!/bin/bash
PATH=/bin:/usr/bin
ECHO=:
EXEC=""
DEVDIR=`pwd`
STFLAGS="-g"
KEEP=
USE_PARTSTORE=
MASTER=
while :; do
case "$1" in
--help|"-?")
echo "\
Usage: $0 [OPTION...] DEVNAME...
Make filesystem nodes for accessing standard system devices
-D, --devdir=DIR           Use DIR when a device node name must be
embedded in a translator; default is the cwd
-M, --master-device=FILE   Use FILE as master device node.
-k, --keep-active          Leave any existing active translator running
-K, --keep-all             Don't overwrite existing files
-p, --parted               Prefer user-space parted stores to kernel devices
for partition devices
-n, --dry-run              Don't actually execute any commands
-v, --verbose              Show what commands are executed to make the devices
-?, --help                 Give this help list
--usage                Give a short usage message
-V, --version              Print program version"
exit 0;;
--devdir)   DEVDIR="$2"; shift 2;;
--devdir=*) DEVDIR="${1
-D)         DEVDIR="$2"; shift 2;;
-D*)        DEVDIR="${1
--master-device)   MASTER="$2":; shift 2;;
--master-device=*) MASTER="${1
-M)         MASTER="$2":; shift 2;;
-M*)        MASTER="${1
--keep-active|-k) STFLAGS="-k"; shift;;
--keep-all|-K) KEEP=1; shift;;
--parted|-p) USE_PARTSTORE=1; shift;;
--verbose|-v) ECHO=echo; shift;;
--dry-run|-n) EXEC=:; shift;;
-nv|-vn)      ECHO=echo; EXEC=:; shift;;
--usage)
echo "Usage: $0 [-V?] [-D DIR] [--help] [--usage] [--version] [--parted]"
echo "                [--devdir=DIR] [--keep-active] [--keep-all] DEVNAME..."
exit 0;;
--version|-V)
echo "STANDARD_HURD_VERSION_MAKEDEV_"; exit 0;;
-*)
echo 1>&2 "$0: unrecognized option \`$1'"
echo 1>&2 "Try \`$0 --help' or \`$0 --usage' for more information";
exit 1;;
*)
break;;
esac
done
case  "$
echo 1>&2 "Usage: $0 [OPTION...] DEVNAME..."
echo 1>&2 "Try \`$0 --help' or \`$0 --usage' for more information"
exit 1;;
esac
cmd() {
eval $ECHO "$@"
eval $EXEC "$@"
}
st() {
local NODE="$1"
local OWNER="$2"
local PERM="$3"
local NODE_TYPE="$4"
shift 4
if [ "$KEEP" ] && showtrans "$NODE" > /dev/null 2>&1 ; then
return;
fi
if [ ! -e "$NODE" ]; then
case "$NODE_TYPE" in
b|c)
cmd mknod "$NODE" "$NODE_TYPE" 0 0
;;
d)
cmd mkdir "$NODE"
;;
*)
lose "Unknown node type $NODE_TYPE for $NODE"
;;
esac
fi
if cmd settrans $STFLAGS -c "$NODE"; then
cmd chown "$OWNER" "$NODE"
cmd chmod "$PERM" "$NODE"
cmd settrans $STFLAGS "$NODE" "$@"
fi
}
lose() {
local line
for line; do
echo 1>&2 "$0: $line"
done
exit 1
}
mkdev() {
local I
for I; do
case $I in
/* | */*)
lose "Device names cannot contain directories" \
"Change to target directory and run $0 from there."
;;
std)
mkdev console tty random urandom null zero full fd time mem klog shm
;;
console|com[0-9])
st $I root 600 c /hurd/term ${DEVDIR}/$I device $I;;
vcs)
st $I root 600 d /hurd/console;;
tty[1-9][0-9]|tty[1-9])
st $I root 600 c /hurd/term ${DEVDIR}/$I hurdio \
${DEVDIR}/vcs/`echo $I | sed -e s/tty//`/console;;
lpr[0-9])
st $I root 660 c /hurd/streamio "$I";;
rtc)
st $I root 644 c /hurd/rtc;;
random)
st $I root 644 c /hurd/random --seed-file /var/lib/random-seed;;
urandom)
cmd ln -f -s random $I;;
null)
st $I root 666 c /hurd/null;;
full)
st $I root 666 c /hurd/null --full;;
zero)
st $I root 666 c /bin/nullauth -- /hurd/storeio -Tzero;;
tty)
st $I root 666 c /hurd/magic tty;;
fd)
st $I root 666 d /hurd/magic --directory fd
cmd ln -f -s fd/0 stdin
cmd ln -f -s fd/1 stdout
cmd ln -f -s fd/2 stderr
;;
'time')
st $I root 644 c /hurd/storeio --no-cache time ;;
mem)
st $I root 660 c /hurd/storeio --no-cache mem ;;
klog)
st $I root 660 c /hurd/streamio kmsg;;
[pt]ty[pqrstuvwxyzPQRS]?)
local id="${I
st pty$id root 666 c /hurd/term ${DEVDIR}/pty$id \
pty-master ${DEVDIR}/tty$id
st tty$id root 666 c /hurd/term ${DEVDIR}/tty$id \
pty-slave ${DEVDIR}/pty$id
;;
[pt]ty[pqrstuvwxyzPQRS])
local n
for n in 0 1 2 3 4 5 6 7 8 9 \
a b c d e f g h i j k l m n o p q r s t u v; do
mkdev ${I}${n}
done
;;
fd*|mt*)
st $I root 640 b /hurd/storeio $I
;;
rumpdisk)
st $I root 660 c /hurd/rumpdisk
cmd ln -f -s rumpdisk disk
;;
rumpusbdisk)
st $I root 660 c /hurd/rumpusbdisk
cmd ln -f -s rumpusbdisk usbdisk
;;
[hrscwu]d*|ucd*)
local sliceno=
local n="${I
local major="${n%%[!0-9]*}"
if [ -z "$major" ]; then
lose "$I: Invalid device name: must supply a device number"
fi
local minor="${n
case "$minor" in
'') ;;
[a-z]) ;;
s[1-9]*)
local slicestuff="${minor
local slice="${slicestuff%%[!0-9]*}"
local rest="${slicestuff
case "$slice" in
[1-9] | [1-9][0-9]) ;;
*)
lose "$I: Invalid slice number \`$slice'"
;;
esac
case "$rest" in
'')
sliceno=$slice
;;
[a-z]) ;;
*)
lose "$I: Invalid partition \`$rest'"
;;
esac
;;
*)
lose "$I: Invalid slice or partition syntax"
;;
esac
dev=$I
case "$I" in
wd*|cd*)
USE_PARTSTORE=1
MASTER=@/dev/disk:
;;
ucd*)
USE_PARTSTORE=1
MASTER=@/dev/usbdisk:
dev=${dev
;;
ud*)
USE_PARTSTORE=1
MASTER=@/dev/usbdisk:
dev=s${dev
;;
esac
if [ "$USE_PARTSTORE" ] && [ -z "$rest" ] && [ "$sliceno" ]; then
local drive=${dev%s[0-9]*}
st $I root 640 b /hurd/storeio -T typed part:$sliceno:device:$MASTER$drive
else
st $I root 640 b /hurd/storeio $MASTER$dev
fi
;;
netdde)
st $I root 660 c /hurd/netdde
cmd ln -f -s netdde net
;;
eth*)
st $I root 660 c /hurd/devnode -M /dev/net $I;;
shm)
if [ ! -e "/dev/$I" ]; then
ln -s /tmp /dev/$I
fi
;;
pseudo-root)
st $I root 640 b /hurd/storeio $I
;;
loop*)
st $I root 640 c /hurd/null
;;
*)
lose "$I: Unknown device name"
;;
esac
done
}
mkdev "$@"