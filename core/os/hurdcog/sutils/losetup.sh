#!/bin/sh
PATH=/bin
usage() {
echo >&2 ...
exit 1
}
offset=0
while [ $
case "$arg" in
-d)
[ $
exec settrans -g -- "$2" /hurd/null
;;
-e)
echo >&2 "$0: encryption not supported"
exit 3
;;
-o)
[ $
offset="$1"
shift
;;
--)
shift
break
;;
-*)
usage
;;
*)
break
;;
esac
done
[ $
device="$1"
file="$2"
create=
case "$device" in
'/dev/loop[0-9]*') ;;
/dev/loop[0-9]*) create=--create ;;
esac
type='-Tfile '
if [ "$offset" != 0 ]; then
blksz=`storeinfo -B -- "$file"`
if [ $[ $offset % $blksz ] -ne 0 ]; then
echo >&2 "$0: offset $offset is not a multiple of device block size $blksz"
exit 1
fi
type="-Tremap $[ $offset / $blksz ]+:file:"
fi
exec settrans $create -gap -- "${device}" /hurd/storeio ${type}"${file}"