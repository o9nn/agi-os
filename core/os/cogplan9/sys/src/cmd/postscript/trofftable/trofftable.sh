#!/bin/sh
POSTBIN=/usr/lbin/postscript
POSTLIB=/usr/lib/postscript
FONTDIR=/usr/lib/font
PROLOGUE=$POSTLIB/trofftable.ps
DPOSTPROLOGUE=$POSTLIB/dpost.ps
COPYFILE=
HOSTFONTDIR=
DEVICE=
LIBRARY=
TEMPLATE=
SLOWDOWN=25
STARTCOMMENTS=256
NONCONFORMING="%!PS"
ENDPROLOG="%%EndProlog"
BEGINSETUP="%%BeginSetup"
ENDSETUP="%%EndSetup"
TRAILER="%%Trailer"
while [ -n "$1" ]; do
    case $1 in
	-C)  shift; COPYFILE="$COPYFILE $1";;
	-C*) COPYFILE="$COPYFILE `echo $1 | sed s/-C//`";;
	-F)  shift; FONTDIR=$1;;
	-F*) FONTDIR=`echo $1 | sed s/-F//`;;
	-H)  shift; HOSTFONTDIR=$1;;
	-H*) HOSTFONTDIR=`echo $1 | sed s/-H//`;;
	-L)  shift; PROLOGUE=$1;;
	-L*) PROLOGUE=`echo $1 | sed s/-L//`;;
	-S)  shift; LIBRARY=$1;;
	-S*) LIBRARY=`echo $1 | sed s/-S//`;;
	-T)  shift; DEVICE=$1;;
	-T*) DEVICE=`echo $1 | sed s/-T//`;;
	-c)  shift; STARTCOMMENTS=$1;;
	-c*) STARTCOMMENTS=`echo $1 | sed s/-c//`;;
	-o)  shift; OCTALESCAPES=$1;;
	-o*) OCTALESCAPES=`echo $1 | sed s/-o//`;;
	-s)  shift; SLOWDOWN=$1;;
	-s*) SLOWDOWN=`echo $1 | sed s/-s//`;;
	-t)  shift; TEMPLATE=$1;;
	-t*) TEMPLATE=`echo $1 | sed s/-t//`;;
	-*)  echo "$0: illegal option $1" >&2; exit 1;;
	*)   break;;
    esac
    shift
done
if [ ! "$DEVICE" -a ! "$LIBRARY" ]; then
    echo "$0: no device or shell library" >&2
    exit 1
fi
if [ $
    echo "$0: bad argument count" >&2
    exit 1
fi
if [ -d "$HOSTFONTDIR" -a -f "$HOSTFONTDIR/$1" ]; then
    COPYFILE="$COPYFILE $HOSTFONTDIR/$1"
fi
. ${LIBRARY:-${FONTDIR}/dev${DEVICE}/shell.lib}
if [ $
    then TEMPLATE=$1
    else TEMPLATE=${TEMPLATE:-R}
fi
CMD=`BuiltinTables | awk '$2 == template"" {
	if ( pname == "" )
		pname = $3
	printf "%s %s %s", $1, tname, pname
	exit 0
}' template="$TEMPLATE" tname="$1" pname="$2"`
if [ ! "$CMD" ]; then
    echo "$0: $TEMPLATE not found" >&2
    exit 1
fi
echo $NONCONFORMING
cat $PROLOGUE
echo "/DpostPrologue 100 dict dup begin"
cat $DPOSTPROLOGUE
echo "end def"
echo $ENDPROLOG
echo $BEGINSETUP
cat ${COPYFILE:-/dev/null}
echo "/slowdown $SLOWDOWN def"
echo "/startcomments $STARTCOMMENTS def"
echo $ENDSETUP
$CMD
echo $TRAILER