while :; do
case "$1" in
-*) LDARGS="$1"; shift;;
*)  break;;
esac
done
MERGED_SO="$1"; shift
PIC_LIBS="$1"; shift
DEPS="$1"; shift
GCC=${GCC-gcc}
LD=${LD-ld}
OBJDUMP=${OBJDUMP-objdump}
OBJCOPY=${OBJCOPY-objcopy}
DEP_FLAGS_FILE=/tmp/,depflags.$$
NEED_DSYMS_FILE=/tmp/,need.dyn.syms.$$
HAVE_DSYMS_FILE=/tmp/,have.dyn.syms.$$
MERGED_PIC_LIB=/tmp/,libmerged_pic.a.$$
$OBJDUMP --dynamic-syms "$@" 2>/dev/null \
| sed -n 's/^.*\*UND\*.* \([^ ]*\)$/\1/p' \
| sort -u > $NEED_DSYMS_FILE
$OBJDUMP --syms $PIC_LIBS 2>/dev/null \
| sed -n 's/^........ \(g \| w\)   .. .*	[0-9a-f]....... \([^ ]*\)$/\2/p' \
| sort -u > $HAVE_DSYMS_FILE
diff --unchanged-l='%L' --old-l= --new-l= $NEED_DSYMS_FILE $HAVE_DSYMS_FILE \
| sed 's/^/-u/' > $DEP_FLAGS_FILE
$GCC $LDARGS -nostdlib -nostartfiles -shared -Wl,-soname=`basename $MERGED_SO` `cat $DEP_FLAGS_FILE` \
-o $MERGED_SO.uns $PIC_LIBS $DEPS \
&& $OBJCOPY --strip-debug $MERGED_SO.uns $MERGED_SO \
&& rm -f $MERGED_SO.uns