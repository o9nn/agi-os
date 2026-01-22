#!/bin/sh
ROOT=/usr/inferno
SYSTARG=FreeBSD
OBJTYPE=386
SYSTYPE=posix
grep -s 'SYSTARG=Plan9' mkconfig || . ./mkconfig
PLAT=$ROOT/$SYSTARG/$OBJTYPE
CC="p gcc -c -I$PLAT/include -I$ROOT/include -I$ROOT/utils/include"
LD="p gcc"
AR="p ar crvs"
RANLIB=":"
error() {
echo $* >&2
exit 1
}
ofiles() {
echo $* | sed 's/\.c/.o/g'
}
p() {
echo $*
"$@"
}
echo removing old libraries and binaries
rm -f $PLAT/lib/*.a $PLAT/bin/*
rm -f utils/cc/y.tab.?
mkdir -p $PLAT/lib $PLAT/bin
cd $ROOT/utils/libregexp || error cannot find libregexp directory
CFILES="regaux.c regcomp.c regerror.c regexec.c regsub.c rregexec.c rregsub.c"
$CC $CFILES || error libregexp compilation failed
$AR $PLAT/lib/libregexp.a `ofiles $CFILES` || error libregexp ar failed
$RANLIB $PLAT/lib/libregexp.a || error libregexp ranlib failed
cd $ROOT/libbio || error cannot find libbio directory
$CC *.c || error libbio compilation failed
$AR $PLAT/lib/libbio.a *.o || error libbio ar failed
$RANLIB $PLAT/lib/libbio.a || error libbio ranlib failed
cd $ROOT/lib9 || error cannot find lib9 directory
CFILES="dirstat-$SYSTYPE.c rerrstr.c errstr-$SYSTYPE.c getuser-$SYSTYPE.c"
CFILES="$CFILES charstod.c cleanname.c create.c dirwstat.c *print*.c *fmt*.c exits.c getfields.c  pow10.c print.c qsort.c rune.c runestrlen.c seek.c strdup.c strtoll.c utflen.c utfrrune.c utfrune.c utf*.c *str*cpy*.c"
$CC $CFILES || error lib9 compilation failed
$AR $PLAT/lib/lib9.a `ofiles $CFILES` || error lib9 ar failed
$RANLIB $PLAT/lib/lib9.a || error lib9 ranlib failed
cd $ROOT/utils/mk
CFILES="Posix.c sh.c"
CFILES="$CFILES arc.c archive.c bufblock.c env.c file.c graph.c job.c lex.c main.c match.c mk.c parse.c recipe.c rule.c run.c shprint.c symtab.c var.c varsub.c word.c"
$CC $CFILES || error mk compilation failed
$LD -o mk `ofiles $CFILES` $PLAT/lib/libregexp.a $PLAT/lib/libbio.a $PLAT/lib/lib9.a || error mk link failed
cp mk $PLAT/bin || error mk binary install failed
echo mk binary built successfully!