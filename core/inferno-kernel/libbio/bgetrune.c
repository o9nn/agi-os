#include	"lib9.h"
#include	<bio.h>
long
Bgetrune(Biobuf *bp)
{
int c, i;
Rune rune;
char str[UTFmax];
c = Bgetc(bp);
if(c < Runeself) {
bp->runesize = 1;
return c;
}
str[0] = c;
bp->runesize = 0;
for(i=1;;) {
c = Bgetc(bp);
if(c < 0)
return c;
if (i >= sizeof str)
return Runeerror;
str[i++] = c;
if(fullrune(str, i)) {
bp->runesize = chartorune(&rune, str);
if (rune == Runeerror)
bp->runesize = 0;
else
for(; i > bp->runesize; i--)
Bungetc(bp);
return rune;
}
}
}
int
Bungetrune(Biobuf *bp)
{
if(bp->state == Bracteof)
bp->state = Bractive;
if(bp->state != Bractive)
return Beof;
bp->icount -= bp->runesize;
bp->runesize = 0;
return 1;
}