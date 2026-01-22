#include <u.h>
#include <libc.h>
#include <bio.h>
#include "../common/common.h"
#include "tr2post.h"
int hpos = 0, vpos = 0;
int fontsize, fontpos;
#define MAXSTR	128
int trindex;
static int expecthmot = 0;
void
initialize(void) {
}
void
hgoto(int x) {
hpos = x;
if (pageon()) {
endstring();
}
}
void
vgoto(int y) {
vpos = y;
if (pageon()) {
endstring();
}
}
void
hmot(int x) {
int delta;
if ((x<expecthmot-1) || (x>expecthmot+1)) {
delta = x - expecthmot;
if (curtrofffontid <0 || curtrofffontid >= troffontcnt) {
fprint(2, "troffontcnt=%d curtrofffontid=%d\n",
troffontcnt, curtrofffontid);
exits("");
}
if (delta == troffontab[curtrofffontid].spacewidth*fontsize/10 &&
isinstring()) {
if (pageon())
runeout(' ');
} else {
if (pageon()) {
endstring();
if (debug)
fprint(2, "x=%d expecthmot=%d\n",
x, expecthmot);
}
}
}
hpos += x;
expecthmot = 0;
}
void
vmot(int y) {
endstring();
vpos += y;
}
struct charent **
findglyph(int trfid, Rune rune, char *stoken) {
struct charent **cp;
for (cp = &(troffontab[trfid].charent[RUNEGETGROUP(rune)][RUNEGETCHAR(rune)]); *cp != 0; cp = &((*cp)->next)) {
if ((*cp)->name) {
if (debug) fprint(2, "looking for <%s>, have <%s> in font %s\n", stoken, (*cp)->name, troffontab[trfid].trfontid);
if (strcmp((*cp)->name, stoken) == 0)
break;
}
}
return(cp);
}
void
glyphout(Rune rune, char *stoken, BOOLEAN specialflag) {
struct charent **cp;
struct troffont *tfp;
struct psfent *psfp;
int i, t, mi, wid;
int fontid;
Rune r;
mi = 0;
settrfont();
fontid = curtrofffontid;
if (debug)
fprint(2, "\tlooking through current font: trying %s\n",
troffontab[fontid].trfontid);
cp = findglyph(fontid, rune, stoken);
if (*cp != 0)
goto foundit;
if (specialflag) {
if (expecthmot)
hmot(0);
for (mi=0; mi<fontmnt; mi++) {
if (troffontab[fontid].trfontid==0)
error(WARNING, "glyphout:troffontab[%d].trfontid=0x%x, botch!\n",
fontid, troffontab[fontid].trfontid);
if (fontmtab[mi]==0) {
if (debug)
fprint(2, "fontmtab[%d]=%#p, fontmnt=%d\n",
mi, fontmtab[mi], fontmnt);
continue;
}
if (strcmp(troffontab[fontid].trfontid, fontmtab[mi])==0)
break;
}
if (mi==fontmnt)
error(FATAL, "current troff font is not mounted, botch!\n");
for (i=(mi+1)%fontmnt; i!=mi; i=(i+1)%fontmnt) {
if (fontmtab[i]==0) {
if (debug)
fprint(2, "fontmtab[%d]=%#p, fontmnt=%d\n",
i, fontmtab[i], fontmnt);
continue;
}
fontid = findtfn(fontmtab[i], TRUE);
if (debug) fprint(2, "	looking through special fonts: trying %s\n", troffontab[fontid].trfontid);
if (troffontab[fontid].special) {
cp = findglyph(fontid, rune, stoken);
if (*cp != 0)
goto foundit;
}
}
if (mi != 1) {
fontid = findtfn(fontmtab[1], TRUE);;
if (debug) fprint(2, "	looking through font at position 1: trying %s\n", troffontab[fontid].trfontid);
cp = findglyph(fontid, rune, stoken);
if (*cp != 0) goto foundit;
}
}
if (*cp == 0) {
error(WARNING, "cannot find glyph, rune=0x%x stoken=<%s> troff font %s\n", rune, stoken,
troffontab[curtrofffontid].trfontid);
expecthmot = 0;
}
rune = 'p';	stoken = "pw";
for (i=(mi+1)%fontmnt; i!=mi; i=(i+1)%fontmnt) {
if (fontmtab[i]==0) {
if (debug) fprint(2, "fontmtab[%d]=%#p\n", i, fontmtab[i]);
continue;
}
fontid = findtfn(fontmtab[i], TRUE);
if (debug) fprint(2, "	looking through special fonts: trying %s\n", troffontab[fontid].trfontid);
if (troffontab[fontid].special) {
cp = findglyph(fontid, rune, stoken);
if (*cp != 0) goto foundit;
}
}
if (*cp == 0) {
error(WARNING, "cannot find glyph, rune=0x%x stoken=<%s> troff font %s\n",
rune, stoken, troffontab[curtrofffontid].trfontid);
expecthmot = 0;
return;
}
foundit:
t = (((*cp)->postfontid&0xff)<<8) | ((*cp)->postcharid&0xff);
if (debug)
fprint(2, "runeout(0x%x)<%C> postfontid=0x%x postcharid=0x%x troffcharwidth=%d\n",
rune, rune, (*cp)->postfontid, (*cp)->postcharid,
(*cp)->troffcharwidth);
tfp = &troffontab[fontid];
psfp = nil;
for (i=0; i<tfp->psfmapsize; i++) {
psfp = &(tfp->psfmap[i]);
if(t>=psfp->start && t<=psfp->end)
break;
}
if (i >= tfp->psfmapsize)
error(FATAL, "character <0x%x> does not have a Postscript font defined.\n", rune);
setpsfont(psfp->psftid, fontsize);
if (t == 0x0001) {
endstring();
if (pageon()) {
Bprint(Bstdout, "%d %d m ", hpos, vpos);
wid = chartorune(&r, (*cp)->name);
if(' '<r && r<0x7F)
Bprint(Bstdout, "%d build_%s\n",
(*cp)->troffcharwidth, (*cp)->name);
else{
if((*cp)->name[wid] != 0)
error(FATAL, "character <%s> badly named\n", (*cp)->name);
Bprint(Bstdout, "%d build_X%.4x\n",
(*cp)->troffcharwidth, r);
}
for (i=0; i<build_char_cnt; i++)
if (*cp == build_char_list[i])
break;
if (i == build_char_cnt) {
build_char_list = galloc(build_char_list,
sizeof(struct charent *)*++build_char_cnt,
"build_char_list");
build_char_list[build_char_cnt-1] = *cp;
}
}
expecthmot = (*cp)->troffcharwidth * fontsize / unitwidth;
} else if (isinstring() || rune != ' ') {
startstring();
if (pageon())
if (rune == ' ')
Bprint(Bstdout, " ");
else
Bprint(Bstdout, "%s", charcode[RUNEGETCHAR(t)].str);
expecthmot = (*cp)->troffcharwidth * fontsize / unitwidth;
}
}
void
runeout(Rune rune) {
char stoken[UTFmax+1];
int i;
i = runetochar(stoken, &rune);
stoken[i] = '\0';
glyphout(rune, stoken, TRUE);
}
void
specialout(char *stoken) {
Rune rune;
chartorune(&rune, stoken);
glyphout(rune, stoken, TRUE);
}
void
graphfunc(Biobufhdr *bp) {
USED(bp);
}
long
nametorune(char *name) {
USED(name);
return(0);
}
void
notavail(char *msg) {
fprint(2, "%s is not available at this time.\n", msg);
}