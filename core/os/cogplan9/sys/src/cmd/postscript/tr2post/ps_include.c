#include <u.h>
#include <libc.h>
#include <bio.h>
#include <stdio.h>
#include "../common/common.h"
#include "ps_include.h"
extern int curpostfontid;
extern int curfontsize;
typedef struct {long start, end;} Section;
static char *buf;
static void
copy(Biobufhdr *fin, Biobufhdr *fout, Section *s) {
if (s->end <= s->start)
return;
Bseek(fin, s->start, 0);
while (Bseek(fin, 0L, 1) < s->end && (buf=Brdline(fin, '\n')) != NULL){
if(buf[0] == '%')
Bputc(fout, ' ');
Bwrite(fout, buf, Blinelen(fin));
}
}
void
ps_include(Biobufhdr *fin, Biobufhdr *fout, int page_no, int whiteout,
int outline, int scaleboth, double cx, double cy, double sx, double sy,
double ax, double ay, double rot) {
int foundpage = 0;
int foundpbox = 0;
int i;
int maxglobal = 0;
int nglobal = 0;
char **strp;
double llx, lly;
double o = outline != 0;
double s = scaleboth != 0;
double urx, ury;
double w = whiteout != 0;
Section *global;
Section prolog, page, trailer;
#define has(word)	(strncmp(buf, word, strlen(word)) == 0)
#define grab(n)		((Section *)(nglobal \
? realloc((char *)global, n*sizeof(Section)) \
: calloc(n, sizeof(Section))))
global = nil;
llx = lly = 0;
urx = 72 * 8.5;
ury = 72 * 11.0;
prolog.start = prolog.end = 0;
page.start = page.end = 0;
trailer.start = 0;
Bseek(fin, 0L, 0);
while ((buf=Brdline(fin, '\n')) != NULL) {
buf[Blinelen(fin)-1] = '\0';
if (!has("%%"))
continue;
else if (has("%%Page: ")) {
if (!foundpage)
page.start = Bseek(fin, 0L, 1);
sscanf(buf, "%*s %*s %d", &i);
if (i == page_no)
foundpage = 1;
else if (foundpage && page.end <= page.start)
page.end = Bseek(fin, 0L, 1);
} else if (has("%%EndPage: ")) {
sscanf(buf, "%*s %*s %d", &i);
if (i == page_no) {
foundpage = 1;
page.end = Bseek(fin, 0L, 1);
}
if (!foundpage)
page.start = Bseek(fin, 0L, 1);
} else if (has("%%PageBoundingBox: ")) {
if (i == page_no) {
foundpbox = 1;
sscanf(buf, "%*s %lf %lf %lf %lf",
&llx, &lly, &urx, &ury);
}
} else if (has("%%BoundingBox: ")) {
if (!foundpbox)
sscanf(buf,"%*s %lf %lf %lf %lf",
&llx, &lly, &urx, &ury);
} else if (has("%%EndProlog") || has("%%EndSetup") || has("%%EndDocumentSetup"))
prolog.end = page.start = Bseek(fin, 0L, 1);
else if (has("%%Trailer"))
trailer.start = Bseek(fin, 0L, 1);
else if (has("%%BeginGlobal")) {
if (page.end <= page.start) {
if (nglobal >= maxglobal) {
maxglobal += 20;
global = grab(maxglobal);
}
global[nglobal].start = Bseek(fin, 0L, 1);
}
} else if (has("%%EndGlobal"))
if (page.end <= page.start)
global[nglobal++].end = Bseek(fin, 0L, 1);
}
Bseek(fin, 0L, 2);
if (trailer.start == 0)
trailer.start = Bseek(fin, 0L, 1);
trailer.end = Bseek(fin, 0L, 1);
if (page.end <= page.start)
page.end = trailer.start;
for (strp = PS_head; *strp != NULL; strp++)
Bwrite(fout, *strp, strlen(*strp));
Bprint(fout, "/llx %g def\n", llx);
Bprint(fout, "/lly %g def\n", lly);
Bprint(fout, "/urx %g def\n", urx);
Bprint(fout, "/ury %g def\n", ury);
Bprint(fout, "/w %g def\n", w);
Bprint(fout, "/o %g def\n", o);
Bprint(fout, "/s %g def\n", s);
Bprint(fout, "/cx %g def\n", cx);
Bprint(fout, "/cy %g def\n", cy);
Bprint(fout, "/sx %g def\n", sx);
Bprint(fout, "/sy %g def\n", sy);
Bprint(fout, "/ax %g def\n", ax);
Bprint(fout, "/ay %g def\n", ay);
Bprint(fout, "/rot %g def\n", rot);
for (strp = PS_setup; *strp != NULL; strp++)
Bwrite(fout, *strp, strlen(*strp));
copy(fin, fout, &prolog);
for(i = 0; i < nglobal; i++)
copy(fin, fout, &global[i]);
copy(fin, fout, &page);
copy(fin, fout, &trailer);
for (strp = PS_tail; *strp != NULL; strp++)
Bwrite(fout, *strp, strlen(*strp));
if(nglobal)
free(global);
curpostfontid = -1;
curfontsize = -1;
}