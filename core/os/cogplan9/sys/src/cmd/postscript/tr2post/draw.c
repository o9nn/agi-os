#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ctype.h>
#include "../common/common.h"
#include "tr2post.h"
BOOLEAN drawflag = FALSE;
BOOLEAN	inpath = FALSE;
void
cover(double x, double y) {
USED(x, y);
}
void
drawspline(Biobufhdr *Bp, int flag) {
int x[100], y[100];
int i, N;
for (N=2; N<sizeof(x)/sizeof(x[0]); N++)
if (Bgetfield(Bp, 'd', &x[N], 0)<=0 || Bgetfield(Bp, 'd', &y[N], 0)<=0)
break;
x[0] = x[1] = hpos;
y[0] = y[1] = vpos;
for (i = 1; i < N; i++) {
x[i+1] += x[i];
y[i+1] += y[i];
}
x[N] = x[N-1];
y[N] = y[N-1];
for (i = ((flag!=1)?0:1); i < ((flag!=1)?N-1:N-2); i++) {
endstring();
if (pageon())
Bprint(Bstdout, "%d %d %d %d %d %d Ds\n", x[i], y[i], x[i+1], y[i+1], x[i+2], y[i+2]);
}
hpos = x[N];
vpos = y[N];
}
void
draw(Biobufhdr *Bp) {
int r, x1, y1, x2, y2, i;
int d1, d2;
drawflag = TRUE;
r = Bgetrune(Bp);
switch(r) {
case 'l':
if (Bgetfield(Bp, 'd', &x1, 0)<=0 || Bgetfield(Bp, 'd', &y1, 0)<=0 || Bgetfield(Bp, 'r', &i, 0)<=0)
error(FATAL, "draw line function, destination coordinates not found.\n");
endstring();
if (pageon())
Bprint(Bstdout, "%d %d %d %d Dl\n", hpos, vpos, hpos+x1, vpos+y1);
hpos += x1;
vpos += y1;
break;
case 'c':
if (Bgetfield(Bp, 'd', &d1, 0)<=0)
error(FATAL, "draw circle function, diameter coordinates not found.\n");
endstring();
if (pageon())
Bprint(Bstdout, "%d %d %d %d De\n", hpos, vpos, d1, d1);
hpos += d1;
break;
case 'e':
if (Bgetfield(Bp, 'd', &d1, 0)<=0 || Bgetfield(Bp, 'd', &d2, 0)<=0)
error(FATAL, "draw ellipse function, diameter coordinates not found.\n");
endstring();
if (pageon())
Bprint(Bstdout, "%d %d %d %d De\n", hpos, vpos, d1, d2);
hpos += d1;
break;
case 'a':
if (Bgetfield(Bp, 'd', &x1, 0)<=0 || Bgetfield(Bp, 'd', &y1, 0)<=0 || Bgetfield(Bp, 'd', &x2, 0)<=0 || Bgetfield(Bp, 'd', &y2, 0)<=0)
error(FATAL, "draw arc function, coordinates not found.\n");
endstring();
if (pageon())
Bprint(Bstdout, "%d %d %d %d %d %d Da\n", hpos, vpos, x1, y1, x2, y2);
hpos += x1 + x2;
vpos += y1 + y2;
break;
case 'q':
drawspline(Bp, 1);
break;
case '~':
drawspline(Bp, 2);
break;
default:
error(FATAL, "unknown draw function <%c>\n", r);
break;
}
}
void
beginpath(char *buf, int copy) {
if (inpath == FALSE) {
endstring();
Bprint(Bstdout, "gsave\n");
Bprint(Bstdout, "newpath\n");
Bprint(Bstdout, "%d %d m\n", hpos, vpos);
Bprint(Bstdout, "/inpath true def\n");
if ( copy == TRUE )
Bprint(Bstdout, "%s\n", buf);
inpath = TRUE;
}
}
static void parsebuf(char*);
void
drawpath(char *buf, int copy) {
if ( inpath == TRUE ) {
if ( copy == TRUE )
Bprint(Bstdout, "%s\n", buf);
else
parsebuf(buf);
Bprint(Bstdout, "grestore\n");
Bprint(Bstdout, "/inpath false def\n");
inpath = FALSE;
}
}
static void
parsebuf(char *buf)
{
char *p;
char *q;
int gsavelevel = 0;
for(p = buf; p != nil; p = q) {
if( q = strchr(p, ' ') )
*q++ = '\0';
if ( gsavelevel == 0 ) {
Bprint(Bstdout, "gsave\n");
gsavelevel++;
}
if ( strcmp(p, "stroke") == 0 ) {
Bprint(Bstdout, "closepath stroke\ngrestore\n");
gsavelevel--;
} else if ( strcmp(p, "openstroke") == 0 ) {
Bprint(Bstdout, "stroke\ngrestore\n");
gsavelevel--;
} else if ( strcmp(p, "fill") == 0 ) {
Bprint(Bstdout, "eofill\ngrestore\n");
gsavelevel--;
} else if ( strcmp(p, "wfill") == 0 ) {
Bprint(Bstdout, "fill\ngrestore\n");
gsavelevel--;
} else if ( strcmp(p, "sfill") == 0 ) {
Bprint(Bstdout, "eofill\ngrestore\ngsave\nstroke\ngrestore\n");
gsavelevel--;
} else if ( strncmp(p, "gray", strlen("gray")) == 0 ) {
if( q ) {
p = q;
if ( q = strchr(p, ' ') )
*q++ = '\0';
Bprint(Bstdout, "%s setgray\n", p);
}
} else if ( strncmp(p, "color", strlen("color")) == 0 ) {
if( q ) {
p = q;
if ( q = strchr(p, ' ') )
*q++ = '\0';
Bprint(Bstdout, "/%s setcolor\n", p);
}
} else if ( strncmp(p, "line", strlen("line")) == 0 ) {
if( q ) {
p = q;
if ( q = strchr(p, ' ') )
*q++ = '\0';
Bprint(Bstdout, "%s resolution mul 2 div setlinewidth\n", p);
}
} else if ( strncmp(p, "reverse", strlen("reverse")) == 0 )
Bprint(Bstdout, "reversepath\n");
else if ( *p == '"' ) {
for ( ; gsavelevel > 0; gsavelevel-- )
Bprint(Bstdout, "grestore\n");
if ( q != nil )
*--q = ' ';
if ( (q = strchr(p, '"')) != nil ) {
*q++ = '\0';
Bprint(Bstdout, "%s\n", p);
}
}
}
for ( ; gsavelevel > 0; gsavelevel-- )
Bprint(Bstdout, "grestore\n");
}