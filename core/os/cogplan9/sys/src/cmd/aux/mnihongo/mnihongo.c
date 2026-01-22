#include <u.h>
#include <libc.h>
#include <draw.h>
#include <bio.h>
#define hmot(n)	hpos += n
#define hgoto(n)	hpos = n
#define vmot(n)	vgoto(vpos + n)
#define vgoto(n)	vpos = n
#define	putchar(x)	Bprint(&bout, "%C", x)
int	hpos;
int	vpos;
char	*fontfile	= "/lib/font/bit/pelm/unicode.9x24.font";
char	*pschar(char *, char *hex, int *wid, int *ht);
int	kanji(char *);
void	Bgetstr(Biobuf *bp, char *s);
void	Bgetline(Biobuf *bp, char *s);
void	Bgetint(Biobuf *bp, int *n);
Biobuf bin, bout;
void
main(void)
{
int c, n;
char str[100], *args[10];
int jfont, curfont;
if(initdraw(0, fontfile, 0) < 0){
fprint(2, "mnihongo: can't initialize display: %r\n");
exits("open");
}
Binit(&bin, 0, OREAD);
Binit(&bout, 1, OWRITE);
jfont = -1;
curfont = 1;
while ((c = Bgetc(&bin)) >= 0) {
switch (c) {
case '\n':
case ' ':
case '\0':
putchar(c);
break;
case '0': case '1': case '2': case '3': case '4':
case '5': case '6': case '7': case '8': case '9':
putchar(c);
n = (c-'0')*10;
c = Bgetc(&bin);
putchar(c);
n += c - '0';
hmot(n);
putchar(Bgetc(&bin));
break;
case 'c':
c = Bgetrune(&bin);
if(c==' ')
break;
else if(jfont == curfont){
Bungetrune(&bin);
Bgetstr(&bin, str);
kanji(str);
}else{
putchar('c');
putchar(c);
}
break;
case 'C':
Bgetstr(&bin, str);
Bprint(&bout, "C%s", str);
break;
case 'f':
Bgetstr(&bin, str);
curfont = atoi(str);
if(curfont < 0 || curfont > 20)
curfont = 1;
Bprint(&bout, "%c%s", c, str);
break;
case 'N':
case 's':
case 'p':
Bgetint(&bin, &n);
Bprint(&bout, "%c%d", c, n);
break;
case 'H':
Bgetint(&bin, &n);
Bprint(&bout, "%c%d", c, n);
hgoto(n);
break;
case 'h':
Bgetint(&bin, &n);
Bprint(&bout, "%c%d", c, n);
hmot(n);
break;
case 'V':
Bgetint(&bin, &n);
Bprint(&bout, "%c%d", c, n);
vgoto(n);
break;
case 'v':
Bgetint(&bin, &n);
Bprint(&bout, "%c%d", c, n);
vmot(n);
break;
case 'w':
putchar(c);
break;
case 'x':
Bgetline(&bin, str);
Bprint(&bout, "%c%s", c, str);
if(tokenize(str, args, 10)>2 && args[0][0]=='f' && ('0'<=args[1][0] && args[1][0]<='9')){
if(strncmp(args[2], "Jp", 2) == 0)
jfont = atoi(args[1]);
else if(atoi(args[1]) == jfont)
jfont = -1;
}
break;
case 'D':
case 'n':
case '#':
Bgetline(&bin, str);
Bprint(&bout, "%c%s", c, str);
break;
default:
fprint(2, "mnihongo: unknown input character %o %c\n", c, c);
exits("error");
}
}
}
int kanji(char *s)
{
Rune r;
char hex[500];
int size = 10, ht, wid;
chartorune(&r, s);
pschar(s, hex, &wid, &ht);
Bprint(&bout, "x X PS save %d %d m\n", hpos, vpos);
Bprint(&bout, "x X PS currentpoint translate %d %d scale ptsize dup scale\n", size, size);
Bprint(&bout, "x X PS %d %d true [%d 0 0 -%d 0 %d]\n",
wid, ht, wid, wid, ht-2);
Bprint(&bout, "x X PS {<%s>}\n", hex);
Bprint(&bout, "x X PS imagemask restore\n");
return 1;
}
char *pschar(char *s, char *hex, int *wid, int *ht)
{
Point chpt, spt;
Image *b;
uchar rowdata[100];
char *hp = hex;
int y, i;
chpt = stringsize(font, s);
*wid = ((chpt.x+7) / 8) * 8;
*ht = chpt.y;
b = allocimage(display, Rpt(ZP, chpt), GREY1, 0, DBlack);
spt = string(b, Pt(0,0), display->white, ZP, font, s);
for (y = 0; y < chpt.y; y++) {
memset(rowdata, 0, sizeof rowdata);
unloadimage(b, Rect(0, y, chpt.x, y+1), rowdata, sizeof rowdata);
for (i = 0; i < spt.x; i += 8) {
sprint(hp, "%2.2x", rowdata[i/8]);
hp += 2;
}
}
*hp = 0;
freeimage(b);
return hex;
}
void	Bgetstr(Biobuf *bp, char *s)
{
int c;
while ((c = Bgetc(bp)) >= 0) {
if (c == ' ' || c == '\t' || c == '\n') {
Bungetc(bp);
break;
}
*s++ = c;
}
*s = 0;
}
void	Bgetline(Biobuf *bp, char *s)
{
int c;
while ((c = Bgetc(bp)) >= 0) {
*s++ = c;
if (c == '\n')
break;
}
*s = 0;
}
void	Bgetint(Biobuf *bp, int *n)
{
double d;
Bgetd(bp, &d);
*n = d;
}