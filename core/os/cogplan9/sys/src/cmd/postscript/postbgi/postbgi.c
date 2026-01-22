#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <signal.h>
#include <math.h>
#include <ctype.h>
#ifdef plan9
#define isascii(c) ((unsigned char)(c)<=0177)
#endif
#include "comments.h"
#include "gen.h"
#include "path.h"
#include "ext.h"
#include "postbgi.h"
char *optnames = "a:c:f:m:n:o:p:w:x:y:A:C:E:J:L:P:R:DI";
char *prologue = POSTBGI;
char *formfile = FORMFILE;
int formsperpage = 1;
int copies = 1;
char *styles[] = STYLES;
int hpos = 0;
int vpos = 0;
int bgisize = BGISIZE;
int linespace;
int bgimode;
int in_subr = FALSE;
int in_global = FALSE;
int subr_id = 0;
int shpos = 0;
int svpos = 0;
Disp displacement[64];
Fontmap fontmap[] = FONTMAP;
char *fontname = "Courier";
int page = 0;
int printed = 0;
FILE *fp_in = stdin;
FILE *fp_out = NULL;
FILE *fp_acct = NULL;
main(agc, agv)
int agc;
char *agv[];
{
argc = agc;
argv = agv;
prog_name = argv[0];
init_signals();
header();
options();
setup();
arguments();
done();
account();
exit(x_stat);
}
init_signals()
{
if ( signal(SIGINT, interrupt) == SIG_IGN ) {
signal(SIGINT, SIG_IGN);
signal(SIGQUIT, SIG_IGN);
signal(SIGHUP, SIG_IGN);
} else {
signal(SIGHUP, interrupt);
signal(SIGQUIT, interrupt);
}
signal(SIGTERM, interrupt);
}
header()
{
int ch;
int old_optind = optind;
while ( (ch = getopt(argc, argv, optnames)) != EOF )
if ( ch == 'L' )
prologue = optarg;
else if ( ch == '?' )
error(FATAL, "");
optind = old_optind;
fprintf(stdout, "%s", CONFORMING);
fprintf(stdout, "%s %s\n", VERSION, PROGRAMVERSION);
fprintf(stdout, "%s %s\n", DOCUMENTFONTS, ATEND);
fprintf(stdout, "%s %s\n", PAGES, ATEND);
fprintf(stdout, "%s", ENDCOMMENTS);
if ( cat(prologue) == FALSE )
error(FATAL, "can't read %s", prologue);
fprintf(stdout, "%s", ENDPROLOG);
fprintf(stdout, "%s", BEGINSETUP);
fprintf(stdout, "mark\n");
}
options()
{
int ch;
while ( (ch = getopt(argc, argv, optnames)) != EOF ) {
switch ( ch ) {
case 'a':
fprintf(stdout, "/aspectratio %s def\n", optarg);
break;
case 'c':
copies = atoi(optarg);
fprintf(stdout, "/#copies %s def\n", optarg);
break;
case 'f':
fontname = get_font(optarg);
fprintf(stdout, "/font /%s def\n", fontname);
break;
case 'm':
fprintf(stdout, "/magnification %s def\n", optarg);
break;
case 'n':
formsperpage = atoi(optarg);
fprintf(stdout, "%s %s\n", FORMSPERPAGE, optarg);
fprintf(stdout, "/formsperpage %s def\n", optarg);
break;
case 'o':
out_list(optarg);
break;
case 'p':
if ( *optarg == 'l' )
fprintf(stdout, "/landscape true def\n");
else fprintf(stdout, "/landscape false def\n");
break;
case 'w':
fprintf(stdout, "/linewidth %s def\n", optarg);
break;
case 'x':
fprintf(stdout, "/xoffset %s def\n", optarg);
break;
case 'y':
fprintf(stdout, "/yoffset %s def\n", optarg);
break;
case 'A':
case 'J':
if ( (fp_acct = fopen(optarg, "a")) == NULL )
error(FATAL, "can't open accounting file %s", optarg);
break;
case 'C':
if ( cat(optarg) == FALSE )
error(FATAL, "can't read %s", optarg);
break;
case 'E':
fontencoding = optarg;
break;
case 'L':
prologue = optarg;
break;
case 'P':
fprintf(stdout, "%s\n", optarg);
break;
case 'R':
saverequest(optarg);
break;
case 'D':
debug = ON;
break;
case 'I':
ignore = ON;
break;
case '?':
error(FATAL, "");
break;
default:
error(FATAL, "missing case for option %c", ch);
break;
}
}
argc -= optind;
argv += optind;
}
char *get_font(name)
char *name;
{
int i;
for ( i = 0; fontmap[i].name != NULL; i++ )
if ( strcmp(name, fontmap[i].name) == 0 )
return(fontmap[i].val);
return(name);
}
setup()
{
writerequest(0, stdout);
setencoding(fontencoding);
fprintf(stdout, "setup\n");
if ( formsperpage > 1 ) {
if ( cat(formfile) == FALSE )
error(FATAL, "can't read %s", formfile);
fprintf(stdout, "%d setupforms\n", formsperpage);
}
fprintf(stdout, "%s", ENDSETUP);
}
arguments()
{
if ( argc < 1 )
conv();
else
while ( argc > 0 ) {
if ( strcmp(*argv, "-") == 0 )
fp_in = stdin;
else if ( (fp_in = fopen(*argv, "r")) == NULL )
error(FATAL, "can't open %s", *argv);
conv();
if ( fp_in != stdin )
fclose(fp_in);
argc--;
argv++;
}
}
done()
{
fprintf(stdout, "%s", TRAILER);
fprintf(stdout, "done\n");
fprintf(stdout, "%s %s\n", DOCUMENTFONTS, fontname);
fprintf(stdout, "%s %d\n", PAGES, printed);
}
account()
{
if ( fp_acct != NULL )
fprintf(fp_acct, " print %d\n copies %d\n", printed, copies);
}
conv()
{
int ch;
redirect(-1);
bgimode = 0;
formfeed();
while ( (ch = get_char()) != EOF ) {
switch ( ch ) {
case BRCHAR:
bgimode = ch;
text(90);
break;
case BCHAR:
bgimode = ch;
text(0);
break;
case BGRAPH:
bgimode = ch;
break;
case BSUB:
subr_def();
break;
case BRET:
subr_end();
break;
case BCALL:
subr_call();
break;
case BEND:
formfeed();
break;
case BERASE:
error(FATAL, "BGI erase opcode obsolete");
break;
case BREP:
error(FATAL, "Repeat not implemented");
repeat();
break;
case BSETX:
hgoto(get_int(0));
break;
case BSETY:
vgoto(get_int(0));
break;
case BSETXY:
hgoto(get_int(0));
vgoto(get_int(0));
break;
case BINTEN:
fprintf(fp_out, "%d %d p\n", hpos, vpos);
break;
case BVISX:
vector(X_COORD, VISIBLE);
break;
case BINVISX:
vector(X_COORD, INVISIBLE);
break;
case BVISY:
vector(Y_COORD, VISIBLE);
break;
case BINVISY:
vector(Y_COORD, INVISIBLE);
break;
case BVEC:
vector(LONGVECTOR, VISIBLE);
break;
case BSVEC:
vector(SHORTVECTOR, VISIBLE);
break;
case BRECT:
rectangle(OUTLINE);
break;
case BPOINT1:
case BPOINT:
point_plot(ch, get_char());
break;
case BLINE:
line_plot();
break;
case BLTY:
fprintf(fp_out, "%s l\n", styles[get_data()]);
break;
case BARC:
arc(OUTLINE);
break;
case BFARC:
arc(FILL);
break;
case BFRECT:
rectangle(FILL);
break;
case BRASRECT:
error(FATAL, "Raster Rectangle not implemented");
break;
case BCOL:
set_color(get_data());
break;
case BFTRAPH:
trapezoid();
break;
case BPAT:
pattern();
break;
case BCSZ:
setsize(get_data());
break;
case BNOISE:
break;
default:
error(FATAL, "bad BGI command %d (0%o)", ch, ch);
break;
}
if ( debug == ON )
fprintf(stderr, "\n");
}
formfeed();
}
hgoto(n)
int n;
{
hpos = n;
}
vgoto(n)
int n;
{
vpos = n;
}
setsize(n)
int n;
{
bgisize = n;
linespace = LINESPACE(bgisize);
fprintf(fp_out, "%d f\n", bgisize);
if ( debug == ON )
fprintf(stderr, "BGI size = %d\n", n);
}
repeat()
{
int count;
int ch;
count = get_int();
while ( (ch = get_char()) != EOF && ch != BENDR ) ;
}
text(angle)
int angle;
{
int ch;
fprintf(fp_out, "%d %d %d(", angle, hpos, vpos);
while ( (ch = get_char()) != EOF ) {
if ( ch == BGRAPH || ch == BCHAR || ch == BRCHAR ) {
ungetc(ch, fp_in);
position--;
break;
}
switch ( ch ) {
case '\012':
vgoto(vpos - linespace);
case '\015':
hgoto(0);
fprintf(fp_out, ")t\n%d %d %d(", angle, hpos, vpos);
break;
case '(':
case ')':
case '\\':
putc('\\', fp_out);
default:
if ( isascii(ch) && isprint(ch) )
putc(ch, fp_out);
else fprintf(fp_out, "\\%.3o", ch & 0377);
break;
}
}
fprintf(fp_out, ") t\n");
}
formfeed()
{
int ch;
if ( bgimode == BGRAPH && (ch = get_char()) != EOF && ! (ch & MSB) ) {
ungetc(ch, fp_in);
position--;
}
if ( fp_out == stdout )
printed++;
fprintf(fp_out, "cleartomark\n");
fprintf(fp_out, "showpage\n");
fprintf(fp_out, "saveobj restore\n");
fprintf(fp_out, "%s %d %d\n", ENDPAGE, page, printed);
while ( (ch = get_char()) == 0 ) ;
ungetc(ch, fp_in);
position--;
if ( ungetc(getc(fp_in), fp_in) == EOF )
redirect(-1);
else redirect(++page);
fprintf(fp_out, "%s %d %d\n", PAGE, page, printed+1);
fprintf(fp_out, "/saveobj save def\n");
fprintf(fp_out, "mark\n");
writerequest(printed+1, fp_out);
fprintf(fp_out, "%d pagesetup\n", printed+1);
setsize(bgisize);
hpos = vpos = 0;
}
subr_def()
{
if ( in_subr == TRUE )
error(FATAL, "can't handle nested subroutine definitions");
if ( (subr_id = get_data()) == EOF )
error(FATAL, "missing subroutine identifier");
if ( in_global == FALSE ) {
fprintf(fp_out, "cleartomark\n");
fprintf(fp_out, "saveobj restore\n");
fprintf(fp_out, "%s", BEGINGLOBAL);
in_global = TRUE;
}
fprintf(fp_out, "/S%d {\n", subr_id);
fprintf(fp_out, "gsave translate\n");
shpos = hpos;
svpos = vpos;
hgoto(0);
vgoto(0);
in_subr = TRUE;
}
subr_end()
{
int ch;
if ( in_subr == FALSE )
error(FATAL, "subroutine end without corresponding start");
fprintf(fp_out, "grestore\n");
fprintf(fp_out, "} def\n");
if ( in_global == TRUE && (ch = get_char()) != BSUB ) {
fprintf(fp_out, "%s", ENDGLOBAL);
fprintf(fp_out, "/saveobj save def\n");
fprintf(fp_out, "mark\n");
in_global = FALSE;
}
ungetc(ch, fp_in);
displacement[subr_id].dx = hpos;
displacement[subr_id].dy = vpos;
hgoto(shpos);
vgoto(svpos);
in_subr = FALSE;
}
subr_call()
{
int ch;
int id;
while ( (ch = get_char()) != EOF && (ch & MSB) ) {
id = ch & DMASK;
fprintf(fp_out, "%d %d S%d\n", hpos, vpos, id);
hgoto(hpos + displacement[id].dx);
vgoto(vpos + displacement[id].dy);
}
ungetc(ch, fp_in);
}
vector(var, mode)
int var;
int mode;
{
int ch;
int x, y;
int count = 0;
x = hpos;
y = vpos;
while ( (ch = get_char()) != EOF && ch & MSB ) {
if ( var == X_COORD )
x += get_int(ch);
else if ( var == Y_COORD )
y += get_int(ch);
else if ( var == LONGVECTOR ) {
x += get_int(ch);
y += get_int(0);
} else {
x += ((ch & MSBMAG) * ((ch & SGNB) ? -1 : 1));
y += (((ch = get_data()) & MSBMAG) * ((ch & SGNB) ? -1 : 1));
}
if ( mode == VISIBLE ) {
fprintf(fp_out, "%d %d\n", hpos - x, vpos - y);
count++;
}
hgoto(x);
vgoto(y);
if ( var == X_COORD )
var = Y_COORD;
else if ( var == Y_COORD )
var = X_COORD;
}
if ( count > 0 )
fprintf(fp_out, "%d %d v\n", hpos, vpos);
ungetc(ch, fp_in);
position--;
}
rectangle(mode)
int mode;
{
int deltax;
int deltay;
deltax = get_int(0);
deltay = get_int(0);
if ( mode == OUTLINE )
fprintf(fp_out, "0 %d %d %d %d R\n", deltax, deltay, hpos, vpos);
else fprintf(fp_out, "1 %d %d %d %d R\n", deltax, deltay, hpos, vpos);
}
trapezoid()
{
int kind;
int d[6];
kind = get_data();
d[0] = get_int(0);
d[1] = 0;
d[2] = get_int(0);
d[3] = get_int(0);
d[4] = get_int(0);
d[5] = 0;
if ( kind == 2 ) {
d[1] = d[0];
d[0] = 0;
d[5] = d[4];
d[4] = 0;
}
fprintf(fp_out, "%d %d %d %d %d %d %d %d T\n", d[4], d[5], d[2], d[3], d[0], d[1], hpos, vpos);
}
point_plot(mode, ch)
int mode;
int ch;
{
int c;
int x, y;
int deltax;
if ( mode == BPOINT1 ) {
deltax = get_int(0);
x = hpos - deltax;
}
while ( (c = get_char()) != EOF && (c & MSB) ) {
if ( mode == BPOINT1 ) {
y = get_int(c);
x += deltax;
} else {
x = get_int(c);
y = get_int(0);
}
hgoto(x);
vgoto(y);
fprintf(fp_out, "%d %d\n", hpos, vpos);
}
putc('(', fp_out);
switch ( ch ) {
case '(':
case ')':
case '\\':
putc('\\', fp_out);
default:
putc(ch, fp_out);
}
fprintf(fp_out, ")pp\n");
ungetc(c, fp_in);
position--;
}
line_plot()
{
int c;
int deltax;
int x0, y0;
int x1, y1;
int count = 0;
deltax = get_int(0);
x1 = hpos;
y1 = get_int(0);
while ( (c = get_char()) != EOF && (c & MSB) ) {
x0 = x1;
y0 = y1;
x1 += deltax;
y1 = get_int(c);
fprintf(fp_out, "%d %d\n", -deltax, y0 - y1);
count++;
}
hgoto(x1);
vgoto(y1);
if ( count > 0 )
fprintf(fp_out, "%d %d v\n", hpos, vpos);
ungetc(c, fp_in);
position--;
}
arc(mode)
int mode;
{
int dx1, dy1;
int dx2, dy2;
int radius;
int angle1, angle2;
dx1 = get_int(0);
dy1 = get_int(0);
dx2 = get_int(0);
dy2 = get_int(0);
radius = get_int(0);
if ( radius == 0 )
return;
angle1 = (atan2((double) dy1, (double) dx1) * 360) / (2 * PI) + .5;
angle2 = (atan2((double) dy2, (double) dx2) * 360) / (2 * PI) + .5;
fprintf(fp_out, "%d %d %d %d %d arcn stroke\n", hpos, vpos, radius, angle1, angle2);
}
pattern()
{
double red = 0;
double green = 0;
double blue = 0;
int kind;
int val;
int i;
if ( (kind = get_char()) == EOF )
error(FATAL, "bad pattern command");
for ( i = 0; i < 4; i++ ) {
val = get_data();
red += get_color(val, RED);
green += get_color(val, GREEN);
blue += get_color(val, BLUE);
}
fprintf(fp_out, "%g %g %g c\n", red/4, green/4, blue/4);
}
get_color(val, component)
int val;
int component;
{
int primary;
int plane;
unsigned rgbcolor;
primary = (val >> 3) & 07;
plane = val & 07;
rgbcolor = (~(primary ^ plane)) & 07;
if ( debug == ON )
fprintf(stderr, "val = %o, primary = %o, plane = %o, rgbcolor = %o\n",
val, primary, plane, rgbcolor);
switch ( component ) {
case RED:
return(rgbcolor>>2);
case GREEN:
return(rgbcolor&01);
case BLUE:
return((rgbcolor>>1)&01);
default:
error(FATAL, "unknown color component");
return(0);
}
}
set_color(val)
int val;
{
fprintf(fp_out, "%d %d %d c\n", get_color(val, RED), get_color(val, GREEN), get_color(val, BLUE));
}
get_int(highbyte)
int highbyte;
{
int lowbyte;
if ( highbyte == 0 )
highbyte = get_data();
lowbyte = get_data();
return(highbyte & SGNB ? -MAG(highbyte, lowbyte) : MAG(highbyte, lowbyte));
}
get_data()
{
int val;
if ( (val = get_char()) == EOF || ! (val & MSB) )
error(FATAL, "missing data value");
return(val & DMASK);
}
get_char()
{
int ch;
if ( (ch = getc(fp_in)) != EOF ) {
position++;
ch &= CHMASK;
}
if ( debug == ON )
fprintf(stderr, "%o ", ch);
return(ch);
}
redirect(pg)
int pg;
{
static FILE *fp_null = NULL;
if ( pg >= 0 && in_olist(pg) == ON )
fp_out = stdout;
else if ( (fp_out = fp_null) == NULL )
fp_out = fp_null = fopen("/dev/null", "w");
}