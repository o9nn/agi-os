#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <fcntl.h>
#include "comments.h"
#include "gen.h"
#include "path.h"
#include "ext.h"
#include "posttek.h"
char	*optnames = "a:c:f:m:n:o:p:w:x:y:A:C:E:J:L:P:R:DI";
char	*prologue = POSTTEK;
char	*formfile = FORMFILE;
int	formsperpage = 1;
int	copies = 1;
int	charheight[] = CHARHEIGHT;
int	charwidth[] = CHARWIDTH;
int	tekfont = TEKFONT;
char	intensity[] = INTENSITY;
char	*styles[] = STYLES;
int	linestyle = 0;
int	linetype = 0;
int	dispmode = ALPHA;
int	points = 0;
int	characters = 0;
int	pen = UP;
int	margin = 0;
Point	cursor;
Fontmap	fontmap[] = FONTMAP;
char	*fontname = "Courier";
int	page = 0;
int	printed = 0;
FILE	*fp_in;
FILE	*fp_out = stdout;
FILE	*fp_acct = NULL;
main(agc, agv)
int		agc;
char	*agv[];
{
argv = agv;
argc = agc;
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
if ( signal(SIGINT, interrupt) == SIG_IGN )  {
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
int		ch;
int		old_optind = optind;
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
int		ch;
while ( (ch = getopt(argc, argv, optnames)) != EOF )  {
switch ( ch )  {
case 'a':
fprintf(stdout, "/aspectratio %s def\n", optarg);
break;
case 'c':
copies = atoi(optarg);
fprintf(stdout, "/#copies %s store\n", optarg);
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
char	*name;
{
int		i;
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
if ( formsperpage > 1 )  {
if ( cat(formfile) == FALSE )
error(FATAL, "can't read %s", formfile);
fprintf(stdout, "%d setupforms\n", formsperpage);
}
fprintf(stdout, "%s", ENDSETUP);
}
arguments()
{
if ( argc < 1 )
statemachine(fp_in = stdin);
else  {
while ( argc > 0 )  {
if ( strcmp(*argv, "-") == 0 )
fp_in = stdin;
else if ( (fp_in = fopen(*argv, "r")) == NULL )
error(FATAL, "can't open %s", *argv);
statemachine(fp_in);
if ( fp_in != stdin )
fclose(fp_in);
argc--;
argv++;
}
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
statemachine(fp)
FILE	*fp;
{
redirect(-1);
formfeed();
dispmode = RESET;
while ( 1 )
switch ( dispmode )  {
case RESET:
reset();
break;
case ALPHA:
alpha();
break;
case GIN:
gin();
break;
case GRAPH:
graph();
break;
case POINT:
case SPECIALPOINT:
point();
break;
case INCREMENTAL:
incremental();
break;
case EXIT:
formfeed();
return;
}
}
reset()
{
tekfont = -1;
home();
setfont(TEKFONT);
setmode(ALPHA);
}
alpha()
{
int		c;
int		x, y;
if ( (c = nextchar()) == OUTMODED )
return;
if ( (c < 040) && ((c = control(c)) <= 0) )
return;
x = cursor.x;
y = cursor.y;
switch ( c )  {
case DEL:
return;
case BS:
if ((x -= charwidth[tekfont]) < margin)
x = TEKXMAX - charwidth[tekfont];
break;
case NL:
y -= charheight[tekfont];
break;
case CR:
x = margin;
break;
case VT:
if ((y += charheight[tekfont]) >= TEKYMAX)
y = 0;
break;
case HT:
case ' ':
default:
if ( characters++ == 0 )
fprintf(fp_out, "%d %d (", cursor.x, cursor.y);
switch ( c )  {
case '(':
case ')':
case '\\':
putc('\\', fp_out);
default:
putc(c, fp_out);
}
x += charwidth[tekfont];
move(x, y);
break;
}
if (x >= TEKXMAX) {
x = margin;
y -= charheight[tekfont];
}
if (y < 0) {
y = TEKYMAX - charheight[tekfont];
x -= margin;
margin = (TEKXMAX/2) - margin;
if ((x += margin) > TEKXMAX)
x -= margin;
}
if ( y != cursor.y || x != cursor.x )
text();
move(x, y);
}
graph()
{
int			c;
int			b;
int			x, y;
static int		hix, hiy;
static int		lox, loy;
static int		extra;
if ((c = nextchar()) < 040) {
control(c);
return;
}
if ((c & 0140) == 040) {
hiy = c & 037;
do
if (((c = nextchar()) < 040) && ((c = control(c)) == OUTMODED))
return;
while (c == 0);
}
if ((c & 0140) == 0140) {
b = c & 037;
do
if (((c = nextchar()) < 040) && ((c = control(c)) == OUTMODED))
return;
while (c == 0);
if ((c & 0140) == 0140) {
extra = b;
loy = c & 037;
do
if (((c = nextchar()) < 040) && ((c = control(c)) == OUTMODED))
return;
while (c == 0);
} else loy = b;
}
if ((c & 0140) == 040) {
hix = c & 037;
do
if (((c = nextchar()) < 040) && ((c = control(c)) == OUTMODED))
return;
while (c == 0);
}
lox = c & 037;
if (extra & 020)
margin = TEKXMAX/2;
x = (hix<<7) | (lox<<2) | (extra & 03);
y = (hiy<<7) | (loy<<2) | ((extra & 014)>>2);
if ( points > 100 )  {
draw();
points = 1;
}
if ( points++ )
fprintf(fp_out, "%d %d\n", cursor.x - x, cursor.y - y);
move(x, y);
}
point()
{
int		c;
if ( dispmode == SPECIALPOINT )  {
if ( (c = nextchar()) < 040 || c > 0175 )
return(control(c));
fprintf(fp_out, "%d %d i\n", intensity[c - ' '], c & 0100);
}
graph();
draw();
}
incremental()
{
int		c;
int		x, y;
if ( (c = nextchar()) == OUTMODED )
return;
if ( (c < 040) && ((c = control(c)) <= 0) )
return;
x = cursor.x;
y = cursor.y;
if ( c & 060 )
pen = ( c & 040 ) ? UP : DOWN;
if ( c & 04 ) y++;
if ( c & 010 ) y--;
if ( c & 01 ) x++;
if ( c & 02 ) x--;
move(x, y);
if ( pen == DOWN )  {
points = 1;
draw();
}
}
gin()
{
control(nextchar());
}
control(c)
int		c;
{
switch ( c )  {
case BEL:
return(0);
case BS:
case HT:
case VT:
return(dispmode == ALPHA ? c : 0);
case CR:
if ( dispmode != ALPHA )  {
setmode(ALPHA);
ungetc(c, fp_in);
return(OUTMODED);
} else return(c);
case FS:
if ( (dispmode == ALPHA) || (dispmode == GRAPH) )  {
setmode(POINT);
return(OUTMODED);
}
return(0);
case GS:
if ( (dispmode == ALPHA) || (dispmode == GRAPH) )  {
setmode(GRAPH);
return(OUTMODED);
}
return(0);
case NL:
ungetc(CR, fp_in);
return(dispmode == ALPHA ? c : 0);
case RS:
if ( dispmode != GIN )  {
setmode(INCREMENTAL);
return(OUTMODED);
}
return(0);
case US:
if ( dispmode == ALPHA )
return(0);
setmode(ALPHA);
return(OUTMODED);
case ESC:
return(esc());
case OUTMODED:
return(c);
default:
return(c < 040 ? 0 : c);
}
}
esc()
{
int		c;
int		ignore;
do  {
c = nextchar();
ignore = 0;
switch ( c )  {
case CAN:
return(0);
case CR:
ignore = 1;
break;
case ENQ:
setmode(ALPHA);
return(OUTMODED);
case ETB:
return(0);
case FF:
formfeed();
setmode(ALPHA);
return(OUTMODED);
case FS:
if ( (dispmode == INCREMENTAL) || ( dispmode == GIN) )
return(0);
setmode(SPECIALPOINT);
return(OUTMODED);
case SI:
case SO:
return(0);
case SUB:
setmode(GIN);
return(OUTMODED);
case OUTMODED:
return(OUTMODED);
case '8':
case '9':
case ':':
case ';':
setfont(c - '8');
return(0);
default:
if ( c == '?' && dispmode == GRAPH )
return(DEL);
if ( (c<'`') || (c>'w') )
break;
c -= '`';
if ( (c & 010) != linetype )
fprintf(fp_out, "%d w\n", (linetype = (c & 010))/010);
if ( ((c + 1) & 7) >= 6 )
break;
if ( (c + 1) & 7 )
if ( (c & 7) != linestyle )  {
linestyle = c & 7;
setmode(dispmode);
fprintf(fp_out, "%s l\n", styles[linestyle]);
}
return(0);
}
} while (ignore);
return(0);
}
move(x, y)
int		x, y;
{
cursor.x = x;
cursor.y = y;
}
setmode(mode)
int		mode;
{
switch ( dispmode )  {
case ALPHA:
text();
break;
case GRAPH:
draw();
break;
case INCREMENTAL:
pen = UP;
break;
}
dispmode = mode;
}
home()
{
margin = 0;
move(0, TEKYMAX);
}
setfont(newfont)
int		newfont;
{
if ( newfont != tekfont )  {
setmode(dispmode);
fprintf(fp_out, "%d f\n", charwidth[newfont]);
}
tekfont = newfont;
}
text()
{
if ( dispmode == ALPHA && characters > 0 )
fprintf(fp_out, ") t\n");
characters = 0;
}
draw()
{
if ( points > 1 )
fprintf(fp_out, "%d %d v\n", cursor.x, cursor.y);
else if ( points == 1 && dispmode != GRAPH )
fprintf(fp_out, "%d %d p\n", cursor.x, cursor.y);
points = 0;
}
formfeed()
{
setmode(dispmode);
if ( fp_out == stdout )
printed++;
fprintf(fp_out, "cleartomark\n");
fprintf(fp_out, "showpage\n");
fprintf(fp_out, "saveobj restore\n");
fprintf(fp_out, "%s %d %d\n", ENDPAGE, page, printed);
if ( ungetc(getc(fp_in), fp_in) == EOF )
redirect(-1);
else redirect(++page);
fprintf(fp_out, "%s %d %d\n", PAGE, page, printed+1);
fprintf(fp_out, "/saveobj save def\n");
fprintf(fp_out, "mark\n");
writerequest(printed+1, fp_out);
fprintf(fp_out, "%d pagesetup\n", printed+1);
fprintf(fp_out, "%d f\n", charwidth[tekfont]);
fprintf(fp_out, "%s l\n", styles[linestyle]);
home();
}
nextchar()
{
int		ch;
if ( (ch = getc(fp_in)) == EOF )  {
setmode(EXIT);
ch = OUTMODED;
}
return(ch);
}
redirect(pg)
int		pg;
{
static FILE	*fp_null = NULL;
if ( pg >= 0 && in_olist(pg) == ON )
fp_out = stdout;
else if ( (fp_out = fp_null) == NULL )
fp_out = fp_null = fopen("/dev/null", "w");
}