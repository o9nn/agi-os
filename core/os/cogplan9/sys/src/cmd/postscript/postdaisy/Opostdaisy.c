#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include <fcntl.h>
#include "comments.h"
#include "gen.h"
#include "path.h"
#include "ext.h"
#include "postdaisy.h"
char *optnames = "a:c:f:h:l:m:n:o:p:r:s:v:x:y:A:C:E:J:L:P:DI";
char *prologue = POSTDAISY;
char *formfile = FORMFILE;
int formsperpage = 1;
int copies = 1;
char htabstops[COLUMNS];
char vtabstops[ROWS];
int res = RES;
int hmi = HMI;
int vmi = VMI;
int ohmi = HMI;
int ovmi = VMI;
int hpos = 0;
int vpos = 0;
int lastx = -1;
int lasty = -1;
int lasthmi = -1;
int lastc = -1;
int prevx = -1;
int leftmargin = LEFTMARGIN;
int rightmargin = RIGHTMARGIN;
int topmargin = TOPMARGIN;
int bottommargin = BOTTOMMARGIN;
int stringcount = 0;
int stringstart = 1;
int advance = 1;
int lfiscr = OFF;
int crislf = OFF;
int linespp = 0;
int markedpage = FALSE;
int page = 0;
int printed = 0;
Fontmap fontmap[] = FONTMAP;
char *fontname = "Courier";
int shadowprint = OFF;
FILE *fp_in;
FILE *fp_out = stdout;
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
int interrupt();
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
if ( DOROUND )
cat(ROUNDPAGE);
fprintf(stdout, "%s", ENDPROLOG);
fprintf(stdout, "%s", BEGINSETUP);
fprintf(stdout, "mark\n");
}
options()
{
int ch;
int n;
while ( (ch = getopt(argc, argv, optnames)) != EOF ) {
switch ( ch ) {
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
case 'h':
ohmi = hmi = atoi(optarg) * HSCALE;
fprintf(stdout, "/hmi %s def\n", optarg);
break;
case 'l':
linespp = atoi(optarg);
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
case 'r':
n = atoi(optarg);
if ( n & 01 )
lfiscr = ON;
else lfiscr = OFF;
if ( n & 02 )
crislf = ON;
else crislf = OFF;
break;
case 's':
fprintf(stdout, "/pointsize %s def\n", optarg);
break;
case 'v':
ovmi = vmi = atoi(optarg) * VSCALE;
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
error(FATAL, "missing case for option %c\n", ch);
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
fp_in = stdin;
if ( argc < 1 )
text();
else {
while ( argc > 0 ) {
if ( strcmp(*argv, "-") == 0 )
fp_in = stdin;
else if ( (fp_in = fopen(*argv, "r")) == NULL )
error(FATAL, "can't open %s", *argv);
text();
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
text()
{
int ch;
redirect(-1);
formfeed();
inittabs();
while ( (ch = getc(fp_in)) != EOF )
switch ( ch ) {
case '\010':
backspace();
break;
case '\011':
htab();
break;
case '\012':
linefeed();
break;
case '\013':
vtab();
break;
case '\014':
formfeed();
break;
case '\015':
carriage();
break;
case '\016':
break;
case '\017':
break;
case '\031':
break;
case '\033':
escape();
break;
default:
if ( isascii(ch) && isprint(ch) )
oput(ch);
break;
}
formfeed();
}
inittabs()
{
int i;
for ( i = 0; i < COLUMNS; i++ )
htabstops[i] = ((i % 8) == 0) ? ON : OFF;
for ( i = 0; i < ROWS; i++ )
vtabstops[i] = ((i * ovmi) > BOTTOMMARGIN) ? ON : OFF;
}
cleartabs()
{
int i;
for ( i = 0; i < ROWS; i++ )
htabstops[i] = OFF;
for ( i = 0; i < COLUMNS; i++ )
vtabstops[i] = OFF;
}
formfeed()
{
if ( fp_out == stdout )
printed++;
endline();
fprintf(fp_out, "cleartomark\n");
if ( feof(fp_in) == 0 || markedpage == TRUE )
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
vgoto(topmargin);
hgoto(leftmargin);
markedpage = FALSE;
}
linefeed()
{
int line = 0;
vmot(vmi);
if ( lfiscr == ON )
hgoto(leftmargin);
if ( linespp > 0 )
line = vpos / ovmi + 1;
if ( vpos > bottommargin || line > linespp )
formfeed();
}
carriage()
{
if ( shadowprint == ON )
changefont(fontname);
advance = 1;
shadowprint = OFF;
hgoto(leftmargin);
if ( crislf == ON )
linefeed();
}
htab()
{
int col;
int i;
endline();
col = hpos/ohmi + 1;
for ( i = col; i < ROWS; i++ )
if ( htabstops[i] == ON ) {
col = i;
break;
}
hgoto(col * ohmi);
lastx = hpos;
}
vtab()
{
int line;
int i;
endline();
line = vpos/ovmi + 1;
for ( i = line; i < COLUMNS; i++ )
if ( vtabstops[i] == ON ) {
line = i;
break;
}
vgoto(line * ovmi);
}
backspace()
{
endline();
if ( hpos - leftmargin >= hmi )
hmot(-hmi);
else hgoto(leftmargin);
lastx = hpos;
}
escape()
{
int ch;
switch ( ch = getc(fp_in) ) {
case 'T':
topmargin = vpos;
break;
case 'L':
bottommargin = vpos;
break;
case 'C':
bottommargin = BOTTOMMARGIN;
topmargin = TOPMARGIN;
break;
case '9':
leftmargin = hpos;
break;
case '0':
rightmargin = hpos;
break;
case '1':
htabstops[hpos/ohmi] = ON;
break;
case '8':
htabstops[hpos/ohmi] = OFF;
break;
case '-':
vtabstops[vpos/ovmi] = ON;
break;
case '2':
cleartabs();
break;
case '\014':
linespp = getc(fp_in);
break;
case '\037':
hmi = HSCALE * (getc(fp_in) - 1);
break;
case 'S':
hmi = ohmi;
break;
case '\011':
hgoto((getc(fp_in)-1) * ohmi);
break;
case '?':
lfiscr = ON;
break;
case '!':
lfiscr = OFF;
break;
case '5':
advance = 1;
break;
case '6':
advance = -1;
break;
case '\036':
vmi = VSCALE * (getc(fp_in) - 1);
break;
case '\013':
vgoto((getc(fp_in)-1) * ovmi);
break;
case 'U':
vmot(vmi/2);
break;
case 'D':
vmot(-vmi/2);
break;
case '\012':
vmot(-vmi);
break;
case '\015':
bottommargin = BOTTOMMARGIN;
topmargin = TOPMARGIN;
leftmargin = BOTTOMMARGIN;
rightmargin = RIGHTMARGIN;
break;
case 'E':
changefont("/Courier-Oblique");
break;
case 'R':
changefont(fontname);
break;
case 'O':
case 'W':
changefont("/Courier-Bold");
shadowprint = ON;
break;
case '&':
changefont(fontname);
shadowprint = OFF;
break;
case '/':
case '\\':
case '<':
case '>':
case '%':
case '=':
case '.':
case '4':
case 'A':
case 'B':
case 'M':
case 'N':
case 'P':
case 'Q':
case 'X':
case '\010':
break;
case ',':
case '\016':
case '\021':
getc(fp_in);
break;
case '3':
case '7':
case 'G':
case 'V':
case 'Y':
case 'Z':
error(FATAL, "graphics mode is not implemented");
break;
default:
error(FATAL, "missing case for escape o%o\n", ch);
break;
}
}
vmot(n)
int n;
{
vpos += n;
}
vgoto(n)
int n;
{
vpos = n;
}
hmot(n)
int n;
{
hpos += n * advance;
if ( hpos < leftmargin )
hpos = leftmargin;
}
hgoto(n)
int n;
{
hpos = n;
}
changefont(name)
char *name;
{
endline();
fprintf(fp_out, "%s f\n", name);
}
startline()
{
if ( stringcount < 1 ) {
putc('(', fp_out);
stringstart = lastx = hpos;
lasty = vpos;
lasthmi = hmi;
lastc = -1;
prevx = -1;
stringcount = 1;
}
}
endline()
{
if ( stringcount > 0 )
fprintf(fp_out, ")%d %d %d t\n", stringstart, lasty, lasthmi);
stringcount = 0;
}
endstring()
{
if ( stringcount > 0 ) {
fprintf(fp_out, ")%d(", stringstart);
lastx = stringstart = hpos;
stringcount++;
}
}
oput(ch)
int ch;
{
if ( stringcount > 100 )
endline();
if ( vpos != lasty )
endline();
if ( advance == -1 )
hmot(hmi);
startline();
if ( lastc != ch || hpos != prevx ) {
if ( lastx != hpos )
endstring();
if ( ch == '\\' || ch == '(' || ch == ')' )
putc('\\', fp_out);
putc(ch, fp_out);
lastc = ch;
prevx = hpos;
lastx += lasthmi;
}
if ( advance != -1 )
hmot(hmi);
markedpage = TRUE;
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