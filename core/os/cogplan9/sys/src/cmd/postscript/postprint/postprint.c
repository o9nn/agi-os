#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <ctype.h>
#ifdef plan9
#define	isascii(c)	((unsigned char)(c)<=0177)
#endif
#include <sys/types.h>
#include <fcntl.h>
#include "comments.h"
#include "gen.h"
#include "path.h"
#include "ext.h"
#include "postprint.h"
char	*optnames = "a:c:ef:l:m:n:o:p:r:s:t:x:y:A:C:E:J:L:P:R:DI";
char	*prologue = POSTPRINT;
char	*formfile = FORMFILE;
int	formsperpage = 1;
int	copies = 1;
int	linespp = LINESPP;
int	pointsize = POINTSIZE;
int	tabstops = TABSTOPS;
int	crmode = 0;
int	extended = TRUE;
int	col = 1;
int	line = 1;
int	stringcount = 0;
int	stringstart = 1;
Fontmap	fontmap[] = FONTMAP;
char	*fontname = "Courier";
int	page = 0;
int	printed = 0;
FILE	*fp_in = stdin;
FILE	*fp_out = stdout;
FILE	*fp_acct = NULL;
main(agc, agv)
int		agc;
char	*agv[];
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
if ( DOROUND )
cat(ROUNDPAGE);
fprintf(stdout, "%s", ENDPROLOG);
fprintf(stdout, "%s", BEGINSETUP);
fprintf(stdout, "mark\n");
}
options()
{
int		ch;
while ( (ch = getopt(argc, argv, optnames)) != EOF ) {
switch ( ch ) {
case 'a':
fprintf(stdout, "/aspectratio %s def\n", optarg);
break;
case 'c':
copies = atoi(optarg);
fprintf(stdout, "/#copies %s store\n", optarg);
break;
case 'e':
extended = TRUE;
break;
case 'f':
fontname = get_font(optarg);
fprintf(stdout, "/font /%s def\n", fontname);
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
crmode = atoi(optarg);
break;
case 's':
pointsize = atoi(optarg);
fprintf(stdout, "/pointsize %s def\n", optarg);
break;
case 't':
tabstops = atoi(optarg);
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
if ( formsperpage > 1 ) {
if ( cat(formfile) == FALSE )
error(FATAL, "can't read %s", formfile);
fprintf(stdout, "%d setupforms\n", formsperpage);
}
fprintf(stdout, "%s", ENDSETUP);
if ( linespp <= 0 )
linespp = LINESPP * POINTSIZE / pointsize;
}
arguments()
{
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
int		ch;
redirect(-1);
formfeed();
while ( (ch = getc(fp_in)) != EOF )
switch ( ch ) {
case '\n':
newline();
break;
case '\t':
case '\b':
case ' ':
spaces(ch);
break;
case '\014':
formfeed();
break;
case '\r':
if ( crmode == 1 )
spaces(ch);
else if ( crmode == 2 )
newline();
break;
default:
oput(ch);
break;
}
formfeed();
}
formfeed()
{
if ( fp_out == stdout )
printed++;
endline();
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
line = 1;
}
newline()
{
startline();
endline();
if ( ++line > linespp )
formfeed();
}
spaces(ch)
int		ch;
{
int		endcol;
int		i;
startline();
endcol = col;
do {
if ( ch == ' ' )
endcol++;
else if ( ch == '\t' )
endcol += tabstops - ((endcol - 1) % tabstops);
else if ( ch == '\b' )
endcol--;
else if ( ch == '\r' )
endcol = 1;
else break;
} while ( ch = getc(fp_in) );
ungetc(ch, fp_in);
if ( endcol < 1 )
endcol = 1;
if ( (i = endcol - col) >= 0 && i < 6 )
for ( ; i > 0; i-- )
oput((int)' ');
else {
endstring();
col = stringstart = endcol;
}
}
startline()
{
if ( stringcount < 1 ) {
putc('(', fp_out);
stringstart = col = 1;
stringcount = 1;
}
}
endstring()
{
if ( stringcount > 100 ) {
fprintf(fp_out, ")%d LL\n(", stringstart-1);
stringcount = 2;
} else {
fprintf(fp_out, ")%d(", stringstart-1);
stringcount++;
}
}
endline()
{
if ( stringcount == 1 )
fprintf(fp_out, ")l\n");
else if ( stringcount > 1 )
fprintf(fp_out, ")%d L\n", stringstart-1);
stringcount = 0;
}
oput(ch)
int		ch;
{
if ( isascii(ch) && isprint(ch) ) {
startline();
if ( ch == '(' || ch == ')' || ch == '\\' )
putc('\\', fp_out);
putc(ch, fp_out);
col++;
} else if ( extended == TRUE ) {
startline();
fprintf(fp_out, "\\%.3o", ch & 0377);
col++;
}
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