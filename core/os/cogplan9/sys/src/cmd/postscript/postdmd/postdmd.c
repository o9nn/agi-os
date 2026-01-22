#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#ifdef plan9
#define isascii(c) ((unsigned char)(c)<=0177)
#endif
#include <sys/types.h>
#include <fcntl.h>
#include "comments.h"
#include "gen.h"
#include "path.h"
#include "ext.h"
char *optnames = "a:b:c:fm:n:o:p:ux:y:A:C:E:J:L:P:DI";
char *prologue = POSTDMD;
char *formfile = FORMFILE;
int bbox[2] = {0, 0};
int formsperpage = 1;
int copies = 1;
int bytespp = 6;
int flip = FALSE;
int v8undo = TRUE;
int v8format = FALSE;
int page = 0;
int printed = 0;
int patterns;
int scanlines;
int patcount = 0;
char *raster = NULL;
char *prevrast = NULL;
char *rptr;
char *eptr;
FILE *fp_in = NULL;
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
case 'b':
bytespp = atoi(optarg);
break;
case 'c':
copies = atoi(optarg);
fprintf(stdout, "/#copies %s store\n", optarg);
break;
case 'f':
flip = TRUE;
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
case 'u':
v8undo = FALSE;
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
FILE *fp;
if ( argc < 1 )
bitmap(stdin);
else {
while ( argc > 0 ) {
if ( strcmp(*argv, "-") == 0 )
fp = stdin;
else if ( (fp = fopen(*argv, "r")) == NULL )
error(FATAL, "can't open %s", *argv);
bitmap(fp);
if ( fp != stdin )
fclose(fp);
argc--;
argv++;
}
}
}
done()
{
fprintf(stdout, "%s", TRAILER);
fprintf(stdout, "done\n");
fprintf(stdout, "%s 0 0 %d %d\n", BOUNDINGBOX, (bbox[0]*72+100)/100, (bbox[1]*72+100)/100);
fprintf(stdout, "%s %d\n", PAGES, printed);
}
account()
{
if ( fp_acct != NULL )
fprintf(fp_acct, " print %d\n copies %d\n", printed, copies);
}
bitmap(fp)
FILE *fp;
{
int count;
long total;
fp_in = fp;
while ( dimensions() == TRUE ) {
patcount = 0;
total = scanlines * patterns;
bbox[0] = MAX(bbox[0], patterns*16);
bbox[1] = MAX(bbox[1], scanlines);
redirect(++page);
fprintf(fp_out, "%s %d %d\n", PAGE, page, printed+1);
fprintf(fp_out, "/saveobj save def\n");
writerequest(printed+1, fp_out);
fprintf(fp_out, "%s ", (v8format == TRUE && v8undo == FALSE) ? "true" : "false");
fprintf(fp_out, "%s ", (flip == TRUE) ? "true" : "false");
fprintf(fp_out, "%d %d bitmap\n", patterns * 16, scanlines);
while ( patcount != total && (count = getc(fp)) != EOF ) {
addrast(count);
patcount += (count & 0177);
if ( patcount % patterns == 0 )
putrast();
}
if ( debug == ON )
fprintf(stderr, "patterns = %d, scanlines = %d, patcount = %d\n", patterns, scanlines, patcount);
if ( total != patcount )
error(FATAL, "bitmap format error");
if ( fp_out == stdout ) printed++;
fprintf(fp_out, "showpage\n");
fprintf(fp_out, "saveobj restore\n");
fprintf(fp_out, "%s %d %d\n", ENDPAGE, page, printed);
}
}
dimensions()
{
int ox, oy;
int cx, cy;
int i;
if ( (scanlines = getint()) == 0 ) {
ox = getint();
oy = getint();
cx = getint();
cy = getint();
scanlines = cy - oy;
patterns = (cx - ox + 15) / 16;
v8format = TRUE;
} else patterns = getint();
if ( scanlines <= 0 || patterns <= 0 )
return(FALSE);
if ( raster != NULL ) free(raster);
if ( prevrast != NULL ) free(prevrast);
if ( (rptr = raster = (char *) malloc(patterns * 2)) == NULL )
error(FATAL, "no memory");
if ( (prevrast = (char *) malloc(patterns * 2)) == NULL )
error(FATAL, "no memory");
for ( i = 0; i < patterns * 2; i++ )
*(prevrast+i) = 0377;
eptr = rptr + patterns * 2;
return(TRUE);
}
addrast(count)
int count;
{
int size;
int l, h;
int i, j;
if ( count & 0200 ) {
size = 1;
count &= 0177;
} else {
size = count;
count = 1;
}
for ( i = size; i > 0; i-- ) {
if ( (l = getc(fp_in)) == EOF || (h = getc(fp_in)) == EOF )
return;
for ( j = count; j > 0; j-- ) {
*rptr++ = l;
*rptr++ = h;
}
}
}
putrast()
{
char *p1, *p2;
int n;
int i;
n = (bytespp <= 0) ? 2 * patterns : bytespp;
if ( v8format == TRUE && v8undo == TRUE )
for ( i = 0; i < patterns * 2; i++ )
*(raster+i) = (*(prevrast+i) ^= *(raster+i));
for ( p1 = raster, p2 = raster + n; p1 < eptr; p1 = p2 )
if ( patncmp(p1, n) == TRUE ) {
while ( patncmp(p2, n) == TRUE ) p2 += n;
p2 += n;
fprintf(fp_out, "%d ", n);
for ( i = 0; i < n; i++, p1++ )
fprintf(fp_out, "%.2X", ((int) *p1) & 0377);
fprintf(fp_out, " %d\n", (p2 - p1) / n);
} else {
while ( p2 < eptr && patncmp(p2, n) == FALSE ) p2 += n;
if ( p2 > eptr ) p2 = eptr;
fprintf(fp_out, "%d ", p2 - p1);
while ( p1 < p2 )
fprintf(fp_out, "%.2X", ((int) *p1++) & 0377);
fprintf(fp_out, " 0\n");
}
fprintf(fp_out, "0\n");
rptr = raster;
}
patncmp(p1, n)
char *p1;
int n;
{
char *p2;
p2 = p1 + n;
for ( ; n > 0; n--, p1++, p2++ )
if ( p2 >= eptr || *p1 != *p2 )
return(FALSE);
return(TRUE);
}
getint()
{
int h, l;
if ( (l = getc(fp_in)) == EOF || (h = getc(fp_in)) == EOF )
return(-1);
return((h & 0377) << 8 | (l & 0377));
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