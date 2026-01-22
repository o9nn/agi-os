#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#ifdef plan9
#define isascii(c) ((unsigned char)(c)<=0177)
#endif
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include "comments.h"
#include "gen.h"
#include "path.h"
#include "ext.h"
#include "postmd.h"
char *optnames = "a:b:c:d:g:i:m:n:o:p:w:x:y:A:C:E:J:L:P:R:DI";
char *prologue = POSTMD;
char *formfile = FORMFILE;
char *temp_dir = TEMPDIR;
int formsperpage = 1;
int copies = 1;
int bytespp = 6;
int dostats = ON;
int nxtstat = ON;
char *interval = DFLTILIST;
char *colormap = NULL;
char *window = NULL;
char *matrixname = "pipe.end";
Ilist ilist[128];
int next = 0;
int regions;
int wlist[4];
int page = 0;
int printed = 0;
int dfltrows = 0;
int dfltcols = 0;
int rows;
int columns;
int patcount = 0;
double element;
char *raster = NULL;
char *rptr;
char *eptr;
FILE *fp_in = stdin;
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
signal(SIGFPE, interrupt);
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
case 'd':
sscanf(optarg, "%dx%d", &dfltrows, &dfltcols);
break;
case 'g':
colormap = optarg;
break;
case 'i':
interval = optarg;
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
window = optarg;
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
if ( argc < 1 )
matrix();
else {
while ( argc > 0 ) {
matrixname = *argv;
if ( strcmp(*argv, "-") == 0 ) {
fp_in = stdin;
matrixname = "pipe.end";
} else if ( (fp_in = fopen(*argv, "r")) == NULL )
error(FATAL, "can't open %s", *argv);
matrix();
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
fprintf(stdout, "%s %d\n", PAGES, printed);
if ( temp_file != NULL )
unlink(temp_file);
}
account()
{
if ( fp_acct != NULL )
fprintf(fp_acct, " print %d\n copies %d\n", printed, copies);
}
matrix()
{
int count;
long total;
if ( fp_in == stdin )
copystdin();
rows = dfltrows;
columns = dfltcols;
buildilist(interval);
addcolormap(colormap);
setwindow(window);
nxtstat = dostats;
getheader();
dimensions();
patcount = 0;
total = rows * columns;
eptr = rptr + (wlist[2] - wlist[0] + 1);
redirect(++page);
fprintf(fp_out, "%s %d %d\n", PAGE, page, printed+1);
fprintf(fp_out, "/saveobj save def\n");
writerequest(printed+1, fp_out);
fprintf(fp_out, "%d %d bitmap\n", wlist[2] - wlist[0] + 1, wlist[3] - wlist[1] + 1);
while ( patcount != total && fscanf(fp_in, "%f", &element) != EOF ) {
if ( inwindow() ) *rptr++ = mapfloat(element);
if ( ++patcount % columns == 0 )
if ( inrange() )
putrow();
}
if ( total != patcount )
error(FATAL, "matrix format error");
labelmatrix();
if ( fp_out == stdout ) printed++;
fprintf(fp_out, "showpage\n");
fprintf(fp_out, "saveobj restore\n");
fprintf(fp_out, "%s %d %d\n", ENDPAGE, page, printed);
}
copystdin()
{
int fd_out;
int fd_in;
int buf[512];
int count;
if ( temp_file != NULL )
unlink(temp_file);
if ( (temp_file = tempnam(temp_dir, "post")) == NULL )
error(FATAL, "can't generate temp file name");
if ( (fd_out = creat(temp_file, 0660)) == -1 )
error(FATAL, "can't create %s", temp_file);
fd_in = fileno(stdin);
while ( (count = read(fd_in, buf, sizeof(buf))) > 0 )
if ( write(fd_out, buf, count) != count )
error(FATAL, "error writing to %s", temp_file);
close(fd_out);
if ( (fp_in = fopen(temp_file, "r")) == NULL )
error(FATAL, "can't open %s", temp_file);
}
getheader()
{
char buf[512];
char *cmap = NULL;
long pos;
pos = ftell(fp_in);
while ( fscanf(fp_in, "%s", buf) != EOF ) {
if ( strncmp(buf, "dimension", strlen("dimension")) == 0 )
fscanf(fp_in, "%dx%d", &rows, &columns);
else if ( strncmp(buf, "window", strlen("window")) == 0 ) {
fgets(buf, sizeof(buf), fp_in);
setwindow(buf);
} else if ( strncmp(buf, "name", strlen("name")) == 0 ) {
fgets(buf, sizeof(buf), fp_in);
matrixname = savestring(buf);
} else if ( strncmp(buf, "colormap", strlen("colormap")) == 0 ) {
fgets(buf, sizeof(buf), fp_in);
cmap = savestring(buf);
} else if ( strncmp(buf, "grayscale", strlen("grayscale")) == 0 ) {
fgets(buf, sizeof(buf), fp_in);
cmap = savestring(buf);
} else if ( strncmp(buf, "interval", strlen("interval")) == 0 ) {
fgets(buf, sizeof(buf), fp_in);
buildilist(buf);
} else if ( strncmp(buf, "statistics", strlen("statistics")) == 0 ) {
fscanf(fp_in, "%s", buf);
if ( strcmp(buf, "on") == 0 || strcmp(buf, "ON") == 0 )
nxtstat = ON;
else nxtstat = OFF;
} else break;
pos = ftell(fp_in);
}
addcolormap(cmap);
fseek(fp_in, pos, 0);
}
dimensions()
{
char buf[100];
long count = 0;
long pos;
if ( rows == 0 ) {
pos = ftell(fp_in);
while ( fscanf(fp_in, "%s", buf) != EOF )
count++;
rows = sqrt((double) count);
fseek(fp_in, pos, 0);
}
if ( columns <= 0 ) columns = rows;
if ( raster != NULL ) free(raster);
if ( (rptr = raster = malloc(columns)) == NULL )
error(FATAL, "no memory");
eptr = rptr + columns;
if ( rows <= 0 || columns <= 0 )
error(FATAL, "bad matrix dimensions");
if ( wlist[0] > wlist[2] || wlist[1] > wlist[3] ) {
wlist[0] = wlist[1] = 1;
wlist[2] = columns;
wlist[3] = rows;
}
}
buildilist(list)
char *list;
{
static char *templist = NULL;
char *ptr;
int i;
if ( templist != NULL )
free(templist);
while ( isascii(*list) && isspace(*list) )
list++;
for ( ptr = list, regions = 3; *ptr != '\0'; ptr++ ) {
if ( *ptr == ',' || *ptr == '/' || isspace(*ptr) )
regions += 2;
while ( isascii(*ptr) && isspace(*ptr) ) ptr++;
}
next = 0;
templist = savestring(list);
ptr = strtok(templist, ",/ \t\n");
while ( ptr != NULL ) {
ilist[next].count = 0;
ilist[next++].color = 254 * (regions - 1 - next) / (regions - 1);
ilist[next].val = atof(ptr);
ilist[next].count = 0;
ilist[next++].color = 254 * (regions - 1 - next) / (regions - 1);
ptr = strtok(NULL, ",/ \t\n");
}
ilist[next].count = 0;
ilist[next].color = 254 * (regions - 1 - next) / (regions - 1);
if ( next == 0 )
error(FATAL, "missing interval list");
for ( i = 3; i < next; i += 2 )
if ( ilist[i].val <= ilist[i-2].val )
error(FATAL, "bad interval list");
}
addcolormap(list)
char *list;
{
static char *templist = NULL;
char *ptr;
int i = 0;
if ( list != NULL ) {
if ( templist != NULL )
free(templist);
templist = savestring(list);
ptr = strtok(templist, ",/ \t\n");
while ( ptr != NULL ) {
ilist[i++].color = atoi(ptr) % 256;
ptr = strtok(NULL, ",/ \t\n");
}
}
}
setwindow(list)
char *list;
{
static char *templist = NULL;
char *ptr;
int i = 0;
wlist[0] = wlist[1] = 1;
wlist[2] = wlist[3] = 0;
if ( list != NULL ) {
if ( templist != NULL )
free(templist);
templist = savestring(list);
ptr = strtok(templist, ",/ \t\n");
while ( ptr != NULL ) {
wlist[i++] = atoi(ptr);
ptr = strtok(NULL, ",/ \t\n");
}
}
}
inwindow()
{
int r;
int c;
r = (patcount/columns) + 1;
c = (patcount%columns) + 1;
return((c >= wlist[0]) && (r >= wlist[1]) && (c <= wlist[2]) && (r <= wlist[3]));
}
inrange()
{
return(((patcount/columns) >= wlist[1]) && ((patcount/columns) <= wlist[3]));
}
mapfloat(element)
double element;
{
int i;
for ( i = 1; i < next && ilist[i].val < element; i += 2 ) ;
if ( i > next || element < ilist[i].val )
i--;
ilist[i].count++;
return(ilist[i].color);
}
putrow()
{
char *p1, *p2;
int n;
int i;
n = (bytespp <= 0) ? columns : bytespp;
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
labelmatrix()
{
int total;
int i;
fprintf(fp_out, "(%s) ((%d, %d) to (%d, %d)) labelmatrix\n", matrixname,
wlist[0], wlist[1], wlist[2], wlist[3]);
total = (wlist[2] - wlist[0] + 1) * (wlist[3] - wlist[1] + 1);
if ( nxtstat == OFF )
for ( i = 0; i < regions; i++ )
ilist[i].count = 0;
for ( i = 1; i < next; i += 2 )
fprintf(fp_out, "(%g) ", ilist[i].val);
fprintf(fp_out, "%d ", (regions - 1) / 2);
for ( i = regions - 1; i >= 0; i-- )
fprintf(fp_out, "{(\\%.3o)} %d ", ilist[i].color, ilist[i].count);
fprintf(fp_out, "%d %d legend\n", total, regions);
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
char *savestring(str)
char *str;
{
char *ptr = NULL;
if ( str != NULL && *str != '\0' ) {
if ( (ptr = malloc(strlen(str) + 1)) == NULL )
error(FATAL, "no memory available for string %s", str);
strcpy(ptr, str);
}
return(ptr);
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