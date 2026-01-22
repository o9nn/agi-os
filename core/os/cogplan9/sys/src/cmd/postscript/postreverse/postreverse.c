#define _BSD_EXTENSION
#define _POSIX_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <fcntl.h>
#include "comments.h"
#include "gen.h"
#include "path.h"
#include "ext.h"
#include "postreverse.h"
int page = 1;
int forms = 1;
char *temp_dir = TEMPDIR;
Pages pages[1000];
int next_page = 0;
long start;
long endoff = -1;
int noreverse = FALSE;
char *endprolog = ENDPROLOG;
double version = 3.3;
int ignoreversion = FALSE;
char buf[2048];
FILE *fp_in;
FILE *fp_out = stdout;
main(agc, agv)
int agc;
char *agv[];
{
argc = agc;
argv = agv;
prog_name = argv[0];
init_signals();
options();
arguments();
done();
exit(x_stat);
return 0;
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
options()
{
int ch;
char *optnames = "n:o:rvT:DI";
extern char *optarg;
extern int optind;
while ( (ch = getopt(argc, argv, optnames)) != EOF ) {
switch ( ch ) {
case 'n':
if ( (forms = atoi(optarg)) <= 0 )
error(FATAL, "illegal forms request %s", optarg);
break;
case 'o':
out_list(optarg);
break;
case 'r':
noreverse = TRUE;
break;
case 'v':
ignoreversion = TRUE;
break;
case 'T':
temp_dir = optarg;
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
arguments()
{
char *name;
if ( argc > 1 )
error(FATAL, "too many arguments");
if ( argc == 0 )
name = copystdin();
else name = *argv;
if ( (fp_in = fopen(name, "r")) == NULL )
error(FATAL, "can't open %s", name);
reverse();
}
done()
{
if ( temp_file != NULL )
unlink(temp_file);
}
char *copystdin()
{
int fd_out;
int fd_in;
int count;
if ( (temp_file = tempnam(temp_dir, "post")) == NULL )
error(FATAL, "can't generate temp file name");
if ( (fd_out = creat(temp_file, 0660)) == -1 )
error(FATAL, "can't open %s", temp_file);
fd_in = fileno(stdin);
while ( (count = read(fd_in, buf, sizeof(buf))) > 0 )
if ( write(fd_out, buf, count) != count )
error(FATAL, "error writing to %s", temp_file);
close(fd_out);
return(temp_file);
}
reverse()
{
if ( moreprolog(ENDPROLOG) == TRUE ) {
readpages();
writepages();
trailer();
}
}
moreprolog(str)
char *str;
{
int len;
int vlen;
len = strlen(FORMSPERPAGE);
vlen = strlen(VERSION);
while ( fgets(buf, sizeof(buf), fp_in) != NULL ) {
if ( strcmp(buf, str) == 0 )
return(TRUE);
else if ( strncmp(buf, FORMSPERPAGE, len) == 0 )
forms = atoi(&buf[len+1]);
else if ( strncmp(buf, VERSION, vlen) == 0 )
version = atof(&buf[vlen+1]);
fprintf(fp_out, "%s", buf);
}
return(FALSE);
}
readpages()
{
int endpagelen;
int pagelen;
int sawendpage = TRUE;
int gotpage = FALSE;
pages[0].start = ftell(fp_in);
endprolog = ENDPROLOG;
endpagelen = strlen(ENDPAGE);
pagelen = strlen(PAGE);
while ( fgets(buf, sizeof(buf), fp_in) != NULL )
if ( buf[0] != '%' )
continue;
else if ( strncmp(buf, ENDPAGE, endpagelen) == 0 ) {
if ( in_olist(page++) == ON ) {
pages[next_page].empty = FALSE;
pages[next_page++].stop = ftell(fp_in);
}
pages[next_page].start = ftell(fp_in);
sawendpage = TRUE;
gotpage = TRUE;
} else if ( strncmp(buf, PAGE, pagelen) == 0 ) {
if ( sawendpage == FALSE && in_olist(page++) == ON ) {
pages[next_page].empty = FALSE;
pages[next_page++].stop = ftell(fp_in) - strlen(buf);
}
pages[next_page].start = ftell(fp_in) - strlen(buf);
sawendpage = FALSE;
gotpage = TRUE;
} else if ( gotpage == FALSE && strcmp(buf, BEGINSETUP) == 0 ) {
fprintf(fp_out, "%s", endprolog);
fprintf(fp_out, "%s", BEGINSETUP);
moreprolog(ENDSETUP);
endprolog = ENDSETUP;
} else if ( strcmp(buf, BEGINGLOBAL) == 0 ) {
moreprolog(ENDGLOBAL);
} else if ( strcmp(buf, TRAILER) == 0 ) {
if ( sawendpage == FALSE )
pages[next_page++].stop = ftell(fp_in) - strlen(buf);
endoff = ftell(fp_in);
break;
}
}
writepages()
{
int i, j, k;
fprintf(fp_out, "%s", endprolog);
if ( noreverse == FALSE )
for ( i = (forms - next_page % forms) % forms; i > 0; i--, next_page++ )
pages[next_page].empty = TRUE;
else forms = next_page;
for ( i = next_page - forms; i >= 0; i -= forms )
for ( j = i, k = 0; k < forms; j++, k++ )
if ( pages[j].empty == TRUE ) {
if ( ignoreversion == TRUE || version > 3.1 ) {
fprintf(fp_out, "%s 0 0\n", PAGE);
fprintf(fp_out, "/saveobj save def\n");
fprintf(fp_out, "showpage\n");
fprintf(fp_out, "saveobj restore\n");
fprintf(fp_out, "%s 0 0\n", ENDPAGE);
} else {
fprintf(fp_out, "%s 0 0\n", PAGE);
fprintf(fp_out, "save showpage restore\n");
fprintf(fp_out, "%s 0 0\n", ENDPAGE);
}
} else copypage(pages[j].start, pages[j].stop);
}
copypage(start, stop)
long start;
long stop;
{
fseek(fp_in, start, 0);
while ( ftell(fp_in) < stop && fgets(buf, sizeof(buf), fp_in) != NULL )
if ( buf[0] == '%' && strcmp(buf, BEGINGLOBAL) == 0 )
while ( fgets(buf, sizeof(buf), fp_in) != NULL && strcmp(buf, ENDGLOBAL) != 0 ) ;
else fprintf(fp_out, "%s", buf);
}
trailer()
{
if ( endoff > 0 ) {
fprintf(fp_out, "%s", TRAILER);
fseek(fp_in, endoff, 0);
while ( fgets(buf, sizeof(buf), fp_in) != NULL )
fprintf(fp_out, "%s", buf);
}
}