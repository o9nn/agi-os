#define _BSD_EXTENSION
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "comments.h"
#include "gen.h"
#include "path.h"
#include "ext.h"
#include "download.h"
char *temp_dir = TEMPDIR;
char *hostfontdir = HOSTDIR;
char *mapname = "map";
char *suffix = "";
Map *map = NULL;
char *stringspace = NULL;
int next = 0;
char *residentfonts = NULL;
char *printer = NULL;
char buf[2048];
char *comment = DOCUMENTFONTS;
int atend = FALSE;
FILE *fp_in = stdin;
FILE *fp_temp = NULL;
void arguments(void);
void copyfonts(char *);
void copyinput(void);
void done(void);
void download(void);
void init_signals(void);
void options(void);
void readmap(void);
void readresident(void);
main(agc, agv)
int agc;
char *agv[];
{
argc = agc;
argv = agv;
prog_name = argv[0];
init_signals();
options();
readmap();
readresident();
arguments();
done();
exit(x_stat);
return 0;
}
void
init_signals(void)
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
void
options(void)
{
int ch;
char *optnames = "c:fm:p:r:H:T:DI";
extern char *optarg;
extern int optind;
while ( (ch = getopt(argc, argv, optnames)) != EOF ) {
switch ( ch ) {
case 'c':
comment = optarg;
break;
case 'f':
atend = TRUE;
break;
case 'm':
mapname = optarg;
break;
case 'p':
printer = optarg;
break;
case 'r':
residentfonts = optarg;
break;
case 'H':
hostfontdir = optarg;
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
void
readmap(void)
{
char *path;
char *ptr;
int fd;
struct stat sbuf;
if ( hostfontdir == NULL || mapname == NULL )
return;
if ( *mapname != '/' ) {
if ( (path = malloc(strlen(hostfontdir) + strlen(mapname) +
strlen(suffix) + 2)) == NULL )
error(FATAL, "no memory");
sprintf(path, "%s/%s%s", hostfontdir, mapname, suffix);
} else path = mapname;
if ( (fd = open(path, 0)) != -1 ) {
if ( fstat(fd, &sbuf) == -1 )
error(FATAL, "can't fstat %s", path);
if ( (stringspace = malloc(sbuf.st_size + 2)) == NULL )
error(FATAL, "no memory for %s (%d bytes)", path, sbuf.st_size + 2);
if ( read(fd, stringspace, sbuf.st_size) == -1 )
error(FATAL, "can't read %s", path);
close(fd);
stringspace[sbuf.st_size] = '\n';
stringspace[sbuf.st_size+1] = '\0';
for ( ptr = stringspace; *ptr != '\0'; ptr++ )
if ( *ptr == '%' )
for ( ; *ptr != '\n' ; ptr++ )
*ptr = ' ';
for ( ptr = stringspace; ; next++ ) {
if ( (next % 50) == 0 )
map = allocate(map, next+50);
map[next].downloaded = FALSE;
map[next].font = strtok(ptr, " \t\n");
map[next].file = strtok(ptr = NULL, " \t\n");
if ( map[next].font == NULL )
break;
if ( map[next].file == NULL )
error(FATAL, "map table format error - check %s", path);
}
}
}
void
readresident(void)
{
FILE *fp;
char *path;
int ch;
int n;
if ( next == 0 || (printer == NULL && residentfonts == NULL) )
return;
if ( printer != NULL ) {
sprintf(buf, "%s/printers/%s", HOSTDIR, printer);
path = buf;
} else path = residentfonts;
if ( (fp = fopen(path, "r")) != NULL ) {
while ( fscanf(fp, "%s", buf) != EOF )
if ( buf[0] == '%' )
while ( (ch = getc(fp)) != EOF && ch != '\n' ) ;
else if ( (n = lookup(buf)) < next )
map[n].downloaded = TRUE;
fclose(fp);
}
}
void
arguments(void)
{
if ( argc < 1 )
download();
else {
while ( argc > 0 ) {
fp_temp = NULL;
if ( strcmp(*argv, "-") == 0 )
fp_in = stdin;
else if ( (fp_in = fopen(*argv, "r")) == NULL )
error(FATAL, "can't open %s", *argv);
download();
if ( fp_in != stdin )
fclose(fp_in);
if ( fp_temp != NULL )
fclose(fp_temp);
argc--;
argv++;
}
}
}
void
done(void)
{
if ( temp_file != NULL )
unlink(temp_file);
}
void
download(void)
{
int infontlist = FALSE;
if ( next > 0 ) {
if ( fp_in == stdin ) {
if ( (temp_file = tempnam(temp_dir, "post")) == NULL )
error(FATAL, "can't generate temp file name");
if ( (fp_temp = fopen(temp_file, "w+r")) == NULL )
error(FATAL, "can't open %s", temp_file);
unlink(temp_file);
}
while ( fgets(buf, sizeof(buf), fp_in) != NULL ) {
if ( fp_temp != NULL )
fprintf(fp_temp, "%s", buf);
if ( buf[0] != '%' || buf[1] != '%' ) {
if ( (buf[0] != '%' || buf[1] != '!') && atend == FALSE )
break;
infontlist = FALSE;
} else if ( strncmp(buf, comment, strlen(comment)) == 0 ) {
copyfonts(buf);
infontlist = TRUE;
} else if ( buf[2] == '+' && infontlist == TRUE )
copyfonts(buf);
else infontlist = FALSE;
}
}
copyinput();
}
void
copyfonts(list)
char *list;
{
char *font;
char *path;
int n;
strtok(list, " \n");
while ( (font = strtok(NULL, " \t\n")) != NULL ) {
if ( strcmp(font, ATEND) == 0 ) {
atend = TRUE;
break;
}
if ( (n = lookup(font)) < next ) {
if ( *map[n].file != '/' ) {
if ( (path = malloc(strlen(hostfontdir)+strlen(map[n].file)+2)) == NULL )
error(FATAL, "no memory");
sprintf(path, "%s/%s", hostfontdir, map[n].file);
cat(path);
free(path);
} else cat(map[n].file);
map[n].downloaded = TRUE;
}
}
}
void
copyinput(void)
{
if ( fp_temp != NULL ) {
fseek(fp_temp, 0L, 0);
while ( fgets(buf, sizeof(buf), fp_temp) != NULL )
printf("%s", buf);
}
if ( fp_in != stdin )
fseek(fp_in, 0L, 0);
while ( fgets(buf, sizeof(buf), fp_in) != NULL )
printf("%s", buf);
}
lookup(font)
char *font;
{
int i;
for ( i = 0; i < next; i++ )
if ( strcmp(font, map[i].font) == 0 ) {
if ( map[i].downloaded == TRUE )
i = next;
break;
}
return(i);
}
Map *
allocate(Map *ptr, int num)
{
if (ptr == NULL)
ptr = (Map *)malloc(num * sizeof(Map));
else
ptr = (Map *)realloc(ptr, num * sizeof(Map));
if (ptr == NULL)
error(FATAL, "no map memory");
return ptr;
}