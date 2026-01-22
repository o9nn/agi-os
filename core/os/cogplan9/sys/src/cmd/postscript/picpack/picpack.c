#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include "gen.h"
#include "ext.h"
#include "path.h"
char *keys[11] = {".BP", ".PI", NULL};
int quiet = FALSE;
FILE *fp_in = stdin;
FILE *fp_out = stdout;
main(agc, agv)
int agc;
char *agv[];
{
argc = agc;
argv = agv;
prog_name = argv[0];
options();
arguments();
done();
exit(x_stat);
}
options()
{
int ch;
extern char *optarg;
extern int optind;
while ( (ch = getopt(argc, argv, "k:qDI")) != EOF ) {
switch ( ch ) {
case 'k':
newkeys(optarg);
break;
case 'q':
quiet = TRUE;
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
newkeys(list)
char *list;
{
char *p;
int i;
int n;
n = (sizeof(keys) / sizeof(char *)) - 1;
for ( i = 0, p = strtok(list, " ,"); p != NULL; i++, p = strtok(NULL, " ,") )
if ( i >= n )
error(FATAL, "too many key strings");
else keys[i] = p;
keys[i] = NULL;
}
arguments()
{
FILE *copystdin();
if ( argc < 1 ) {
fp_in = copystdin();
picpack();
} else
while ( argc > 0 ) {
if ( strcmp(*argv, "-") == 0 )
fp_in = copystdin();
else if ( (fp_in = fopen(*argv, "r")) == NULL )
error(FATAL, "can't open %s", *argv);
picpack();
fclose(fp_in);
argc--;
argv++;
}
}
FILE *copystdin()
{
char *tfile;
int fd_out;
FILE *fp;
if ( (tfile = tempnam(TEMPDIR, "post")) == NULL )
error(FATAL, "can't generate temp file name");
if ( (fd_out = creat(tfile, 0660)) == -1 )
error(FATAL, "can't create %s", tfile);
copyfile(fileno(stdin), fd_out);
close(fd_out);
if ( (fp = fopen(tfile, "r")) == NULL )
error(FATAL, "can't open %s", tfile);
unlink(tfile);
return(fp);
}
copyfile(fd_in, fd_out)
int fd_in;
int fd_out;
{
char buf[512];
int count;
while ( (count = read(fd_in, buf, sizeof(buf))) > 0 )
if ( write(fd_out, buf, count) != count )
error(FATAL, "write error");
}
done()
{
if ( temp_file != NULL )
unlink(temp_file);
}
picpack()
{
char line[512];
char name[100];
int i;
while ( fgets(line, sizeof(line), fp_in) != NULL ) {
for ( i = 0; keys[i] != NULL; i++ )
if ( strncmp(line, keys[i], strlen(keys[i])) == 0 ) {
if ( sscanf(line, "%*s %s", name) == 1 ) {
strtok(name, "(");
if ( gotpicfile(name) == FALSE )
inline(name);
}
}
}
fflush(fp_out);
fseek(fp_in, 0L, 0);
copyfile(fileno(fp_in), fileno(fp_out));
}
inline(name)
char *name;
{
long size;
FILE *fp;
int ch;
int lastch = '\n';
struct stat sbuf;
if ( (fp = fopen(name, "r")) != NULL ) {
fstat(fileno(fp), &sbuf);
if ( (size = sbuf.st_size) > 0 ) {
fprintf(fp_out, "\\!x X InlinePicture %s %ld\n", name, size);
while ( (ch = getc(fp)) != EOF ) {
if ( lastch == '\n' )
fprintf(fp_out, "\\!");
if ( ch == '\\' )
putc('\\', fp_out);
putc(lastch = ch, fp_out);
}
if ( lastch != '\n' )
putc('\n', fp_out);
}
fclose(fp);
addpicfile(name);
} else if ( quiet == FALSE )
error(NON_FATAL, "can't read picture file %s", name);
}
gotpicfile(name)
char *name;
{
char buf[100];
FILE *fp_pic;
if ( temp_file != NULL )
if ( (fp_pic = fopen(temp_file, "r")) != NULL ) {
while ( fscanf(fp_pic, "%s", buf) != EOF )
if ( strcmp(buf, name) == 0 ) {
fclose(fp_pic);
return(TRUE);
}
fclose(fp_pic);
}
return(FALSE);
}
addpicfile(name)
char *name;
{
FILE *fp_pic;
if ( temp_file == NULL )
if ( (temp_file = tempnam(TEMPDIR, "picpac")) == NULL )
return;
if ( (fp_pic = fopen(temp_file, "a")) != NULL ) {
fprintf(fp_pic, "%s\n", name);
fclose(fp_pic);
}
}