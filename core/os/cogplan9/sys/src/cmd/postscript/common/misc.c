#define _BSD_EXTENSION
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/types.h>
#include <fcntl.h>
#include "gen.h"
#include "ext.h"
#include "path.h"
int nolist = 0;
int olist[50];
int
out_list(str)
char *str;
{
int start, stop;
while ( *str && nolist < sizeof(olist) - 2 ) {
start = stop = str_convert(&str, 0);
if ( *str == '-' && *str++ )
stop = str_convert(&str, 9999);
if ( start > stop )
error(FATAL, "illegal range %d-%d", start, stop, 0);
olist[nolist++] = start;
olist[nolist++] = stop;
if ( *str != '\0' ) str++;
}
olist[nolist] = 0;
return 0;
}
in_olist(num)
int num;
{
int i;
if ( nolist == 0 )
return(ON);
for ( i = 0; i < nolist; i += 2 )
if ( num >= olist[i] && num <= olist[i+1] )
return(ON);
return(OFF);
}
int
setencoding(name)
char *name;
{
char path[150];
if ( name == NULL )
name = "Default";
if ( *name == '/' )
strcpy(path, name);
else sprintf(path, "%s/%s.enc", ENCODINGDIR, name);
if ( cat(path) == TRUE )
writing = strncmp(name, "UTF", 3) == 0;
return 0;
}
cat(file)
char *file;
{
int fd_in;
int fd_out;
char buf[512];
int count;
fflush(stdout);
if ( (fd_in = open(file, O_RDONLY)) == -1 )
return(FALSE);
fd_out = fileno(stdout);
while ( (count = read(fd_in, buf, sizeof(buf))) > 0 )
write(fd_out, buf, count);
close(fd_in);
return(TRUE);
}
str_convert(str, err)
char **str;
int err;
{
int i;
if ( ! isdigit(**str) )
return(err);
for ( i = 0; isdigit(**str); *str += 1 )
i = 10 * i + **str - '0';
return(i);
}
void
error(int kind, char *fmt, ...)
{
char buf[256];
va_list arg;
if (fmt) {
va_start(arg, fmt);
vsnprintf(buf, sizeof buf, fmt, arg);
va_end(arg);
fprintf(stderr, "%s: %s", prog_name, buf);
if (lineno > 0)
fprintf(stderr, " (line %d)", lineno);
if (position > 0)
fprintf(stderr, " (near byte %d)", position);
putc('\n', stderr);
}
if (kind == FATAL && ignore == OFF) {
if (temp_file != NULL)
unlink(temp_file);
exit(x_stat | 01);
}
}
void
interrupt(int sig)
{
USED(sig);
if ( temp_file != NULL )
unlink(temp_file);
exit(1);
}