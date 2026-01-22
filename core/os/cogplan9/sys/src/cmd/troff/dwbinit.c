#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "dwbinit.h"
#ifndef DWBCONFIG
#define DWBCONFIG "/dev/null"
#endif
#ifndef DWBENV
#define DWBENV "DWBHOME"
#endif
#ifndef DWBHOME
#define DWBHOME ""
#endif
#ifndef DWBDEBUG
#define DWBDEBUG "DWBDEBUG"
#endif
#ifndef DWBPREFIX
#define DWBPREFIX "\\*(.P"
#endif
void DWBdebug(dwbinit *ptr, int level)
{
char *path;
char *home;
static char *debug = NULL;
if ( debug == NULL && (debug = getenv(DWBDEBUG)) == NULL )
debug = "OFF";
if ( strcmp(debug, "ON") == 0 ) {
if ( level == 0 ) {
fprintf(stderr, "Environment variable: %s\n", DWBENV);
fprintf(stderr, "Configuration file: %s\n", DWBCONFIG);
fprintf(stderr, "Default home: %s\n", DWBHOME);
if ( (home = DWBhome()) != NULL )
fprintf(stderr, "Current home: %s\n", home);
}
fprintf(stderr, "\n%s pathnames:\n", level == 0 ? "Original" : "Final");
for ( ; ptr->value != NULL || ptr->address != NULL; ptr++ ) {
if ( (path = ptr->value) == NULL ) {
path = *ptr->address;
fprintf(stderr, " pointer: %s\n", path);
} else fprintf(stderr, " array[%d]: %s\n", ptr->length, path);
if ( level == 0 && *path == '/' )
fprintf(stderr, "  WARNING - absolute path\n");
}
}
}
char *DWBhome(void)
{
FILE *fp;
char *ptr;
char *path;
int len;
char buf[200];
char *home = NULL;
if ( (fp = fopen(DWBCONFIG, "r")) != NULL ) {
len = strlen(DWBENV);
while ( fgets(buf, sizeof(buf), fp) != NULL ) {
for ( ptr = buf; isspace(*ptr); ptr++ ) ;
if ( strncmp(ptr, DWBENV, len) == 0 && *(ptr+len) == '=' ) {
path = ptr + len + 1;
for ( ptr = path; !isspace(*ptr) && *ptr != ';'; ptr++ ) ;
*ptr = '\0';
if ( home != NULL )
free(home);
if ( (home = malloc(strlen(path)+1)) != NULL )
strcpy(home, path);
}
}
fclose(fp);
}
if ( home == NULL ) {
if ( (home = getenv(DWBENV)) == NULL ) {
if ( (home = DWBHOME) == NULL || *home == '\0' || *home == ' ' )
home = NULL;
}
}
while (home && *home == '/' && *(home +1) == '/')
home++;
return(home);
}
void DWBinit(char *prog, dwbinit *paths)
{
char *prefix;
char *value;
char *path;
int plen;
int length;
dwbinit *opaths = paths;
if ( (prefix = DWBhome()) == NULL ) {
fprintf(stderr, "%s: no DWB home directory\n", prog);
exit(1);
}
DWBdebug(opaths, 0);
plen = strlen(prefix);
for ( ; paths->value != NULL || paths->address != NULL; paths++ ) {
if ( paths->address == NULL ) {
length = 0;
value = paths->value;
} else {
length = paths->length;
value = *paths->address;
}
length += plen + 1 + strlen(value);
if ( (path = malloc(length+1)) == NULL ) {
fprintf(stderr, "%s: can't allocate pathname memory\n", prog);
exit(1);
}
if ( *value != '\0' ) {
char *eop = prefix;
while(*eop++)
;
eop -= 2;
if (*value != '/' && *eop != '/') {
sprintf(path, "%s/%s", prefix, value);
} else if (*value == '/' && *eop == '/') {
value++;
sprintf(path, "%s%s", prefix, value);
} else
sprintf(path, "%s%s", prefix, value);
} else
sprintf(path, "%s", prefix);
if ( paths->address == NULL ) {
if ( strlen(path) >= paths->length ) {
fprintf(stderr, "%s: no room for %s\n", prog, path);
exit(1);
}
strcpy(paths->value, path);
free(path);
} else *paths->address = path;
}
DWBdebug(opaths, 1);
}
void DWBprefix( char *prog, char *path, int length)
{
char *home;
char buf[512];
int len = strlen(DWBPREFIX);
if ( strncmp(path, DWBPREFIX, len) == 0 ) {
if ( (home = DWBhome()) != NULL ) {
if ( strlen(home) + strlen(path+len) < length ) {
sprintf(buf, "%s%s", home, path+len);
strcpy(path, buf);
} else fprintf(stderr, "%s: no room to grow path %s", prog, path);
}
}
}