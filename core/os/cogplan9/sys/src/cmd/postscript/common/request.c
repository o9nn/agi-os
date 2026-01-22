#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gen.h"
#include "ext.h"
#include "request.h"
#include "path.h"
Request request[MAXREQUEST];
int nextreq = 0;
char *requestfile = REQUESTFILE;
void dumprequest(char *, char *, FILE *);
void writerequest(int, FILE *);
void
saverequest(want)
char *want;
{
char *page;
if ( nextreq < MAXREQUEST ) {
request[nextreq].want = strtok(want, ": ");
if ( (page = strtok(NULL, ": ")) == NULL )
request[nextreq].page = 0;
else request[nextreq].page = atoi(page);
if ( (request[nextreq].file = strtok(NULL, ": ")) == NULL )
request[nextreq].file = requestfile;
nextreq++;
} else error(NON_FATAL, "too many requests - ignoring %s", want);
}
void
writerequest(page, fp_out)
int page;
FILE *fp_out;
{
int i;
for ( i = 0; i < nextreq; i++ )
if ( request[i].page == page )
dumprequest(request[i].want, request[i].file, fp_out);
}
void
dumprequest(want, file, fp_out)
char *want;
char *file;
FILE *fp_out;
{
char buf[100];
FILE *fp_in;
if ( (fp_in = fopen(file, "r")) != NULL ) {
while ( fgets(buf, sizeof(buf), fp_in) != NULL )
if ( buf[0] == '@' && strncmp(want, &buf[1], strlen(want)) == 0 )
while ( fgets(buf, sizeof(buf), fp_in) != NULL )
if ( buf[0] == '#' || buf[0] == '%' )
continue;
else if ( buf[0] != '@' )
fprintf(fp_out, "%s", buf);
else break;
fclose(fp_in);
}
}