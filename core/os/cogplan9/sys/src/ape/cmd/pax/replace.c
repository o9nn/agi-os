#ifndef lint
static char *ident = "$Id: replace.c,v 1.2 89/02/12 10:05:59 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
void add_replstr(char *pattern)
#else
void add_replstr(pattern)
char *pattern;
#endif
{
char *p;
char sep;
Replstr *rptr;
int len;
if ((len = strlen(pattern)) < 4) {
warn("Replacement string not added",
"Malformed substitution syntax");
return;
}
if ((rptr = (Replstr *) malloc(sizeof(Replstr))) == (Replstr *)NULL) {
warn("Replacement string not added", "No space");
return;
}
sep = *pattern;
p = pattern + len - 1;
while (*p != sep) {
if (*p == 'g') {
rptr->global = 1;
} else if (*p == 'p') {
rptr->print = 1;
} else {
warn(p, "Invalid RE modifier");
}
p--;
}
if (*p != sep) {
warn("Replacement string not added", "Bad delimeters");
free(rptr);
return;
}
*p = '\0';
pattern++;
p = pattern;
while (*p) {
if (*p == sep) {
break;
}
if (*p == '\\' && *(p + 1) != '\0') {
p++;
}
p++;
}
if (*p != sep) {
warn("Replacement string not added", "Bad delimeters");
free(rptr);
return;
}
*p++ = '\0';
if ((rptr->comp = regcomp(pattern)) == (regexp *)NULL) {
warn("Replacement string not added", "Invalid RE");
free(rptr);
return;
}
rptr->replace = p;
rptr->next = (Replstr *)NULL;
if (rplhead == (Replstr *)NULL) {
rplhead = rptr;
rpltail = rptr;
} else {
rpltail->next = rptr;
rpltail = rptr;
}
}
#ifdef __STDC__
void rpl_name(char *name)
#else
void rpl_name(name)
char *name;
#endif
{
int found = 0;
int ret;
Replstr *rptr;
char buff[PATH_MAX + 1];
char buff1[PATH_MAX + 1];
char buff2[PATH_MAX + 1];
char *p;
char *b;
strcpy(buff, name);
for (rptr = rplhead; !found && rptr != (Replstr *)NULL; rptr = rptr->next) {
do {
if ((ret = regexec(rptr->comp, buff)) != 0) {
p = buff;
b = buff1;
while (p < rptr->comp->startp[0]) {
*b++ = *p++;
}
p = rptr->replace;
while (*p) {
*b++ = *p++;
}
strcpy(b, rptr->comp->endp[0]);
found = 1;
regsub(rptr->comp, buff1, buff2);
strcpy(buff, buff2);
}
} while (ret && rptr->global);
if (found) {
if (rptr->print) {
fprintf(stderr, "%s >> %s\n", name, buff);
}
strcpy(name, buff);
}
}
}
#ifdef __STDC__
int get_disposition(char *mode, char *name)
#else
int get_disposition(mode, name)
char *mode;
char *name;
#endif
{
char ans[2];
char buf[PATH_MAX + 10];
if (f_disposition) {
sprintf(buf, "%s %s? ", mode, name);
if (nextask(buf, ans, sizeof(ans)) == -1 || ans[0] == 'q') {
exit(0);
}
if (strlen(ans) == 0 || ans[0] != 'y') {
return(1);
}
}
return(0);
}
#ifdef __STDC__
int get_newname(char *name, int size)
#else
int get_newname(name, size)
char *name;
int size;
#endif
{
char buf[PATH_MAX + 10];
if (f_interactive) {
sprintf(buf, "rename %s? ", name);
if (nextask(buf, name, size) == -1) {
exit(0);
}
if (strlen(name) == 0) {
return(1);
}
}
return(0);
}