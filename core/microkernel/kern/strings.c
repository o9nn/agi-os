#include <string.h>
#ifdef	strcpy
#undef strcmp
#undef strncmp
#undef strcpy
#undef strncpy
#undef strlen
#endif
int __attribute__ ((pure))
strcmp(
const char *s1,
const char *s2)
{
unsigned int a, b;
do {
a = *s1++;
b = *s2++;
if (a != b)
return a-b;
} while (a != '\0');
return 0;
}
int __attribute__ ((pure))
strncmp(
const char *s1,
const char *s2,
size_t n)
{
unsigned int a, b;
while (n != 0) {
a = *s1++;
b = *s2++;
if (a != b)
return a-b;
if (a == '\0')
return 0;
n--;
}
return 0;
}
char *
strcpy(
char *to,
const char *from)
{
char *ret = to;
while ((*to++ = *from++) != '\0')
continue;
return ret;
}
char *
strncpy(
char *to,
const char *from,
size_t count)
{
char *ret = to;
while (count != 0) {
count--;
if ((*to++ = *from++) == '\0')
break;
}
while (count != 0) {
*to++ = '\0';
count--;
}
return ret;
}
size_t __attribute__ ((pure))
strlen(
const char *string)
{
const char *ret = string;
while (*string++ != '\0')
continue;
return string - 1 - ret;
}
char *
strchr(
const char *s,
int c)
{
while (*s != c) {
if (*s == '\0') {
return NULL;
}
s++;
}
return (char *)s;
}
char *
strsep(
char **sp,
const char *delim)
{
const char *d;
char *s, *t;
s = t = *sp;
if (s == NULL) {
return NULL;
}
for (;;) {
if (*s == '\0') {
*sp = NULL;
return t;
}
d = delim;
for (;;) {
if (*d == '\0') {
break;
}
if (*d == *s) {
*s = '\0';
*sp = s + 1;
return t;
}
d++;
}
s++;
}
}
char *
strstr(
const char *s,
const char *find)
{
size_t len;
len = strlen(find);
if (len == 0) {
return (char *)s;
}
for (;;) {
if (*s == '\0') {
return NULL;
}
if (strncmp(s, find, len) == 0) {
return (char *)s;
}
s++;
}
}