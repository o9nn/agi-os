#define DEBUG
#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "awk.h"
#include "y.tab.h"
#include "regexp.h"
#define	MAXRE	512
static char	re[MAXRE];
char	*patbeg;
int	patlen;
#define	NPATS	20
static struct pat_list
{
char	*re;
int	use;
Reprog	*program;
} pattern[NPATS];
static int npats;
void
*compre(char *pat)
{
int i, j, inclass;
char c, *p, *s;
Reprog *program;
if (!compile_time) {
for (i = 0; i < npats; i++)
if (!strcmp(pat, pattern[i].re)) {
pattern[i].use++;
return((void *) pattern[i].program);
}
}
p = re;
s = pat;
inclass = 0;
while (c = *s++) {
if (c == '\\') {
quoted(&s, &p, re+MAXRE);
continue;
}
else if (!inclass && c == '(' && *s == ')') {
if (p < re+MAXRE-2) {
*p++ = '[';
*p++ = ']';
c = '*';
s++;
}
else overflow();
}
else if (c == '['){
inclass = 1;
if (*s == '-') {
if (p < re+MAXRE-2) {
*p++ = '[';
*p++ = '\\';
c = *s++;
}
else overflow();
}
else if (*s == '^' && s[1] == '-'){
if (p < re+MAXRE-3) {
*p++ = '[';
*p++ = *s++;
*p++ = '\\';
c = *s++;
}
else overflow();
}
else if (*s == '['){
if (p < re+MAXRE-1)
*p++ = c;
else overflow();
c = *s++;
}
else if (*s == '^' && s[1] == '[') {
if (p < re+MAXRE-2) {
*p++ = c;
*p++ = *s++;
c = *s++;
}
else overflow();
}
else if (*s == ']') {
if (p < re+MAXRE-2) {
*p++ = c;
*p++ = *s++;
c = '*';
inclass = 0;
}
else overflow();
}
}
else if (c == '-' && *s == ']') {
if (p < re+MAXRE-1)
*p++ = '\\';
else overflow();
}
else if (c == ']')
inclass = 0;
if (p < re+MAXRE-1)
*p++ = c;
else overflow();
}
*p = 0;
program = regcomp(re);
if (!compile_time) {
if (npats < NPATS)
i = npats++;
else {
int use = pattern[0].use;
i = 0;
for (j = 1; j < NPATS; j++) {
if (pattern[j].use < use) {
use = pattern[j].use;
i = j;
}
}
xfree(pattern[i].program);
xfree(pattern[i].re);
}
pattern[i].re = tostring(pat);
pattern[i].program = program;
pattern[i].use = 1;
}
return((void *) program);
}
int
match(void *p, char *s, char *)
{
return regexec((Reprog *) p, (char *) s, 0, 0);
}
int
pmatch(void *p, char *s, char *start)
{
Resub m;
m.s.sp = start;
m.e.ep = 0;
if (regexec((Reprog *) p, (char *) s, &m, 1)) {
patbeg = m.s.sp;
patlen = m.e.ep-m.s.sp;
return 1;
}
patlen = -1;
patbeg = start;
return 0;
}
int
nematch(void *p, char *s, char *start)
{
if (pmatch(p, s, start) == 1 && patlen > 0)
return 1;
patlen = -1;
patbeg = start;
return 0;
}
hexstr(char **pp)
{
char c;
int n = 0;
int i;
for (i = 0, c = (*pp)[i]; i < 4 && isxdigit(c); i++, c = (*pp)[i]) {
if (isdigit(c))
n = 16 * n + c - '0';
else if ('a' <= c && c <= 'f')
n = 16 * n + c - 'a' + 10;
else if ('A' <= c && c <= 'F')
n = 16 * n + c - 'A' + 10;
}
*pp += i;
return n;
}
#define isoctdigit(c) ((c) >= '0' && (c) <= '7')
void
quoted(char **s, char **to, char *end)
{
char *p = *s;
char *t = *to;
wchar_t c;
switch(c = *p++) {
case 't':
c = '\t';
break;
case 'n':
c = '\n';
break;
case 'f':
c = '\f';
break;
case 'r':
c = '\r';
break;
case 'b':
c = '\b';
break;
default:
if (t < end-1)
*t++ = '\\';
if (c == 'x') {
c = hexstr(&p);
if (t < end-MB_CUR_MAX)
t += wctomb(t, c);
else overflow();
*to = t;
*s = p;
return;
} else if (isoctdigit(c)) {
c -= '0';
if (isoctdigit(*p)) {
c = 8 * c + *p++ - '0';
if (isoctdigit(*p))
c = 8 * c + *p++ - '0';
}
}
break;
}
if (t < end-1)
*t++ = c;
*s = p;
*to = t;
}
int
countposn(char *s, int n)
{
int i, j;
char *end;
for (i = 0, end = s+n; *s && s < end; i++){
j = mblen(s, n);
if(j <= 0)
j = 1;
s += j;
}
return(i);
}
void
regerror(char *s)
{
FATAL("%s", s);
}
void
overflow(void)
{
FATAL("%s", "regular expression too big");
}