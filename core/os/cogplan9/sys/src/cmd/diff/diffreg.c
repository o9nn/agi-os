#include <u.h>
#include <libc.h>
#include <bio.h>
#include "diff.h"
struct cand {
int x;
int y;
int pred;
} cand;
struct line {
int serial;
int value;
} *file[2], line;
int len[2];
int binary;
struct line *sfile[2];
int slen[2];
int pref, suff;
int *class;
int *member;
int *klist;
struct cand *clist;
int clen;
int *J;
long *ixold;
long *ixnew;
static void
sort(struct line *a, int n)
{
int m;
struct line *ai, *aim, *j, *k;
struct line w;
int i;
m = 0;
for (i = 1; i <= n; i *= 2)
m = 2*i - 1;
for (m /= 2; m != 0; m /= 2) {
k = a+(n-m);
for (j = a+1; j <= k; j++) {
ai = j;
aim = ai+m;
do {
if (aim->value > ai->value ||
aim->value == ai->value &&
aim->serial > ai->serial)
break;
w = *ai;
*ai = *aim;
*aim = w;
aim = ai;
ai -= m;
} while (ai > a && aim >= ai);
}
}
}
static void
unsort(struct line *f, int l, int *b)
{
int *a;
int i;
a = MALLOC(int, (l+1));
for(i=1;i<=l;i++)
a[f[i].serial] = f[i].value;
for(i=1;i<=l;i++)
b[i] = a[i];
FREE(a);
}
static void
prune(void)
{
int i,j;
for(pref=0;pref<len[0]&&pref<len[1]&&
file[0][pref+1].value==file[1][pref+1].value;
pref++ ) ;
for(suff=0;suff<len[0]-pref&&suff<len[1]-pref&&
file[0][len[0]-suff].value==file[1][len[1]-suff].value;
suff++) ;
for(j=0;j<2;j++) {
sfile[j] = file[j]+pref;
slen[j] = len[j]-pref-suff;
for(i=0;i<=slen[j];i++)
sfile[j][i].serial = i;
}
}
static void
equiv(struct line *a, int n, struct line *b, int m, int *c)
{
int i, j;
i = j = 1;
while(i<=n && j<=m) {
if(a[i].value < b[j].value)
a[i++].value = 0;
else if(a[i].value == b[j].value)
a[i++].value = j;
else
j++;
}
while(i <= n)
a[i++].value = 0;
b[m+1].value = 0;
j = 0;
while(++j <= m) {
c[j] = -b[j].serial;
while(b[j+1].value == b[j].value) {
j++;
c[j] = b[j].serial;
}
}
c[j] = -1;
}
static int
newcand(int x, int  y, int pred)
{
struct cand *q;
clist = REALLOC(clist, struct cand, (clen+1));
q = clist + clen;
q->x = x;
q->y = y;
q->pred = pred;
return clen++;
}
static int
search(int *c, int k, int y)
{
int i, j, l;
int t;
if(clist[c[k]].y < y)
return k+1;
i = 0;
j = k+1;
while((l=(i+j)/2) > i) {
t = clist[c[l]].y;
if(t > y)
j = l;
else if(t < y)
i = l;
else
return l;
}
return l+1;
}
static int
stone(int *a, int n, int *b, int *c)
{
int i, k,y;
int j, l;
int oldc, tc;
int oldl;
k = 0;
c[0] = newcand(0,0,0);
for(i=1; i<=n; i++) {
j = a[i];
if(j==0)
continue;
y = -b[j];
oldl = 0;
oldc = c[0];
do {
if(y <= clist[oldc].y)
continue;
l = search(c, k, y);
if(l!=oldl+1)
oldc = c[l-1];
if(l<=k) {
if(clist[c[l]].y <= y)
continue;
tc = c[l];
c[l] = newcand(i,y,oldc);
oldc = tc;
oldl = l;
} else {
c[l] = newcand(i,y,oldc);
k++;
break;
}
} while((y=b[++j]) > 0);
}
return k;
}
static void
unravel(int p)
{
int i;
struct cand *q;
for(i=0; i<=len[0]; i++) {
if (i <= pref)
J[i] = i;
else if (i > len[0]-suff)
J[i] = i+len[1]-len[0];
else
J[i] = 0;
}
for(q=clist+p;q->y!=0;q=clist+q->pred)
J[q->x+pref] = q->y+pref;
}
static void
output(void)
{
int m, i0, i1, j0, j1;
m = len[0];
J[0] = 0;
J[m+1] = len[1]+1;
if (mode != 'e') {
for (i0 = 1; i0 <= m; i0 = i1+1) {
while (i0 <= m && J[i0] == J[i0-1]+1)
i0++;
j0 = J[i0-1]+1;
i1 = i0-1;
while (i1 < m && J[i1+1] == 0)
i1++;
j1 = J[i1+1]-1;
J[i1] = j1;
change(i0, i1, j0, j1);
}
}
else {
for (i0 = m; i0 >= 1; i0 = i1-1) {
while (i0 >= 1 && J[i0] == J[i0+1]-1 && J[i0])
i0--;
j0 = J[i0+1]-1;
i1 = i0+1;
while (i1 > 1 && J[i1-1] == 0)
i1--;
j1 = J[i1-1]+1;
J[i1] = j1;
change(i1 , i0, j1, j0);
}
}
if (m == 0)
change(1, 0, 1, len[1]);
flushchanges();
}
#define BUF 4096
static int
cmp(Biobuf* b1, Biobuf* b2)
{
int n;
uchar buf1[BUF], buf2[BUF];
int f1, f2;
vlong nc = 1;
uchar *b1s, *b1e, *b2s, *b2e;
f1 = Bfildes(b1);
f2 = Bfildes(b2);
seek(f1, 0, 0);
seek(f2, 0, 0);
b1s = b1e = buf1;
b2s = b2e = buf2;
for(;;){
if(b1s >= b1e){
if(b1s >= &buf1[BUF])
b1s = buf1;
n = read(f1, b1s,  &buf1[BUF] - b1s);
b1e = b1s + n;
}
if(b2s >= b2e){
if(b2s >= &buf2[BUF])
b2s = buf2;
n = read(f2, b2s,  &buf2[BUF] - b2s);
b2e = b2s + n;
}
n = b2e - b2s;
if(n > b1e - b1s)
n = b1e - b1s;
if(n <= 0)
break;
if(memcmp((void *)b1s, (void *)b2s, n) != 0){
return 1;
}
nc += n;
b1s += n;
b2s += n;
}
if(b1e - b1s == b2e - b2s)
return 0;
return 1;
}
void
diffreg(char *f, char *t)
{
Biobuf *b0, *b1;
int k;
binary = 0;
b0 = prepare(0, f);
if (!b0)
return;
b1 = prepare(1, t);
if (!b1) {
Bterm(b0);
return;
}
if (binary){
if (cmp(b0, b1))
print("binary files %s %s differ\n", f, t);
Bterm(b0);
Bterm(b1);
return;
}
clen = 0;
prune();
sort(sfile[0], slen[0]);
sort(sfile[1], slen[1]);
member = (int *)file[1];
equiv(sfile[0], slen[0], sfile[1], slen[1], member);
member = REALLOC(member, int, slen[1]+2);
class = (int *)file[0];
unsort(sfile[0], slen[0], class);
class = REALLOC(class, int, slen[0]+2);
klist = MALLOC(int, slen[0]+2);
clist = MALLOC(struct cand, 1);
k = stone(class, slen[0], member, klist);
FREE(member);
FREE(class);
J = MALLOC(int, len[0]+2);
unravel(klist[k]);
FREE(clist);
FREE(klist);
ixold = MALLOC(long, len[0]+2);
ixnew = MALLOC(long, len[1]+2);
Bseek(b0, 0, 0); Bseek(b1, 0, 0);
check(b0, b1);
output();
FREE(J); FREE(ixold); FREE(ixnew);
Bterm(b0); Bterm(b1);
}