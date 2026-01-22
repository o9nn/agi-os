#include <u.h>
#include <libc.h>
#include <bio.h>
#include "sky.h"
static void unshuffle(Pix*, int, int, Pix*);
static void unshuffle1(Pix*, int, Pix*);
void
hinv(Pix *a, int nx, int ny)
{
int nmax, log2n, h0, hx, hy, hc, i, j, k;
int nxtop, nytop, nxf, nyf, c;
int oddx, oddy;
int shift;
int s10, s00;
Pix *tmp;
nmax = ny;
if(nx > nmax)
nmax = nx;
log2n = log(nmax)/LN2 + 0.5;
if(nmax > (1<<log2n))
log2n++;
tmp = (Pix*)malloc(((nmax+1)/2) * sizeof(*tmp));
if(tmp == nil) {
fprint(2, "hinv: insufficient memory\n");
exits("memory");
}
shift = 1;
nxtop = 1;
nytop = 1;
nxf = nx;
nyf = ny;
c = 1<<log2n;
for(k = log2n-1; k>=0; k--) {
c = c>>1;
nxtop = nxtop<<1;
nytop = nytop<<1;
if(nxf <= c)
nxtop--;
else
nxf -= c;
if(nyf <= c)
nytop--;
else
nyf -= c;
if(k == 0)
shift = 0;
for(i = 0; i<nxtop; i++)
unshuffle1(&a[ny*i], nytop, tmp);
for(j = 0; j<nytop; j++)
unshuffle(&a[j], nxtop, ny, tmp);
oddx = nxtop & 1;
oddy = nytop & 1;
for(i = 0; i<nxtop-oddx; i += 2) {
s00 = ny*i;
s10 = s00+ny;
for(j = 0; j<nytop-oddy; j += 2) {
h0 = a[s00 ] << shift;
hx = a[s10 ] << shift;
hy = a[s00+1] << shift;
hc = a[s10+1] << shift;
a[s10+1] = (h0 + hx + hy + hc + 2) >> 2;
a[s10 ] = (h0 + hx - hy - hc + 2) >> 2;
a[s00+1] = (h0 - hx + hy - hc + 2) >> 2;
a[s00 ] = (h0 - hx - hy + hc + 2) >> 2;
s00 += 2;
s10 += 2;
}
if(oddy) {
h0 = a[s00 ] << shift;
hx = a[s10 ] << shift;
a[s10 ] = (h0 + hx + 2) >> 2;
a[s00 ] = (h0 - hx + 2) >> 2;
}
}
if(oddx) {
s00 = ny*i;
for(j = 0; j<nytop-oddy; j += 2) {
h0 = a[s00 ] << shift;
hy = a[s00+1] << shift;
a[s00+1] = (h0 + hy + 2) >> 2;
a[s00 ] = (h0 - hy + 2) >> 2;
s00 += 2;
}
if(oddy) {
h0 = a[s00 ] << shift;
a[s00 ] = (h0 + 2) >> 2;
}
}
}
free(tmp);
}
static
void
unshuffle(Pix *a, int n, int n2, Pix *tmp)
{
int i;
int nhalf, twon2, n2xnhalf;
Pix *p1, *p2, *pt;
twon2 = n2<<1;
nhalf = (n+1)>>1;
n2xnhalf = n2*nhalf;
pt = tmp;
p1 = &a[n2xnhalf];
for(i=nhalf; i<n; i++) {
*pt = *p1;
pt++;
p1 += n2;
}
p2 = &a[n2xnhalf];
p1 = &a[n2xnhalf<<1];
for(i=nhalf-1; i>=0; i--) {
p1 -= twon2;
p2 -= n2;
*p1 = *p2;
}
pt = tmp;
p1 = &a[n2];
for(i=1; i<n; i+=2) {
*p1 = *pt;
p1 += twon2;
pt++;
}
}
static
void
unshuffle1(Pix *a, int n, Pix *tmp)
{
int i;
int nhalf;
Pix *p1, *p2, *pt;
nhalf = (n+1) >> 1;
pt = tmp;
p1 = &a[nhalf];
for(i=nhalf; i<n; i++) {
*pt = *p1;
pt++;
p1++;
}
p2 = &a[nhalf];
p1 = &a[nhalf<<1];
for(i=nhalf-1; i>=0; i--) {
p1 -= 2;
p2--;
*p1 = *p2;
}
pt = tmp;
p1 = &a[1];
for(i=1; i<n; i+=2) {
*p1 = *pt;
p1 += 2;
pt++;
}
}