#include	<u.h>
#include	<libc.h>
#include	<bio.h>
#include	"sky.h"
static void	qtree_expand(Biobuf*, uchar*, int, int, uchar*);
static void	qtree_copy(uchar*, int, int, uchar*, int);
static void	qtree_bitins(uchar*, int, int, Pix*, int, int);
static void	read_bdirect(Biobuf*, Pix*, int, int, int, uchar*, int);
void
qtree_decode(Biobuf *infile, Pix *a, int n, int nqx, int nqy, int nbitplanes)
{
int log2n, k, bit, b, nqmax;
int nx,ny,nfx,nfy,c;
int nqx2, nqy2;
unsigned char *scratch;
nqmax = nqy;
if(nqx > nqmax)
nqmax = nqx;
log2n = log(nqmax)/LN2+0.5;
if (nqmax > (1<<log2n))
log2n++;
nqx2 = (nqx+1)/2;
nqy2 = (nqy+1)/2;
scratch = (uchar*)malloc(nqx2*nqy2);
if(scratch == nil) {
fprint(2, "qtree_decode: insufficient memory\n");
exits("memory");
}
for(bit = nbitplanes-1; bit >= 0; bit--) {
b = input_nybble(infile);
if(b == 0) {
read_bdirect(infile, a, n, nqx, nqy, scratch, bit);
} else
if(b != 0xf) {
fprint(2, "qtree_decode: bad format code %x\n",b);
exits("format");
} else {
scratch[0] = input_huffman(infile);
nx = 1;
ny = 1;
nfx = nqx;
nfy = nqy;
c = 1<<log2n;
for(k = 1; k<log2n; k++) {
c = c>>1;
nx = nx<<1;
ny = ny<<1;
if(nfx <= c)
nx--;
else
nfx -= c;
if(nfy <= c)
ny--;
else
nfy -= c;
qtree_expand(infile, scratch, nx, ny, scratch);
}
qtree_bitins(scratch, nqx, nqy, a, n, bit);
}
}
free(scratch);
}
static
void
qtree_expand(Biobuf *infile, uchar *a, int nx, int ny, uchar *b)
{
uchar *b1;
qtree_copy(a, nx, ny, b, ny);
b1 = &b[nx*ny];
while(b1 > b) {
b1--;
if(*b1 != 0)
*b1 = input_huffman(infile);
}
}
static
void
qtree_copy(uchar *a, int nx, int ny, uchar *b, int n)
{
int i, j, k, nx2, ny2;
int s00, s10;
nx2 = (nx+1)/2;
ny2 = (ny+1)/2;
k = ny2*(nx2-1) + ny2-1;
for (i = nx2-1; i >= 0; i--) {
s00 = 2*(n*i+ny2-1);
for (j = ny2-1; j >= 0; j--) {
b[s00] = a[k];
k -= 1;
s00 -= 2;
}
}
for(i = 0; i<nx-1; i += 2) {
s00 = n*i;
s10 = s00+n;
for(j = 0; j<ny-1; j += 2) {
b[s10+1] =  b[s00]     & 1;
b[s10  ] = (b[s00]>>1) & 1;
b[s00+1] = (b[s00]>>2) & 1;
b[s00  ] = (b[s00]>>3) & 1;
s00 += 2;
s10 += 2;
}
if(j < ny) {
b[s10  ] = (b[s00]>>1) & 1;
b[s00  ] = (b[s00]>>3) & 1;
}
}
if(i < nx) {
s00 = n*i;
for (j = 0; j<ny-1; j += 2) {
b[s00+1] = (b[s00]>>2) & 1;
b[s00  ] = (b[s00]>>3) & 1;
s00 += 2;
}
if(j < ny) {
b[s00  ] = (b[s00]>>3) & 1;
}
}
}
static
void
qtree_bitins(uchar *a, int nx, int ny, Pix *b, int n, int bit)
{
int i, j;
Pix *s00, *s10;
Pix px;
for(i=0; i<nx-1; i+=2) {
s00 = &b[n*i];
s10 = s00+n;
for(j=0; j<ny-1; j+=2) {
px = *a++;
s10[1] |= ( px     & 1) << bit;
s10[0] |= ((px>>1) & 1) << bit;
s00[1] |= ((px>>2) & 1) << bit;
s00[0] |= ((px>>3) & 1) << bit;
s00 += 2;
s10 += 2;
}
if(j < ny) {
px = *a++;
s10[0] |= ((px>>1) & 1) << bit;
s00[0] |= ((px>>3) & 1) << bit;
}
}
if(i < nx) {
s00 = &b[n*i];
for(j=0; j<ny-1; j+=2) {
px = *a++;
s00[1] |= ((px>>2) & 1) << bit;
s00[0] |= ((px>>3) & 1) << bit;
s00 += 2;
}
if(j < ny) {
s00[0] |= ((*a>>3) & 1) << bit;
}
}
}
static
void
read_bdirect(Biobuf *infile, Pix *a, int n, int nqx, int nqy, uchar *scratch, int bit)
{
int i;
for(i = 0; i < ((nqx+1)/2) * ((nqy+1)/2); i++) {
scratch[i] = input_nybble(infile);
}
qtree_bitins(scratch, nqx, nqy, a, n, bit);
}