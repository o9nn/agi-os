#include <u.h>
#include <libc.h>
#include <flate.h>
typedef struct Chain	Chain;
typedef struct Chains	Chains;
typedef struct Dyncode	Dyncode;
typedef struct Huff	Huff;
typedef struct LZblock	LZblock;
typedef struct LZstate	LZstate;
enum
{
DeflateUnc	= 0,
DeflateFix	= 1,
DeflateDyn	= 2,
DeflateEob	= 256,
DeflateMaxBlock	= 64*1024-1,
DeflateMaxExp	= 10,
LenStart	= 257,
Nlitlen		= 288,
Noff		= 30,
Nclen		= 19,
MaxOff		= 32*1024,
MinMatch	= 3,
MaxMatch	= 258,
MaxLeaf		= Nlitlen,
MaxHuffBits	= 16,
ChainMem	= 2 * (MaxHuffBits - 1) * MaxHuffBits,
LenFlag		= 1 << 3,
LenShift	= 4,
MaxLitRun	= LenFlag - 1,
DeflateOut	= 4096,
BlockSize	= 8192,
DeflateBlock	= DeflateMaxBlock & ~(BlockSize - 1),
MinMatchMaxOff	= 4096,
HistSlop	= 512,
HistBlock	= 64*1024,
HistSize	= HistBlock + HistSlop,
HashLog		= 13,
HashSize	= 1<<HashLog,
MaxOffCode	= 256,
EstLitBits	= 8,
EstLenBits	= 4,
EstOffBits	= 5,
};
#define hashit(c)	((((ulong)(c) & 0xffffff) * 0x6b43a9b5) >> (32 - HashLog))
struct LZstate
{
uchar	hist[HistSize];
ulong	pos;
ulong	avail;
int	eof;
ushort	hash[HashSize];
ushort	nexts[MaxOff];
int	now;
int	dot;
int	prevlen;
int	prevoff;
int	maxcheck;
uchar	obuf[DeflateOut];
uchar	*out;
uchar	*eout;
ulong	bits;
int	nbits;
int	rbad;
int	wbad;
int	(*w)(void*, void*, int);
void	*wr;
ulong	totr;
ulong	totw;
int	debug;
};
struct LZblock
{
ushort	parse[DeflateMaxBlock / 2 + 1];
int	lastv;
ulong	litlencount[Nlitlen];
ulong	offcount[Noff];
ushort	*eparse;
int	bytes;
int	excost;
};
struct Huff
{
short	bits;
ushort	encode;
};
struct Dyncode
{
int	nlit;
int	noff;
int	nclen;
int	ncode;
Huff	codetab[Nclen];
uchar	codes[Nlitlen+Noff];
uchar	codeaux[Nlitlen+Noff];
};
static	int	deflateb(LZstate *lz, LZblock *lzb, void *rr, int (*r)(void*, void*, int));
static	int	lzcomp(LZstate*, LZblock*, uchar*, ushort*, int finish);
static	void	wrblock(LZstate*, int, ushort*, ushort*, Huff*, Huff*);
static	int	bitcost(Huff*, ulong*, int);
static	int	huffcodes(Dyncode*, Huff*, Huff*);
static	void	wrdyncode(LZstate*, Dyncode*);
static	void	lzput(LZstate*, ulong bits, int nbits);
static	void	lzflushbits(LZstate*);
static	void	lzflush(LZstate *lz);
static	void	lzwrite(LZstate *lz, void *buf, int n);
static	int	hufftabinit(Huff*, int, ulong*, int);
static	int	mkgzprecode(Huff*, ulong *, int, int);
static	int	mkprecode(Huff*, ulong *, int, int, ulong*);
static	void	nextchain(Chains*, int);
static	void	leafsort(ulong*, ushort*, int, int);
static int lencode[MaxMatch];
static int offcode[MaxOffCode];
static int bigoffcode[256];
static int litlenbase[Nlitlen-LenStart];
static int litlenextra[Nlitlen-LenStart] =
{
0, 0, 0,
0, 0, 0, 0, 0, 1, 1, 1, 1, 2,
2, 2, 2, 3, 3, 3, 3, 4, 4, 4,
4, 5, 5, 5, 5, 0, 0, 0
};
static int offbase[Noff];
static int offextra[] =
{
0,  0,  0,  0,  1,  1,  2,  2,  3,  3,
4,  4,  5,  5,  6,  6,  7,  7,  8,  8,
9,  9,  10, 10, 11, 11, 12, 12, 13, 13,
0,  0,
};
static int clenorder[Nclen] =
{
16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
};
static	Huff	litlentab[Nlitlen];
static	Huff	offtab[Noff];
static	Huff	hofftab[Noff];
static	uchar	revtab[256];
static	ulong	nlits;
static	ulong	nmatches;
int
deflateinit(void)
{
ulong bitcount[MaxHuffBits];
int i, j, ci, n;
for(i=0; i<256; i++)
for(j=0; j<8; j++)
if(i & (1<<j))
revtab[i] |= 0x80 >> j;
for(i=0; i<144; i++)
litlentab[i].bits = 8;
for(i=144; i<256; i++)
litlentab[i].bits = 9;
for(i=256; i<280; i++)
litlentab[i].bits = 7;
for(i=280; i<Nlitlen; i++)
litlentab[i].bits = 8;
memset(bitcount, 0, sizeof(bitcount));
bitcount[8] += 144 - 0;
bitcount[9] += 256 - 144;
bitcount[7] += 280 - 256;
bitcount[8] += Nlitlen - 280;
if(!hufftabinit(litlentab, Nlitlen, bitcount, 9))
return FlateInternal;
for(i = 0; i < Noff; i++)
offtab[i].bits = 5;
memset(bitcount, 0, sizeof(bitcount));
bitcount[5] = Noff;
if(!hufftabinit(offtab, Noff, bitcount, 5))
return FlateInternal;
bitcount[0] = 0;
bitcount[1] = 0;
if(!mkgzprecode(hofftab, bitcount, 2, MaxHuffBits))
return FlateInternal;
ci = 0;
for(i = LenStart; i < 286; i++){
n = ci + (1 << litlenextra[i - LenStart]);
litlenbase[i - LenStart] = ci;
for(; ci < n; ci++)
lencode[ci] = i;
}
lencode[MaxMatch-MinMatch] = 285;
litlenbase[285-LenStart] = MaxMatch-MinMatch;
ci = 0;
for(i = 0; i < 16; i++){
n = ci + (1 << offextra[i]);
offbase[i] = ci;
for(; ci < n; ci++)
offcode[ci] = i;
}
ci = ci >> 7;
for(; i < 30; i++){
n = ci + (1 << (offextra[i] - 7));
offbase[i] = ci << 7;
for(; ci < n; ci++)
bigoffcode[ci] = i;
}
return FlateOk;
}
static void
deflatereset(LZstate *lz, int level, int debug)
{
memset(lz->nexts, 0, sizeof lz->nexts);
memset(lz->hash, 0, sizeof lz->hash);
lz->totr = 0;
lz->totw = 0;
lz->pos = 0;
lz->avail = 0;
lz->out = lz->obuf;
lz->eout = &lz->obuf[DeflateOut];
lz->prevlen = MinMatch - 1;
lz->prevoff = 0;
lz->now = MaxOff + 1;
lz->dot = lz->now;
lz->bits = 0;
lz->nbits = 0;
lz->maxcheck = (1 << level);
lz->maxcheck -= lz->maxcheck >> 2;
if(lz->maxcheck < 2)
lz->maxcheck = 2;
else if(lz->maxcheck > 1024)
lz->maxcheck = 1024;
lz->debug = debug;
}
int
deflate(void *wr, int (*w)(void*, void*, int), void *rr, int (*r)(void*, void*, int), int level, int debug)
{
LZstate *lz;
LZblock *lzb;
int ok;
lz = malloc(sizeof *lz + sizeof *lzb);
if(lz == nil)
return FlateNoMem;
lzb = (LZblock*)&lz[1];
deflatereset(lz, level, debug);
lz->w = w;
lz->wr = wr;
lz->wbad = 0;
lz->rbad = 0;
lz->eof = 0;
ok = FlateOk;
while(!lz->eof || lz->avail){
ok = deflateb(lz, lzb, rr, r);
if(ok != FlateOk)
break;
}
if(ok == FlateOk && lz->rbad)
ok = FlateInputFail;
if(ok == FlateOk && lz->wbad)
ok = FlateOutputFail;
free(lz);
return ok;
}
static int
deflateb(LZstate *lz, LZblock *lzb, void *rr, int (*r)(void*, void*, int))
{
Dyncode dyncode, hdyncode;
Huff dlitlentab[Nlitlen], dofftab[Noff], hlitlentab[Nlitlen];
ulong litcount[Nlitlen];
long nunc, ndyn, nfix, nhuff;
uchar *slop, *hslop;
ulong ep;
int i, n, m, mm, nslop;
memset(lzb->litlencount, 0, sizeof lzb->litlencount);
memset(lzb->offcount, 0, sizeof lzb->offcount);
lzb->litlencount[DeflateEob]++;
lzb->bytes = 0;
lzb->eparse = lzb->parse;
lzb->lastv = 0;
lzb->excost = 0;
slop = &lz->hist[lz->pos];
n = lz->avail;
while(n < DeflateBlock && (!lz->eof || lz->avail)){
if(!lz->eof){
ep = lz->pos + lz->avail;
if(ep >= HistBlock)
ep -= HistBlock;
m = HistBlock - MaxOff - lz->avail;
if(m > HistBlock - n)
m = HistBlock - n;
if(m > (HistBlock + HistSlop) - ep)
m = (HistBlock + HistSlop) - ep;
if(m & ~(BlockSize - 1))
m &= ~(BlockSize - 1);
if(m < HistSlop){
if(!m || !lzb->bytes)
return FlateInternal;
break;
}
mm = (*r)(rr, &lz->hist[ep], m);
if(mm > 0){
if(ep < HistSlop)
memmove(&lz->hist[ep + HistBlock], &lz->hist[ep], HistSlop - ep);
else if(ep + mm > HistBlock)
memmove(&lz->hist[0], &lz->hist[HistBlock], ep + mm - HistBlock);
lz->totr += mm;
n += mm;
lz->avail += mm;
}else{
if(mm < 0)
lz->rbad = 1;
lz->eof = 1;
}
}
ep = lz->pos + lz->avail;
if(ep > HistSize)
ep = HistSize;
if(lzb->bytes + ep - lz->pos > DeflateMaxBlock)
ep = lz->pos + DeflateMaxBlock - lzb->bytes;
m = lzcomp(lz, lzb, &lz->hist[ep], lzb->eparse, lz->eof);
lzb->bytes += m;
lz->pos = (lz->pos + m) & (HistBlock - 1);
lz->avail -= m;
}
if(lzb->lastv)
*lzb->eparse++ = lzb->lastv;
if(lzb->eparse > lzb->parse + nelem(lzb->parse))
return FlateInternal;
nunc = lzb->bytes;
if(!mkgzprecode(dlitlentab, lzb->litlencount, Nlitlen, MaxHuffBits)
|| !mkgzprecode(dofftab, lzb->offcount, Noff, MaxHuffBits))
return FlateInternal;
ndyn = huffcodes(&dyncode, dlitlentab, dofftab);
if(ndyn < 0)
return FlateInternal;
ndyn += bitcost(dlitlentab, lzb->litlencount, Nlitlen)
+ bitcost(dofftab, lzb->offcount, Noff)
+ lzb->excost;
memset(litcount, 0, sizeof litcount);
nslop = nunc;
if(nslop > &lz->hist[HistSize] - slop)
nslop = &lz->hist[HistSize] - slop;
for(i = 0; i < nslop; i++)
litcount[slop[i]]++;
hslop = &lz->hist[HistSlop - nslop];
for(; i < nunc; i++)
litcount[hslop[i]]++;
litcount[DeflateEob]++;
if(!mkgzprecode(hlitlentab, litcount, Nlitlen, MaxHuffBits))
return FlateInternal;
nhuff = huffcodes(&hdyncode, hlitlentab, hofftab);
if(nhuff < 0)
return FlateInternal;
nhuff += bitcost(hlitlentab, litcount, Nlitlen);
nfix = bitcost(litlentab, lzb->litlencount, Nlitlen)
+ bitcost(offtab, lzb->offcount, Noff)
+ lzb->excost;
lzput(lz, lz->eof && !lz->avail, 1);
if(lz->debug){
fprint(2, "block: bytes=%lud entries=%ld extra bits=%d\n\tuncompressed=%lud fixed=%lud dynamic=%lud huffman=%lud\n",
nunc, lzb->eparse - lzb->parse, lzb->excost, (nunc + 4) * 8, nfix, ndyn, nhuff);
fprint(2, "\tnlit=%lud matches=%lud eof=%d\n", nlits, nmatches, lz->eof && !lz->avail);
}
if((nunc + 4) * 8 < ndyn && (nunc + 4) * 8 < nfix && (nunc + 4) * 8 < nhuff){
lzput(lz, DeflateUnc, 2);
lzflushbits(lz);
lzput(lz, nunc & 0xff, 8);
lzput(lz, (nunc >> 8) & 0xff, 8);
lzput(lz, ~nunc & 0xff, 8);
lzput(lz, (~nunc >> 8) & 0xff, 8);
lzflush(lz);
lzwrite(lz, slop, nslop);
lzwrite(lz, &lz->hist[HistSlop], nunc - nslop);
}else if(ndyn < nfix && ndyn < nhuff){
lzput(lz, DeflateDyn, 2);
wrdyncode(lz, &dyncode);
wrblock(lz, slop - lz->hist, lzb->parse, lzb->eparse, dlitlentab, dofftab);
lzput(lz, dlitlentab[DeflateEob].encode, dlitlentab[DeflateEob].bits);
}else if(nhuff < nfix){
lzput(lz, DeflateDyn, 2);
wrdyncode(lz, &hdyncode);
m = 0;
for(i = nunc; i > MaxLitRun; i -= MaxLitRun)
lzb->parse[m++] = MaxLitRun;
lzb->parse[m++] = i;
wrblock(lz, slop - lz->hist, lzb->parse, lzb->parse + m, hlitlentab, hofftab);
lzput(lz, hlitlentab[DeflateEob].encode, hlitlentab[DeflateEob].bits);
}else{
lzput(lz, DeflateFix, 2);
wrblock(lz, slop - lz->hist, lzb->parse, lzb->eparse, litlentab, offtab);
lzput(lz, litlentab[DeflateEob].encode, litlentab[DeflateEob].bits);
}
if(lz->eof && !lz->avail){
lzflushbits(lz);
lzflush(lz);
}
return FlateOk;
}
static void
lzwrite(LZstate *lz, void *buf, int n)
{
int nw;
if(n && lz->w){
nw = (*lz->w)(lz->wr, buf, n);
if(nw != n){
lz->w = nil;
lz->wbad = 1;
}else
lz->totw += n;
}
}
static void
lzflush(LZstate *lz)
{
lzwrite(lz, lz->obuf, lz->out - lz->obuf);
lz->out = lz->obuf;
}
static void
lzput(LZstate *lz, ulong bits, int nbits)
{
bits = (bits << lz->nbits) | lz->bits;
for(nbits += lz->nbits; nbits >= 8; nbits -= 8){
*lz->out++ = bits;
if(lz->out == lz->eout)
lzflush(lz);
bits >>= 8;
}
lz->bits = bits;
lz->nbits = nbits;
}
static void
lzflushbits(LZstate *lz)
{
if(lz->nbits)
lzput(lz, 0, 8 - (lz->nbits & 7));
}
static void
wrblock(LZstate *out, int litoff, ushort *soff, ushort *eoff, Huff *litlentab, Huff *offtab)
{
ushort *off;
int i, run, offset, lit, len, c;
if(out->debug > 2){
for(off = soff; off < eoff; ){
offset = *off++;
run = offset & MaxLitRun;
if(run){
for(i = 0; i < run; i++){
lit = out->hist[litoff & (HistBlock - 1)];
litoff++;
fprint(2, "\tlit %.2ux %c\n", lit, lit);
}
if(!(offset & LenFlag))
continue;
len = offset >> LenShift;
offset = *off++;
}else if(offset & LenFlag){
len = offset >> LenShift;
offset = *off++;
}else{
len = 0;
offset >>= LenShift;
}
litoff += len + MinMatch;
fprint(2, "\t<%d, %d>\n", offset + 1, len + MinMatch);
}
}
for(off = soff; off < eoff; ){
offset = *off++;
run = offset & MaxLitRun;
if(run){
for(i = 0; i < run; i++){
lit = out->hist[litoff & (HistBlock - 1)];
litoff++;
lzput(out, litlentab[lit].encode, litlentab[lit].bits);
}
if(!(offset & LenFlag))
continue;
len = offset >> LenShift;
offset = *off++;
}else if(offset & LenFlag){
len = offset >> LenShift;
offset = *off++;
}else{
len = 0;
offset >>= LenShift;
}
litoff += len + MinMatch;
c = lencode[len];
lzput(out, litlentab[c].encode, litlentab[c].bits);
c -= LenStart;
if(litlenextra[c])
lzput(out, len - litlenbase[c], litlenextra[c]);
if(offset < MaxOffCode)
c = offcode[offset];
else
c = bigoffcode[offset >> 7];
lzput(out, offtab[c].encode, offtab[c].bits);
if(offextra[c])
lzput(out, offset - offbase[c], offextra[c]);
}
}
static int
lzmatch(int now, int then, uchar *p, uchar *es, ushort *nexts, uchar *hist, int runlen, int check, int *m)
{
uchar *s, *t;
int ml, off, last;
ml = check;
if(runlen >= 8)
check >>= 2;
*m = 0;
if(p + runlen >= es)
return runlen;
last = 0;
for(; check-- > 0; then = nexts[then & (MaxOff-1)]){
off = (ushort)(now - then);
if(off <= last || off > MaxOff)
break;
s = p + runlen;
t = hist + (((p - hist) - off) & (HistBlock-1));
t += runlen;
for(; s >= p; s--){
if(*s != *t)
goto matchloop;
t--;
}
t += runlen + 2;
s += runlen + 2;
for(; s < es; s++){
if(*s != *t)
break;
t++;
}
runlen = s - p;
*m = off - 1;
if(s == es || runlen > ml)
break;
matchloop:;
last = off;
}
return runlen;
}
static int
lzcomp(LZstate *lz, LZblock *lzb, uchar *ep, ushort *parse, int finish)
{
ulong cont, excost, *litlencount, *offcount;
uchar *p, *q, *s, *es;
ushort *nexts, *hash;
int v, i, h, runlen, n, now, then, m, prevlen, prevoff, maxdefer;
litlencount = lzb->litlencount;
offcount = lzb->offcount;
nexts = lz->nexts;
hash = lz->hash;
now = lz->now;
p = &lz->hist[lz->pos];
if(lz->prevlen != MinMatch - 1)
p++;
n = MinMatch;
if(n > ep - p)
n = ep - p;
cont = 0;
for(i = 0; i < n - 1; i++){
m = now - ((MinMatch-1) - i);
if(m < lz->dot)
continue;
s = lz->hist + (((p - lz->hist) - (now - m)) & (HistBlock-1));
cont = (s[0] << 16) | (s[1] << 8) | s[2];
h = hashit(cont);
prevoff = 0;
for(then = hash[h]; ; then = nexts[then & (MaxOff-1)]){
v = (ushort)(now - then);
if(v <= prevoff || v >= (MinMatch-1) - i)
break;
prevoff = v;
}
if(then == (ushort)m)
continue;
nexts[m & (MaxOff-1)] = hash[h];
hash[h] = m;
}
for(i = 0; i < n; i++)
cont = (cont << 8) | p[i];
prevlen = lz->prevlen;
prevoff = lz->prevoff;
maxdefer = lz->maxcheck >> 2;
excost = 0;
v = lzb->lastv;
for(;;){
es = p + MaxMatch;
if(es > ep){
if(!finish || p >= ep)
break;
es = ep;
}
h = hashit(cont);
runlen = lzmatch(now, hash[h], p, es, nexts, lz->hist, prevlen, lz->maxcheck, &m);
if(runlen == MinMatch && m >= MinMatchMaxOff){
runlen = MinMatch - 1;
m = 0;
}
if(prevlen >= runlen && prevlen != MinMatch - 1){
n = prevlen - MinMatch;
if(v || n){
*parse++ = v | LenFlag | (n << LenShift);
*parse++ = prevoff;
}else
*parse++ = prevoff << LenShift;
v = 0;
n = lencode[n];
litlencount[n]++;
excost += litlenextra[n - LenStart];
if(prevoff < MaxOffCode)
n = offcode[prevoff];
else
n = bigoffcode[prevoff >> 7];
offcount[n]++;
excost += offextra[n];
runlen = prevlen - 1;
prevlen = MinMatch - 1;
nmatches++;
}else if(runlen == MinMatch - 1){
if(++v == MaxLitRun){
*parse++ = v;
v = 0;
}
litlencount[*p]++;
nlits++;
runlen = 1;
}else{
if(prevlen != MinMatch - 1){
if(++v == MaxLitRun){
*parse++ = v;
v = 0;
}
litlencount[p[-1]]++;
nlits++;
}
prevoff = m;
if(runlen < maxdefer){
prevlen = runlen;
runlen = 1;
}else{
n = runlen - MinMatch;
if(v || n){
*parse++ = v | LenFlag | (n << LenShift);
*parse++ = prevoff;
}else
*parse++ = prevoff << LenShift;
v = 0;
n = lencode[n];
litlencount[n]++;
excost += litlenextra[n - LenStart];
if(prevoff < MaxOffCode)
n = offcode[prevoff];
else
n = bigoffcode[prevoff >> 7];
offcount[n]++;
excost += offextra[n];
prevlen = MinMatch - 1;
nmatches++;
}
}
for(q = p + runlen; p != q; p++){
if(p + MinMatch <= ep){
h = hashit(cont);
nexts[now & (MaxOff-1)] = hash[h];
hash[h] = now;
if(p + MinMatch < ep)
cont = (cont << 8) | p[MinMatch];
}
now++;
}
}
if(prevlen != MinMatch - 1)
p--;
lzb->excost += excost;
lzb->eparse = parse;
lzb->lastv = v;
lz->now = now;
lz->prevlen = prevlen;
lz->prevoff = prevoff;
return p - &lz->hist[lz->pos];
}
static int
huffcodes(Dyncode *dc, Huff *littab, Huff *offtab)
{
Huff *codetab;
uchar *codes, *codeaux;
ulong codecount[Nclen], excost;
int i, n, m, v, c, nlit, noff, ncode, nclen;
codetab = dc->codetab;
codes = dc->codes;
codeaux = dc->codeaux;
for(nlit = Nlitlen; nlit > 257 && littab[nlit-1].bits == 0; nlit--)
;
for(noff = Noff; noff > 1 && offtab[noff-1].bits == 0; noff--)
;
for(i = 0; i < nlit; i++)
codes[i] = littab[i].bits;
for(i = 0; i < noff; i++)
codes[i + nlit] = offtab[i].bits;
excost = 0;
c = 0;
ncode = nlit+noff;
for(i = 0; i < ncode; ){
n = i + 1;
v = codes[i];
while(n < ncode && v == codes[n])
n++;
n -= i;
i += n;
if(v == 0){
while(n >= 11){
m = n;
if(m > 138)
m = 138;
codes[c] = 18;
codeaux[c++] = m - 11;
n -= m;
excost += 7;
}
if(n >= 3){
codes[c] = 17;
codeaux[c++] = n - 3;
n = 0;
excost += 3;
}
}
while(n--){
codes[c++] = v;
while(n >= 3){
m = n;
if(m > 6)
m = 6;
codes[c] = 16;
codeaux[c++] = m - 3;
n -= m;
excost += 3;
}
}
}
memset(codecount, 0, sizeof codecount);
for(i = 0; i < c; i++)
codecount[codes[i]]++;
if(!mkgzprecode(codetab, codecount, Nclen, 8))
return -1;
for(nclen = Nclen; nclen > 4 && codetab[clenorder[nclen-1]].bits == 0; nclen--)
;
dc->nlit = nlit;
dc->noff = noff;
dc->nclen = nclen;
dc->ncode = c;
return 5 + 5 + 4 + nclen * 3 + bitcost(codetab, codecount, Nclen) + excost;
}
static void
wrdyncode(LZstate *out, Dyncode *dc)
{
Huff *codetab;
uchar *codes, *codeaux;
int i, v, c;
lzput(out, dc->nlit-257, 5);
lzput(out, dc->noff-1, 5);
lzput(out, dc->nclen-4, 4);
codetab = dc->codetab;
for(i = 0; i < dc->nclen; i++)
lzput(out, codetab[clenorder[i]].bits, 3);
codes = dc->codes;
codeaux = dc->codeaux;
c = dc->ncode;
for(i = 0; i < c; i++){
v = codes[i];
lzput(out, codetab[v].encode, codetab[v].bits);
if(v >= 16){
if(v == 16)
lzput(out, codeaux[i], 2);
else if(v == 17)
lzput(out, codeaux[i], 3);
else
lzput(out, codeaux[i], 7);
}
}
}
static int
bitcost(Huff *tab, ulong *count, int n)
{
ulong tot;
int i;
tot = 0;
for(i = 0; i < n; i++)
tot += count[i] * tab[i].bits;
return tot;
}
static int
mkgzprecode(Huff *tab, ulong *count, int n, int maxbits)
{
ulong bitcount[MaxHuffBits];
int i, nbits;
nbits = mkprecode(tab, count, n, maxbits, bitcount);
for(i = 0; i < n; i++){
if(tab[i].bits == -1)
tab[i].bits = 0;
else if(tab[i].bits == 0){
if(nbits != 0 || bitcount[0] != 1)
return 0;
bitcount[1] = 1;
bitcount[0] = 0;
nbits = 1;
tab[i].bits = 1;
}
}
if(bitcount[0] != 0)
return 0;
return hufftabinit(tab, n, bitcount, nbits);
}
static int
hufftabinit(Huff *tab, int n, ulong *bitcount, int nbits)
{
ulong code, nc[MaxHuffBits];
int i, bits;
code = 0;
for(bits = 1; bits <= nbits; bits++){
code = (code + bitcount[bits-1]) << 1;
nc[bits] = code;
}
for(i = 0; i < n; i++){
bits = tab[i].bits;
if(bits){
code = nc[bits]++ << (16 - bits);
if(code & ~0xffff)
return 0;
tab[i].encode = revtab[code >> 8] | (revtab[code & 0xff] << 8);
}
}
return 1;
}
struct Chain
{
ulong	count;
ushort	leaf;
char	col;
char	gen;
Chain	*up;
};
struct Chains
{
Chain	*lists[(MaxHuffBits - 1) * 2];
ulong	leafcount[MaxLeaf];
ushort	leafmap[MaxLeaf];
int	nleaf;
Chain	chains[ChainMem];
Chain	*echains;
Chain	*free;
char	col;
int	nlists;
};
static int
mkprecode(Huff *tab, ulong *count, int n, int maxbits, ulong *bitcount)
{
Chains cs;
Chain *c;
int i, m, em, bits;
m = 0;
for(i = 0; i < n; i++){
tab[i].bits = -1;
tab[i].encode = 0;
if(count[i] != 0){
cs.leafcount[m] = count[i];
cs.leafmap[m] = i;
m++;
}
}
if(m < 2){
if(m != 0){
tab[cs.leafmap[0]].bits = 0;
bitcount[0] = 1;
}else
bitcount[0] = 0;
return 0;
}
cs.nleaf = m;
leafsort(cs.leafcount, cs.leafmap, 0, m);
for(i = 0; i < m; i++)
cs.leafcount[i] = count[cs.leafmap[i]];
cs.free = &cs.chains[2];
cs.echains = &cs.chains[ChainMem];
cs.col = 1;
c = &cs.chains[0];
c->count = cs.leafcount[0];
c->leaf = 1;
c->col = cs.col;
c->up = nil;
c->gen = 0;
cs.chains[1] = cs.chains[0];
cs.chains[1].leaf = 2;
cs.chains[1].count = cs.leafcount[1];
for(i = 0; i < maxbits-1; i++){
cs.lists[i * 2] = &cs.chains[0];
cs.lists[i * 2 + 1] = &cs.chains[1];
}
cs.nlists = 2 * (maxbits - 1);
m = 2 * m - 2;
for(i = 2; i < m; i++)
nextchain(&cs, cs.nlists - 2);
bits = 0;
bitcount[0] = cs.nleaf;
for(c = cs.lists[cs.nlists - 1]; c != nil; c = c->up){
m = c->leaf;
bitcount[bits++] -= m;
bitcount[bits] = m;
}
m = 0;
for(i = bits; i >= 0; i--)
for(em = m + bitcount[i]; m < em; m++)
tab[cs.leafmap[m]].bits = i;
return bits;
}
static void
nextchain(Chains *cs, int list)
{
Chain *c, *oc;
int i, nleaf, sumc;
oc = cs->lists[list + 1];
cs->lists[list] = oc;
if(oc == nil)
return;
if(oc->gen){
nextchain(cs, list - 2);
nextchain(cs, list - 2);
oc->gen = 0;
}
for(c = cs->free; ; c++){
if(c >= cs->echains){
cs->col++;
for(i = 0; i < cs->nlists; i++)
for(c = cs->lists[i]; c != nil; c = c->up)
c->col = cs->col;
c = cs->chains;
}
if(c->col != cs->col)
break;
}
nleaf = oc->leaf;
sumc = 0;
if(list > 0 && cs->lists[list-1] != nil)
sumc = cs->lists[list-2]->count + cs->lists[list-1]->count;
if(sumc != 0 && (nleaf >= cs->nleaf || cs->leafcount[nleaf] > sumc)){
c->count = sumc;
c->leaf = oc->leaf;
c->up = cs->lists[list-1];
c->gen = 1;
}else if(nleaf >= cs->nleaf){
cs->lists[list + 1] = nil;
return;
}else{
c->leaf = nleaf + 1;
c->count = cs->leafcount[nleaf];
c->up = oc->up;
c->gen = 0;
}
cs->free = c + 1;
cs->lists[list + 1] = c;
c->col = cs->col;
}
static int
pivot(ulong *c, int a, int n)
{
int j, pi, pj, pk;
j = n/6;
pi = a + j;
j += j;
pj = pi + j;
pk = pj + j;
if(c[pi] < c[pj]){
if(c[pi] < c[pk]){
if(c[pj] < c[pk])
return pj;
return pk;
}
return pi;
}
if(c[pj] < c[pk]){
if(c[pi] < c[pk])
return pi;
return pk;
}
return pj;
}
static	void
leafsort(ulong *leafcount, ushort *leafmap, int a, int n)
{
ulong t;
int j, pi, pj, pn;
while(n > 1){
if(n > 10){
pi = pivot(leafcount, a, n);
}else
pi = a + (n>>1);
t = leafcount[pi];
leafcount[pi] = leafcount[a];
leafcount[a] = t;
t = leafmap[pi];
leafmap[pi] = leafmap[a];
leafmap[a] = t;
pi = a;
pn = a + n;
pj = pn;
for(;;){
do
pi++;
while(pi < pn && (leafcount[pi] < leafcount[a] || leafcount[pi] == leafcount[a] && leafmap[pi] > leafmap[a]));
do
pj--;
while(pj > a && (leafcount[pj] > leafcount[a] || leafcount[pj] == leafcount[a] && leafmap[pj] < leafmap[a]));
if(pj < pi)
break;
t = leafcount[pi];
leafcount[pi] = leafcount[pj];
leafcount[pj] = t;
t = leafmap[pi];
leafmap[pi] = leafmap[pj];
leafmap[pj] = t;
}
t = leafcount[a];
leafcount[a] = leafcount[pj];
leafcount[pj] = t;
t = leafmap[a];
leafmap[a] = leafmap[pj];
leafmap[pj] = t;
j = pj - a;
n = n-j-1;
if(j >= n){
leafsort(leafcount, leafmap, a, j);
a += j+1;
}else{
leafsort(leafcount, leafmap, a + (j+1), n);
n = j;
}
}
}