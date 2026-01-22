#include "sh.h"
#define	INIT_TBLS	8
static void     texpand     ARGS((struct table *tp, int nsize));
static int      tnamecmp    ARGS((void *p1, void *p2));
unsigned int
hash(n)
register const char * n;
{
register unsigned int h = 0;
while (*n != '\0')
h = 2*h + *n++;
return h * 32821;
}
void
tinit(tp, ap, tsize)
register struct table *tp;
register Area *ap;
int tsize;
{
tp->areap = ap;
tp->tbls = NULL;
tp->size = tp->nfree = 0;
if (tsize)
texpand(tp, tsize);
}
static void
texpand(tp, nsize)
register struct table *tp;
int nsize;
{
register int i;
register struct tbl *tblp, **p;
register struct tbl **ntblp, **otblp = tp->tbls;
int osize = tp->size;
ntblp = (struct tbl**) alloc(sizeofN(struct tbl *, nsize), tp->areap);
for (i = 0; i < nsize; i++)
ntblp[i] = NULL;
tp->size = nsize;
tp->nfree = 8*nsize/10;
tp->tbls = ntblp;
if (otblp == NULL)
return;
for (i = 0; i < osize; i++)
if ((tblp = otblp[i]) != NULL)
if ((tblp->flag&DEFINED)) {
for (p = &ntblp[hash(tblp->name)
& (tp->size-1)];
*p != NULL; p--)
if (p == ntblp)
p += tp->size;
*p = tblp;
tp->nfree--;
} else if (!(tblp->flag & FINUSE)) {
afree((void*)tblp, tp->areap);
}
afree((void*)otblp, tp->areap);
}
struct tbl *
tsearch(tp, n, h)
register struct table *tp;
register const char *n;
unsigned int h;
{
register struct tbl **pp, *p;
if (tp->size == 0)
return NULL;
for (pp = &tp->tbls[h & (tp->size-1)]; (p = *pp) != NULL; pp--) {
if (*p->name == *n && strcmp(p->name, n) == 0
&& (p->flag&DEFINED))
return p;
if (pp == tp->tbls)
pp += tp->size;
}
return NULL;
}
struct tbl *
tenter(tp, n, h)
register struct table *tp;
register const char *n;
unsigned int h;
{
register struct tbl **pp, *p;
register int len;
if (tp->size == 0)
texpand(tp, INIT_TBLS);
Search:
for (pp = &tp->tbls[h & (tp->size-1)]; (p = *pp) != NULL; pp--) {
if (*p->name == *n && strcmp(p->name, n) == 0)
return p;
if (pp == tp->tbls)
pp += tp->size;
}
if (tp->nfree <= 0) {
texpand(tp, 2*tp->size);
goto Search;
}
len = strlen(n) + 1;
p = (struct tbl *) alloc(offsetof(struct tbl, name[0]) + len,
tp->areap);
p->flag = 0;
p->type = 0;
p->areap = tp->areap;
p->u2.field = 0;
p->u.array = (struct tbl *)0;
memcpy(p->name, n, len);
tp->nfree--;
*pp = p;
return p;
}
void
tdelete(p)
register struct tbl *p;
{
p->flag = 0;
}
void
twalk(ts, tp)
struct tstate *ts;
struct table *tp;
{
ts->left = tp->size;
ts->next = tp->tbls;
}
struct tbl *
tnext(ts)
struct tstate *ts;
{
while (--ts->left >= 0) {
struct tbl *p = *ts->next++;
if (p != NULL && (p->flag&DEFINED))
return p;
}
return NULL;
}
static int
tnamecmp(p1, p2)
void *p1, *p2;
{
return strcmp(((struct tbl *)p1)->name, ((struct tbl *)p2)->name);
}
struct tbl **
tsort(tp)
register struct table *tp;
{
register int i;
register struct tbl **p, **sp, **dp;
p = (struct tbl **)alloc(sizeofN(struct tbl *, tp->size+1), ATEMP);
sp = tp->tbls;
dp = p;
for (i = 0; i < tp->size; i++)
if ((*dp = *sp++) != NULL && (((*dp)->flag&DEFINED) ||
((*dp)->flag&ARRAY)))
dp++;
i = dp - p;
qsortp((void**)p, (size_t)i, tnamecmp);
p[i] = NULL;
return p;
}
#ifdef PERF_DEBUG
void tprintinfo ARGS((struct table *tp));
void
tprintinfo(tp)
struct table *tp;
{
struct tbl *te;
char *n;
unsigned int h;
int ncmp;
int totncmp = 0, maxncmp = 0;
int nentries = 0;
struct tstate ts;
shellf("table size %d, nfree %d\n", tp->size, tp->nfree);
shellf("    Ncmp name\n");
twalk(&ts, tp);
while ((te = tnext(&ts))) {
register struct tbl **pp, *p;
h = hash(n = te->name);
ncmp = 0;
for (pp = &tp->tbls[h & (tp->size-1)]; (p = *pp); pp--) {
ncmp++;
if (*p->name == *n && strcmp(p->name, n) == 0
&& (p->flag&DEFINED))
break;
if (pp == tp->tbls)
pp += tp->size;
}
shellf("    %4d %s\n", ncmp, n);
totncmp += ncmp;
nentries++;
if (ncmp > maxncmp)
maxncmp = ncmp;
}
if (nentries)
shellf("  %d entries, worst ncmp %d, avg ncmp %d.%02d\n",
nentries, maxncmp,
totncmp / nentries,
(totncmp % nentries) * 100 / nentries);
}
#endif