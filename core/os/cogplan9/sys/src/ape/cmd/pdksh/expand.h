#define X_EXTRA		8
#if 0
XString xs;
char *xp;
Xinit(xs, xp, 128, ATEMP);
while ((c = generate()) {
Xcheck(xs, xp);
Xput(xs, xp, c);
}
return Xclose(xs, xp);
#endif
typedef struct XString {
char   *end, *beg;
size_t	len;
Area	*areap;
} XString;
typedef char * XStringP;
#define	Xinit(xs, xp, length, area) do { \
(xs).len = length; \
(xs).areap = (area); \
(xs).beg = alloc((xs).len + X_EXTRA, (xs).areap); \
(xs).end = (xs).beg + (xs).len; \
xp = (xs).beg; \
} while (0)
#define	Xput(xs, xp, c)	(*xp++ = (c))
#define	XcheckN(xs, xp, n) do { \
int more = ((xp) + (n)) - (xs).end; \
if (more > 0) \
xp = Xcheck_grow_(&xs, xp, more); \
} while (0)
#define Xcheck(xs, xp)	XcheckN(xs, xp, 1)
#define	Xfree(xs, xp)	afree((void*) (xs).beg, (xs).areap)
#define	Xclose(xs, xp)	(char*) aresize((void*)(xs).beg, \
(size_t)((xp) - (xs).beg), (xs).areap)
#define	Xstring(xs, xp)	((xs).beg)
#define Xnleft(xs, xp) ((xs).end - (xp))
#define	Xlength(xs, xp) ((xp) - (xs).beg)
#define Xsize(xs, xp) ((xs).end - (xs).beg)
#define	Xsavepos(xs, xp) ((xp) - (xs).beg)
#define	Xrestpos(xs, xp, n) ((xs).beg + (n))
char *	Xcheck_grow_	ARGS((XString *xsp, char *xp, int more));
typedef struct XPtrV {
void  **cur;
void  **beg, **end;
} XPtrV;
#define	XPinit(x, n) do { \
register void **vp__; \
vp__ = (void**) alloc(sizeofN(void*, n), ATEMP); \
(x).cur = (x).beg = vp__; \
(x).end = vp__ + n; \
} while (0)
#define	XPput(x, p) do { \
if ((x).cur >= (x).end) { \
int n = XPsize(x); \
(x).beg = (void**) aresize((void*) (x).beg, \
sizeofN(void*, n*2), ATEMP); \
(x).cur = (x).beg + n; \
(x).end = (x).cur + n; \
} \
*(x).cur++ = (p); \
} while (0)
#define	XPptrv(x)	((x).beg)
#define	XPsize(x)	((x).cur - (x).beg)
#define	XPclose(x)	(void**) aresize((void*)(x).beg, \
sizeofN(void*, XPsize(x)), ATEMP)
#define	XPfree(x)	afree((void*) (x).beg, ATEMP)