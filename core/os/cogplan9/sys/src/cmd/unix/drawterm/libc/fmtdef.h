typedef int (*Fmts)(Fmt*);
typedef struct Quoteinfo Quoteinfo;
struct Quoteinfo
{
int quoted;
int nrunesin;
int nbytesin;
int nrunesout;
int nbytesout;
};
double __Inf(int sign);
double __NaN(void);
int __badfmt(Fmt *f);
int __charfmt(Fmt *f);
int __countfmt(Fmt *f);
int __efgfmt(Fmt *fmt);
int __errfmt(Fmt *f);
int __flagfmt(Fmt *f);
int __fmtFdFlush(Fmt *f);
int __fmtcpy(Fmt *f, const void *vm, int n, int sz);
void* __fmtdispatch(Fmt *f, void *fmt, int isrunes);
void * __fmtflush(Fmt *f, void *t, int len);
void __fmtlock(void);
int __fmtpad(Fmt *f, int n);
double __fmtpow10(int n);
int __fmtrcpy(Fmt *f, const void *vm, int n);
void __fmtunlock(void);
int __ifmt(Fmt *f);
int __isInf(double d, int sign);
int __isNaN(double d);
int __needsquotes(char *s, int *quotelenp);
int __percentfmt(Fmt *f);
void __quotesetup(char *s, Rune *r, int nin, int nout, Quoteinfo *q, int sharp, int runesout);
int __quotestrfmt(int runesin, Fmt *f);
int __rfmtpad(Fmt *f, int n);
int __runefmt(Fmt *f);
int __runeneedsquotes(Rune *r, int *quotelenp);
int __runesfmt(Fmt *f);
int __strfmt(Fmt *f);
#define FMTCHAR(f, t, s, c)\
do{\
if(t + 1 > (char*)s){\
t = __fmtflush(f, t, 1);\
if(t != nil)\
s = f->stop;\
else\
return -1;\
}\
*t++ = c;\
}while(0)
#define FMTRCHAR(f, t, s, c)\
do{\
if(t + 1 > (Rune*)s){\
t = __fmtflush(f, t, sizeof(Rune));\
if(t != nil)\
s = f->stop;\
else\
return -1;\
}\
*t++ = c;\
}while(0)
#define FMTRUNE(f, t, s, r)\
do{\
Rune _rune;\
int _runelen;\
if(t + UTFmax > (char*)s && t + (_runelen = runelen(r)) > (char*)s){\
t = __fmtflush(f, t, _runelen);\
if(t != nil)\
s = f->stop;\
else\
return -1;\
}\
if(r < Runeself)\
*t++ = r;\
else{\
_rune = r;\
t += runetochar(t, &_rune);\
}\
}while(0)
#ifdef va_copy
# define VA_COPY(a,b) va_copy(a,b)
# define VA_END(a) va_end(a)
#else
# define VA_COPY(a,b) (a) = (b)
# define VA_END(a)
#endif
#define PLAN9PORT