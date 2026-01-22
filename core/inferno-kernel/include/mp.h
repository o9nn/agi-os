#pragma src "/usr/inferno/src/libmp"
#define _MPINT 1
typedef struct mpint mpint;
struct mpint
{
int sign;
int size;
int top;
mpdigit *p;
char flags;
};
enum
{
MPstatic= 0x01,
Dbytes= sizeof(mpdigit),
Dbits= Dbytes*8
};
void mpsetminbits(int n);
mpint* mpnew(int n);
void mpfree(mpint *b);
void mpbits(mpint *b, int n);
void mpnorm(mpint *b);
mpint* mpcopy(mpint *b);
void mpassign(mpint *old, mpint *new);
mpint* mprand(int bits, void (*gen)(uchar*, int), mpint *b);
mpint* strtomp(char*, char**, int, mpint*);
int mpfmt(Fmt*);
char* mptoa(mpint*, int, char*, int);
mpint* letomp(uchar*, uint, mpint*);
int mptole(mpint*, uchar*, uint, uchar**);
mpint* betomp(uchar*, uint, mpint*);
int mptobe(mpint*, uchar*, uint, uchar**);
uint mptoui(mpint*);
mpint* uitomp(uint, mpint*);
int mptoi(mpint*);
mpint* itomp(int, mpint*);
uvlong mptouv(mpint*);
mpint* uvtomp(uvlong, mpint*);
vlong mptov(mpint*);
mpint* vtomp(vlong, mpint*);
void mpdigdiv(mpdigit *dividend, mpdigit divisor, mpdigit *quotient);
void mpadd(mpint *b1, mpint *b2, mpint *sum);
void mpsub(mpint *b1, mpint *b2, mpint *diff);
void mpleft(mpint *b, int shift, mpint *res);
void mpright(mpint *b, int shift, mpint *res);
void mpmul(mpint *b1, mpint *b2, mpint *prod);
void mpexp(mpint *b, mpint *e, mpint *m, mpint *res);
void mpmod(mpint *b, mpint *m, mpint *remainder);
void mpdiv(mpint *dividend, mpint *divisor, mpint *quotient, mpint *remainder);
int mpcmp(mpint *b1, mpint *b2);
void mpextendedgcd(mpint *a, mpint *b, mpint *d, mpint *x, mpint *y);
void mpinvert(mpint *b, mpint *m, mpint *res);
int mpsignif(mpint*);
int mplowbits0(mpint*);
extern mpint *mpzero, *mpone, *mptwo;
void mpvecadd(mpdigit *a, int alen, mpdigit *b, int blen, mpdigit *sum);
void mpvecsub(mpdigit *a, int alen, mpdigit *b, int blen, mpdigit *diff);
void mpvecdigmuladd(mpdigit *b, int n, mpdigit m, mpdigit *p);
int mpvecdigmulsub(mpdigit *b, int n, mpdigit m, mpdigit *p);
void mpvecmul(mpdigit *a, int alen, mpdigit *b, int blen, mpdigit *p);
int mpveccmp(mpdigit *a, int alen, mpdigit *b, int blen);
void mpdigdiv(mpdigit *dividend, mpdigit divisor, mpdigit *quotient);
int mpmagcmp(mpint *b1, mpint *b2);
void mpmagadd(mpint *b1, mpint *b2, mpint *sum);
void mpmagsub(mpint *b1, mpint *b2, mpint *sum);
typedef struct CRTpre CRTpre;
typedef struct CRTres CRTres;
struct CRTres
{
int n;
mpint *r[1];
};
CRTpre* crtpre(int, mpint**);
CRTres* crtin(CRTpre*, mpint*);
void crtout(CRTpre*, CRTres*, mpint*);
void crtprefree(CRTpre*);
void crtresfree(CRTres*);
#pragma varargck type "B" mpint*
#pragma varargck type "U" mpint*