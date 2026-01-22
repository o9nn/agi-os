#ifndef _MPINT
typedef struct mpint mpint;
#endif
enum
{
AESbsize=	16,
AESmaxkey=	32,
AESmaxrounds=	14
};
typedef struct AESstate AESstate;
struct AESstate
{
ulong	setup;
int	rounds;
int	keybytes;
uchar	key[AESmaxkey];
u32int	ekey[4*(AESmaxrounds + 1)];
u32int	dkey[4*(AESmaxrounds + 1)];
uchar	ivec[AESbsize];
};
void	setupAESstate(AESstate *s, uchar key[], int keybytes, uchar *ivec);
void	aesCBCencrypt(uchar *p, int len, AESstate *s);
void	aesCBCdecrypt(uchar *p, int len, AESstate *s);
enum
{
BFbsize	= 8,
BFrounds	= 16
};
typedef struct BFstate BFstate;
struct BFstate
{
ulong	setup;
uchar	key[56];
uchar	ivec[8];
u32int 	pbox[BFrounds+2];
u32int	sbox[1024];
};
void	setupBFstate(BFstate *s, uchar key[], int keybytes, uchar *ivec);
void	bfCBCencrypt(uchar*, int, BFstate*);
void	bfCBCdecrypt(uchar*, int, BFstate*);
void	bfECBencrypt(uchar*, int, BFstate*);
void	bfECBdecrypt(uchar*, int, BFstate*);
enum
{
DESbsize=	8
};
typedef struct DESstate DESstate;
struct DESstate
{
ulong	setup;
uchar	key[8];
ulong	expanded[32];
uchar	ivec[8];
};
void	setupDESstate(DESstate *s, uchar key[8], uchar *ivec);
void	des_key_setup(uchar[8], ulong[32]);
void	block_cipher(ulong*, uchar*, int);
void	desCBCencrypt(uchar*, int, DESstate*);
void	desCBCdecrypt(uchar*, int, DESstate*);
void	desECBencrypt(uchar*, int, DESstate*);
void	desECBdecrypt(uchar*, int, DESstate*);
void	des56to64(uchar *k56, uchar *k64);
void	des64to56(uchar *k64, uchar *k56);
void	key_setup(uchar[7], ulong[32]);
enum {
DES3E=		0,
DES3D=		1,
DES3EEE=	0,
DES3EDE=	2,
DES3DED=	5,
DES3DDD=	7
};
typedef struct DES3state DES3state;
struct DES3state
{
ulong	setup;
uchar	key[3][8];
ulong	expanded[3][32];
uchar	ivec[8];
};
void	setupDES3state(DES3state *s, uchar key[3][8], uchar *ivec);
void	triple_block_cipher(ulong keys[3][32], uchar*, int);
void	des3CBCencrypt(uchar*, int, DES3state*);
void	des3CBCdecrypt(uchar*, int, DES3state*);
void	des3ECBencrypt(uchar*, int, DES3state*);
void	des3ECBdecrypt(uchar*, int, DES3state*);
enum
{
SHA1dlen=	20,
MD4dlen=	16,
MD5dlen=	16
};
typedef struct DigestState DigestState;
struct DigestState
{
ulong len;
u32int state[5];
uchar buf[128];
int blen;
char malloced;
char seeded;
};
typedef struct DigestState SHAstate;
typedef struct DigestState SHA1state;
typedef struct DigestState MD5state;
typedef struct DigestState MD4state;
DigestState* md4(uchar*, ulong, uchar*, DigestState*);
DigestState* md5(uchar*, ulong, uchar*, DigestState*);
DigestState* sha1(uchar*, ulong, uchar*, DigestState*);
DigestState* hmac_md5(uchar*, ulong, uchar*, ulong, uchar*, DigestState*);
DigestState* hmac_sha1(uchar*, ulong, uchar*, ulong, uchar*, DigestState*);
char* sha1pickle(SHA1state*);
SHA1state* sha1unpickle(char*);
void	genrandom(uchar *buf, int nbytes);
void	prng(uchar *buf, int nbytes);
ulong	fastrand(void);
ulong	nfastrand(ulong);
void	genprime(mpint *p, int n, int accuracy);
void	gensafeprime(mpint *p, mpint *alpha, int n, int accuracy);
void	genstrongprime(mpint *p, int n, int accuracy);
void	DSAprimes(mpint *q, mpint *p, uchar seed[SHA1dlen]);
int	probably_prime(mpint *n, int nrep);
int	smallprimetest(mpint *p);
typedef struct RC4state RC4state;
struct RC4state
{
uchar state[256];
uchar x;
uchar y;
};
void	setupRC4state(RC4state*, uchar*, int);
void	rc4(RC4state*, uchar*, int);
void	rc4skip(RC4state*, int);
void	rc4back(RC4state*, int);
typedef struct RSApub RSApub;
typedef struct RSApriv RSApriv;
struct RSApub
{
mpint	*n;
mpint	*ek;
};
struct RSApriv
{
RSApub	pub;
mpint	*dk;
mpint	*p;
mpint	*q;
mpint	*kp;
mpint	*kq;
mpint	*c2;
};
RSApriv*	rsagen(int nlen, int elen, int rounds);
RSApriv*	rsafill(mpint *n, mpint *e, mpint *d, mpint *p, mpint *q);
mpint*		rsaencrypt(RSApub *k, mpint *in, mpint *out);
mpint*		rsadecrypt(RSApriv *k, mpint *in, mpint *out);
RSApub*		rsapuballoc(void);
void		rsapubfree(RSApub*);
RSApriv*	rsaprivalloc(void);
void		rsaprivfree(RSApriv*);
RSApub*		rsaprivtopub(RSApriv*);
RSApub*		X509toRSApub(uchar*, int, char*, int);
RSApriv*	asn1toRSApriv(uchar*, int);
void		asn1dump(uchar *der, int len);
uchar*		decodepem(char *s, char *type, int *len);
uchar*		X509gen(RSApriv *priv, char *subj, ulong valid[2], int *certlen);
uchar*		X509req(RSApriv *priv, char *subj, int *certlen);
char*		X509verify(uchar *cert, int ncert, RSApub *pk);
void		X509dump(uchar *cert, int ncert);
typedef struct EGpub EGpub;
typedef struct EGpriv EGpriv;
typedef struct EGsig EGsig;
struct EGpub
{
mpint	*p;
mpint	*alpha;
mpint	*key;
};
struct EGpriv
{
EGpub	pub;
mpint	*secret;
};
struct EGsig
{
mpint	*r, *s;
};
EGpriv*		eggen(int nlen, int rounds);
mpint*		egencrypt(EGpub *k, mpint *in, mpint *out);
mpint*		egdecrypt(EGpriv *k, mpint *in, mpint *out);
EGsig*		egsign(EGpriv *k, mpint *m);
int		egverify(EGpub *k, EGsig *sig, mpint *m);
EGpub*		egpuballoc(void);
void		egpubfree(EGpub*);
EGpriv*		egprivalloc(void);
void		egprivfree(EGpriv*);
EGsig*		egsigalloc(void);
void		egsigfree(EGsig*);
EGpub*		egprivtopub(EGpriv*);
typedef struct DSApub DSApub;
typedef struct DSApriv DSApriv;
typedef struct DSAsig DSAsig;
struct DSApub
{
mpint	*p;
mpint	*q;
mpint	*alpha;
mpint	*key;
};
struct DSApriv
{
DSApub	pub;
mpint	*secret;
};
struct DSAsig
{
mpint	*r, *s;
};
DSApriv*	dsagen(DSApub *opub);
DSAsig*		dsasign(DSApriv *k, mpint *m);
int		dsaverify(DSApub *k, DSAsig *sig, mpint *m);
DSApub*		dsapuballoc(void);
void		dsapubfree(DSApub*);
DSApriv*	dsaprivalloc(void);
void		dsaprivfree(DSApriv*);
DSAsig*		dsasigalloc(void);
void		dsasigfree(DSAsig*);
DSApub*		dsaprivtopub(DSApriv*);
typedef struct Thumbprint{
struct Thumbprint *next;
uchar sha1[SHA1dlen];
} Thumbprint;
typedef struct TLSconn{
char dir[40];
uchar *cert;
uchar *sessionID;
int certlen, sessionIDlen;
int (*trace)(char*fmt, ...);
} TLSconn;
extern int tlsClient(int fd, TLSconn *c);
extern int tlsServer(int fd, TLSconn *c);
extern Thumbprint* initThumbprints(char *ok, char *crl);
extern void freeThumbprints(Thumbprint *ok);
extern int okThumbprint(uchar *sha1, Thumbprint *ok);
extern uchar *readcert(char *filename, int *pcertlen);