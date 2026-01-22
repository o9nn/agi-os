#pragma lib "libventi.a"
#pragma src "/sys/src/libventi"
enum
{
MaxFragSize = 9*1024
};
typedef struct Packet Packet;
#pragma incomplete Packet
Packet* packetalloc(void);
void packetappend(Packet*, uchar *buf, int n);
uint packetasize(Packet*);
int packetcmp(Packet*, Packet*);
int packetcompact(Packet*);
void packetconcat(Packet*, Packet*);
int packetconsume(Packet*, uchar *buf, int n);
int packetcopy(Packet*, uchar *buf, int offset, int n);
Packet* packetdup(Packet*, int offset, int n);
Packet* packetforeign(uchar *buf, int n, void (*free)(void *a), void *a);
int packetfragments(Packet*, IOchunk*, int nio, int offset);
void packetfree(Packet*);
uchar* packetheader(Packet*, int n);
uchar* packetpeek(Packet*, uchar *buf, int offset, int n);
void packetprefix(Packet*, uchar *buf, int n);
void packetsha1(Packet*, uchar sha1[20]);
uint packetsize(Packet*);
Packet* packetsplit(Packet*, int n);
void packetstats(void);
uchar* packettrailer(Packet*, int n);
int packettrim(Packet*, int offset, int n);
typedef struct VtLog VtLog;
typedef struct VtLogChunk VtLogChunk;
struct VtLog
{
VtLog *next;
char *name;
VtLogChunk *chunk;
uint nchunk;
VtLogChunk *w;
QLock lk;
int ref;
};
struct VtLogChunk
{
char *p;
char *ep;
char *wp;
};
VtLog* vtlogopen(char *name, uint size);
void vtlogprint(VtLog *log, char *fmt, ...);
void vtlog(char *name, char *fmt, ...);
void vtlogclose(VtLog*);
void vtlogremove(char *name);
char** vtlognames(int*);
void vtlogdump(int fd, VtLog*);
typedef struct VtFcall VtFcall;
typedef struct VtConn VtConn;
typedef struct VtEntry VtEntry;
typedef struct VtRoot VtRoot;
enum
{
VtScoreSize = 20,
VtMaxStringSize = 1024,
VtMaxLumpSize = 56*1024,
VtPointerDepth = 7
};
#define VtMaxFileSize ((1ULL<<48)-1)
int vtputstring(Packet*, char*);
int vtgetstring(Packet*, char**);
enum
{
VtDataType = 0<<3,
VtDirType = 1<<3,
VtRootType = 2<<3,
VtMaxType,
VtCorruptType = 0xFF,
VtTypeDepthMask = 7,
VtTypeBaseMask = ~VtTypeDepthMask
};
uint vttodisktype(uint);
uint vtfromdisktype(uint);
enum
{
VtEntryActive = 1<<0,
_VtEntryDir = 1<<1,
_VtEntryDepthShift = 2,
_VtEntryDepthMask = 7<<2,
VtEntryLocal = 1<<5
};
enum
{
VtEntrySize = 40
};
struct VtEntry
{
ulong gen;
ushort psize;
ushort dsize;
uchar type;
uchar flags;
uvlong size;
uchar score[VtScoreSize];
};
void vtentrypack(VtEntry*, uchar*, int index);
int vtentryunpack(VtEntry*, uchar*, int index);
struct VtRoot
{
char name[128];
char type[128];
uchar score[VtScoreSize];
ushort blocksize;
uchar prev[VtScoreSize];
};
enum
{
VtRootSize = 300,
VtRootVersion = 2
};
void vtrootpack(VtRoot*, uchar*);
int vtrootunpack(VtRoot*, uchar*);
extern uchar vtzeroscore[VtScoreSize];
void vtzeroextend(int type, uchar *buf, uint n, uint nn);
uint vtzerotruncate(int type, uchar *buf, uint n);
int vtparsescore(char *s, char **prefix, uchar[VtScoreSize]);
#pragma varargck type "V" uchar*
#pragma varargck type "F" VtFcall*
#pragma varargck type "T" void
#pragma varargck type "lT" void
int vtscorefmt(Fmt*);
void vtfree(void *);
void* vtmalloc(int);
void* vtmallocz(int);
void* vtrealloc(void *p, int);
void* vtbrk(int n);
char* vtstrdup(char *);
enum
{
VtCryptoStrengthNone,
VtCryptoStrengthAuth,
VtCryptoStrengthWeak,
VtCryptoStrengthStrong
};
enum
{
VtCryptoNone,
VtCryptoSSL3,
VtCryptoTLS1,
VtCryptoMax
};
enum
{
VtCodecNone,
VtCodecDeflate,
VtCodecThwack,
VtCodecMax
};
enum
{
VtRerror = 1,
VtTping = 2,
VtRping,
VtThello = 4,
VtRhello,
VtTgoodbye = 6,
VtRgoodbye,
VtTauth0 = 8,
VtRauth0,
VtTauth1 = 10,
VtRauth1,
VtTread = 12,
VtRread,
VtTwrite = 14,
VtRwrite,
VtTsync = 16,
VtRsync,
VtTmax
};
struct VtFcall
{
uchar msgtype;
uchar tag;
char *error;
char *version;
char *uid;
uchar strength;
uchar *crypto;
uint ncrypto;
uchar *codec;
uint ncodec;
char *sid;
uchar rcrypto;
uchar rcodec;
uchar *auth;
uint nauth;
uchar score[VtScoreSize];
uchar blocktype;
ushort count;
Packet *data;
};
Packet* vtfcallpack(VtFcall*);
int vtfcallunpack(VtFcall*, Packet*);
void vtfcallclear(VtFcall*);
int vtfcallfmt(Fmt*);
enum
{
VtStateAlloc,
VtStateConnected,
VtStateClosed
};
struct VtConn
{
QLock lk;
QLock inlk;
QLock outlk;
int debug;
int infd;
int outfd;
int muxer;
void *writeq;
void *readq;
int state;
void *wait[256];
uint ntag;
uint nsleep;
Packet *part;
Rendez tagrend;
Rendez rpcfork;
char *version;
char *uid;
char *sid;
char addr[256];
};
VtConn* vtconn(int infd, int outfd);
VtConn* vtdial(char*);
void vtfreeconn(VtConn*);
int vtsend(VtConn*, Packet*);
Packet* vtrecv(VtConn*);
int vtversion(VtConn* z);
void vtdebug(VtConn* z, char*, ...);
void vthangup(VtConn* z);
int vtgoodbye(VtConn* z);
typedef struct VtSrv VtSrv;
#pragma incomplete VtSrv
typedef struct VtReq VtReq;
struct VtReq
{
VtFcall tx;
VtFcall rx;
VtSrv *srv;
void *sc;
};
int vtsrvhello(VtConn*);
VtSrv* vtlisten(char *addr);
VtReq* vtgetreq(VtSrv*);
void vtrespond(VtReq*);
Packet* vtrpc(VtConn*, Packet*);
Packet* _vtrpc(VtConn*, Packet*, VtFcall*);
void vtrecvproc(void*);
void vtsendproc(void*);
int vtconnect(VtConn*);
int vthello(VtConn*);
int vtread(VtConn*, uchar score[VtScoreSize], uint type, uchar *buf, int n);
int vtwrite(VtConn*, uchar score[VtScoreSize], uint type, uchar *buf, int n);
Packet* vtreadpacket(VtConn*, uchar score[VtScoreSize], uint type, int n);
int vtwritepacket(VtConn*, uchar score[VtScoreSize], uint type, Packet *p);
int vtsync(VtConn*);
int vtping(VtConn*);
enum
{
NilBlock = ~0
};
typedef struct VtBlock VtBlock;
typedef struct VtCache VtCache;
#pragma incomplete VtCache
struct VtBlock
{
VtCache *c;
QLock lk;
uchar *data;
uchar score[VtScoreSize];
uchar type;
int nlock;
int iostate;
int ref;
u32int heap;
VtBlock *next;
VtBlock **prev;
u32int used;
u32int used2;
u32int addr;
uintptr pc;
};
u32int vtglobaltolocal(uchar[VtScoreSize]);
void vtlocaltoglobal(u32int, uchar[VtScoreSize]);
VtCache*vtcachealloc(VtConn*, int blocksize, ulong nblocks);
void vtcachefree(VtCache*);
VtBlock*vtcachelocal(VtCache*, u32int addr, int type);
VtBlock*vtcacheglobal(VtCache*, uchar[VtScoreSize], int type);
VtBlock*vtcacheallocblock(VtCache*, int type);
void vtcachesetwrite(VtCache*,
int(*)(VtConn*, uchar[VtScoreSize], uint, uchar*, int));
void vtblockput(VtBlock*);
u32int vtcacheblocksize(VtCache*);
int vtblockwrite(VtBlock*);
VtBlock*vtblockcopy(VtBlock*);
void vtblockduplock(VtBlock*);
extern int vtcachencopy, vtcachenread, vtcachenwrite;
extern int vttracelevel;
typedef struct VtFile VtFile;
struct VtFile
{
QLock lk;
int ref;
int local;
VtBlock *b;
uchar score[VtScoreSize];
VtCache *c;
int mode;
u32int gen;
int dsize;
int psize;
int dir;
VtFile *parent;
int epb;
u32int offset;
};
enum
{
VtOREAD,
VtOWRITE,
VtORDWR
};
VtBlock*vtfileblock(VtFile*, u32int, int mode);
int vtfileblockscore(VtFile*, u32int, uchar[VtScoreSize]);
void vtfileclose(VtFile*);
VtFile* _vtfilecreate(VtFile*, int offset, int psize, int dsize, int dir);
VtFile* vtfilecreate(VtFile*, int psize, int dsize, int dir);
VtFile* vtfilecreateroot(VtCache*, int psize, int dsize, int type);
int vtfileflush(VtFile*);
int vtfileflushbefore(VtFile*, u64int);
u32int vtfilegetdirsize(VtFile*);
int vtfilegetentry(VtFile*, VtEntry*);
uvlong vtfilegetsize(VtFile*);
void vtfileincref(VtFile*);
int vtfilelock2(VtFile*, VtFile*, int);
int vtfilelock(VtFile*, int);
VtFile* vtfileopen(VtFile*, u32int, int);
VtFile* vtfileopenroot(VtCache*, VtEntry*);
long vtfileread(VtFile*, void*, long, vlong);
int vtfileremove(VtFile*);
int vtfilesetdirsize(VtFile*, u32int);
int vtfilesetentry(VtFile*, VtEntry*);
int vtfilesetsize(VtFile*, u64int);
int vtfiletruncate(VtFile*);
void vtfileunlock(VtFile*);
long vtfilewrite(VtFile*, void*, long, vlong);
int vttimefmt(Fmt*);
extern int chattyventi;
extern int ventidoublechecksha1;
extern int ventilogging;
extern char *VtServerLog;