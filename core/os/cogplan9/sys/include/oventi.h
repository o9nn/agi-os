#pragma lib "liboventi.a"
#pragma src "/sys/src/liboventi"
typedef struct VtSession VtSession;
typedef struct VtSha1 VtSha1;
typedef struct Packet Packet;
typedef struct VtLock VtLock;
typedef struct VtRendez VtRendez;
typedef struct VtRoot VtRoot;
typedef struct VtEntry VtEntry;
typedef struct VtServerVtbl VtServerVtbl;
#pragma incomplete VtSession
#pragma incomplete VtSha1
#pragma incomplete Packet
#pragma incomplete VtLock
#pragma incomplete VtRendez
enum {
VtScoreSize = 20,
VtMaxLumpSize = 56*1024,
VtPointerDepth = 7,
VtEntrySize = 40,
VtRootSize = 300,
VtMaxStringSize = 1000,
VtAuthSize = 1024,
MaxFragSize = 9*1024,
VtMaxFileSize = (1ULL<<48) - 1,
VtRootVersion = 2,
};
enum {
VtCryptoStrengthNone,
VtCryptoStrengthAuth,
VtCryptoStrengthWeak,
VtCryptoStrengthStrong,
};
enum {
VtCryptoNone,
VtCryptoSSL3,
VtCryptoTLS1,
VtCryptoMax
};
enum {
VtCodecNone,
VtCodecDeflate,
VtCodecThwack,
VtCodecMax
};
enum {
VtErrType,
VtRootType,
VtDirType,
VtPointerType0,
VtPointerType1,
VtPointerType2,
VtPointerType3,
VtPointerType4,
VtPointerType5,
VtPointerType6,
VtPointerType7,
VtPointerType8,
VtPointerType9,
VtDataType,
VtMaxType
};
enum {
VtEntryActive = (1<<0),
VtEntryDir = (1<<1),
VtEntryDepthShift = 2,
VtEntryDepthMask = (0x7<<2),
VtEntryLocal = (1<<5),
VtEntryNoArchive = (1<<6),
};
struct VtRoot {
ushort version;
char name[128];
char type[128];
uchar score[VtScoreSize];
ushort blockSize;
uchar prev[VtScoreSize];
};
struct VtEntry {
ulong gen;
ushort psize;
ushort dsize;
uchar depth;
uchar flags;
uvlong size;
uchar score[VtScoreSize];
};
struct VtServerVtbl {
Packet *(*read)(VtSession*, uchar score[VtScoreSize], int type, int n);
int (*write)(VtSession*, uchar score[VtScoreSize], int type, Packet *p);
void (*closing)(VtSession*, int clean);
void (*sync)(VtSession*);
};
enum {
VtVersion01 = 1,
VtVersion02,
};
extern uchar vtZeroScore[VtScoreSize];
void vtAttach(void);
void vtDetach(void);
void vtClose(VtSession *s);
void vtFree(VtSession *s);
char *vtGetUid(VtSession *s);
char *vtGetSid(VtSession *s);
int vtSetDebug(VtSession *s, int);
int vtGetDebug(VtSession *s);
int vtSetFd(VtSession *s, int fd);
int vtGetFd(VtSession *s);
int vtConnect(VtSession *s, char *password);
int vtSetCryptoStrength(VtSession *s, int);
int vtGetCryptoStrength(VtSession *s);
int vtSetCompression(VtSession *s, int);
int vtGetCompression(VtSession *s);
int vtGetCrypto(VtSession *s);
int vtGetCodec(VtSession *s);
char *vtGetVersion(VtSession *s);
char *vtGetError(void);
int vtErrFmt(Fmt *fmt);
void vtDebug(VtSession*, char *, ...);
void vtDebugMesg(VtSession *z, Packet *p, char *s);
VtSession *vtAlloc(void);
void vtReset(VtSession*);
int vtAddString(Packet*, char*);
int vtGetString(Packet*, char**);
int vtSendPacket(VtSession*, Packet*);
Packet *vtRecvPacket(VtSession*);
void vtDisconnect(VtSession*, int);
int vtHello(VtSession*);
VtSession *vtClientAlloc(void);
VtSession *vtDial(char *server, int canfail);
int vtRedial(VtSession*, char *server);
VtSession *vtStdioServer(char *server);
int vtPing(VtSession *s);
int vtSetUid(VtSession*, char *uid);
int vtRead(VtSession*, uchar score[VtScoreSize], int type, uchar *buf, int n);
int vtWrite(VtSession*, uchar score[VtScoreSize], int type, uchar *buf, int n);
Packet *vtReadPacket(VtSession*, uchar score[VtScoreSize], int type, int n);
int vtWritePacket(VtSession*, uchar score[VtScoreSize], int type, Packet *p);
int vtSync(VtSession *s);
int vtZeroExtend(int type, uchar *buf, int n, int nn);
int vtZeroTruncate(int type, uchar *buf, int n);
int vtParseScore(char*, uint, uchar[VtScoreSize]);
void vtRootPack(VtRoot*, uchar*);
int vtRootUnpack(VtRoot*, uchar*);
void vtEntryPack(VtEntry*, uchar*, int index);
int vtEntryUnpack(VtEntry*, uchar*, int index);
VtSession *vtServerAlloc(VtServerVtbl*);
int vtSetSid(VtSession *s, char *sid);
int vtExport(VtSession *s);
VtSha1* vtSha1Alloc(void);
void vtSha1Free(VtSha1*);
void vtSha1Init(VtSha1*);
void vtSha1Update(VtSha1*, uchar *, int n);
void vtSha1Final(VtSha1*, uchar sha1[VtScoreSize]);
void vtSha1(uchar score[VtScoreSize], uchar *, int);
int vtSha1Check(uchar score[VtScoreSize], uchar *, int);
int vtScoreFmt(Fmt *fmt);
Packet *packetAlloc(void);
void packetFree(Packet*);
Packet *packetForeign(uchar *buf, int n, void (*free)(void *a), void *a);
Packet *packetDup(Packet*, int offset, int n);
Packet *packetSplit(Packet*, int n);
int packetConsume(Packet*, uchar *buf, int n);
int packetTrim(Packet*, int offset, int n);
uchar *packetHeader(Packet*, int n);
uchar *packetTrailer(Packet*, int n);
int packetPrefix(Packet*, uchar *buf, int n);
int packetAppend(Packet*, uchar *buf, int n);
int packetConcat(Packet*, Packet*);
uchar *packetPeek(Packet*, uchar *buf, int offset, int n);
int packetCopy(Packet*, uchar *buf, int offset, int n);
int packetFragments(Packet*, IOchunk*, int nio, int offset);
int packetSize(Packet*);
int packetAllocatedSize(Packet*);
void packetSha1(Packet*, uchar sha1[VtScoreSize]);
int packetCompact(Packet*);
int packetCmp(Packet*, Packet*);
void packetStats(void);
void vtMemFree(void *);
void *vtMemAlloc(int);
void *vtMemAllocZ(int);
void *vtMemRealloc(void *p, int);
void *vtMemBrk(int n);
char *vtStrDup(char *);
void vtFatal(char *, ...);
char *vtGetError(void);
char *vtSetError(char *, ...);
char *vtOSError(void);
int vtThread(void (*f)(void*), void *rock);
void vtThreadSetName(char*);
VtLock *vtLockAlloc(void);
void vtLock(VtLock*);
int vtCanLock(VtLock*);
void vtRLock(VtLock*);
int vtCanRLock(VtLock*);
void vtUnlock(VtLock*);
void vtRUnlock(VtLock*);
void vtLockFree(VtLock*);
VtRendez *vtRendezAlloc(VtLock*);
void vtRendezFree(VtRendez*);
int vtSleep(VtRendez*);
int vtWakeup(VtRendez*);
int vtWakeupAll(VtRendez*);
void vtFdClose(int);
int vtFdRead(int, uchar*, int);
int vtFdReadFully(int, uchar*, int);
int vtFdWrite(int, uchar*, int);
#pragma varargck type "V" uchar*
#pragma varargck type "R" void
#pragma varargck argpos vtSetError 1