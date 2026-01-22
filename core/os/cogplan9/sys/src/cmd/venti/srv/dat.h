typedef struct Config Config;
typedef struct AMap AMap;
typedef struct AMapN AMapN;
typedef struct Arena Arena;
typedef struct AState AState;
typedef struct ArenaCIG ArenaCIG;
typedef struct ArenaHead ArenaHead;
typedef struct ArenaPart ArenaPart;
typedef struct ArenaTail ArenaTail;
typedef struct ATailStats ATailStats;
typedef struct CIBlock CIBlock;
typedef struct Clump Clump;
typedef struct ClumpInfo ClumpInfo;
typedef struct Graph Graph;
typedef struct IAddr IAddr;
typedef struct IBucket IBucket;
typedef struct IEStream IEStream;
typedef struct IEntry IEntry;
typedef struct IFile IFile;
typedef struct ISect ISect;
typedef struct Index Index;
typedef struct Lump Lump;
typedef struct DBlock DBlock;
typedef struct Part Part;
typedef struct Statbin Statbin;
typedef struct Statdesc Statdesc;
typedef struct Stats Stats;
typedef struct ZBlock ZBlock;
typedef struct Round Round;
typedef struct Bloom Bloom;
#pragma incomplete IEStream
#define TWID32 ((u32int)~(u32int)0)
#define TWID64 ((u64int)~(u64int)0)
#define TWID8 ((u8int)~(u8int)0)
enum
{
ABlockLog = 9,
ANameSize = 64,
MaxDiskBlock = 64*1024,
MaxIoSize = 64*1024,
PartBlank = 256*1024,
HeadSize = 512,
MinArenaSize = 1*1024*1024,
IndexBase = 1024*1024,
MaxIo = 64*1024,
ICacheBits = 16,
MaxAMap = 31*1024,
Unspecified = TWID32,
SyncDataErr = 1 << 0,
SyncCIErr = 1 << 1,
SyncCIZero = 1 << 2,
SyncFixErr = 1 << 3,
SyncHeader = 1 << 4,
EOk = 0,
EStrange,
ECorrupt,
EICorrupt,
EAdmin,
ECrash,
EBug,
EInconsist,
EMax,
_ClumpMagic = 0xd15cb10cU,
ClumpFreeMagic = 0,
ArenaPartMagic = 0xa9e4a5e7U,
ArenaMagic = 0xf2a14eadU,
ArenaHeadMagic = 0xd15c4eadU,
BloomMagic = 0xb1004eadU,
BloomMaxHash = 32,
ISectMagic = 0xd15c5ec7U,
ArenaPartVersion = 3,
ArenaVersion4 = 4,
ArenaVersion5 = 5,
BloomVersion = 1,
IndexVersion = 1,
ISectVersion1 = 1,
ISectVersion2 = 2,
ClumpEErr = 0,
ClumpENone,
ClumpECompress,
ClumpEMax,
U8Size = 1,
U16Size = 2,
U32Size = 4,
U64Size = 8,
ArenaPartSize = 4 * U32Size,
ArenaSize4 = 2 * U64Size + 6 * U32Size + ANameSize + U8Size,
ArenaSize5 = ArenaSize4 + U32Size,
ArenaSize5a = ArenaSize5 + 2 * U8Size + 2 * U32Size + 2 * U64Size,
ArenaHeadSize4 = U64Size + 3 * U32Size + ANameSize,
ArenaHeadSize5 = ArenaHeadSize4 + U32Size,
BloomHeadSize = 4 * U32Size,
ISectSize1 = 7 * U32Size + 2 * ANameSize,
ISectSize2 = ISectSize1 + U32Size,
ClumpInfoSize = U8Size + 2 * U16Size + VtScoreSize,
ClumpSize = ClumpInfoSize + U8Size + 3 * U32Size,
MaxBloomSize = 1<<(32-3),
MaxBloomHash = 32,
IBucketSize = U32Size + U16Size,
IEntrySize = U64Size + U32Size + 2*U16Size + 2*U8Size + VtScoreSize,
IEntryTypeOff = VtScoreSize + U32Size + U16Size + U64Size + U16Size,
IEntryAddrOff = VtScoreSize + U32Size + U16Size,
MaxClumpBlocks = (VtMaxLumpSize + ClumpSize + (1 << ABlockLog) - 1) >> ABlockLog,
IcacheFrac = 1000000,
SleepForever = 1000000000,
DirtyArena = 1,
DirtyArenaCib,
DirtyArenaTrailer,
DirtyMax,
ArenaCIGSize = 10*1024,
VentiZZZZZZZZ
};
extern char TraceDisk[];
extern char TraceLump[];
extern char TraceBlock[];
extern char TraceProc[];
extern char TraceWork[];
extern char TraceQuiet[];
extern char TraceRpc[];
struct Config
{
char *index;
int naparts;
ArenaPart **aparts;
int nsects;
ISect **sects;
Bloom *bloom;
u32int bcmem;
u32int mem;
u32int icmem;
int queuewrites;
char* haddr;
char* vaddr;
char* webroot;
};
struct Part
{
int fd;
int mode;
u64int offset;
u64int size;
u32int blocksize;
u32int fsblocksize;
char *name;
char *filename;
Channel *writechan;
};
struct DBlock
{
u8int *data;
Part *part;
u64int addr;
u32int size;
u32int mode;
u32int dirty;
u32int dirtying;
DBlock *next;
DBlock *prev;
u32int heap;
u32int used;
u32int used2;
u32int ref;
RWLock lock;
Channel *writedonechan;
void* chanbuf[1];
};
struct Lump
{
Packet *data;
Part *part;
u8int score[VtScoreSize];
u8int type;
u32int size;
Lump *next;
Lump *prev;
u32int heap;
u32int used;
u32int used2;
u32int ref;
QLock lock;
};
struct AMap
{
u64int start;
u64int stop;
char name[ANameSize];
};
struct AMapN
{
int n;
AMap *map;
};
struct ArenaPart
{
Part *part;
u64int size;
Arena **arenas;
u32int tabbase;
u32int tabsize;
u32int version;
u32int blocksize;
u32int arenabase;
AMap *map;
int narenas;
};
struct CIBlock
{
u32int block;
int offset;
DBlock *data;
};
struct ATailStats
{
u32int clumps;
u32int cclumps;
u64int used;
u64int uncsize;
u8int sealed;
};
struct AState
{
Arena *arena;
u64int aa;
ATailStats stats;
};
struct Arena
{
QLock lock;
Part *part;
int blocksize;
u64int base;
u64int size;
u8int score[VtScoreSize];
int clumpmax;
AState mem;
int inqueue;
u32int version;
char name[ANameSize];
ATailStats memstats;
ATailStats diskstats;
u32int ctime;
u32int wtime;
u32int clumpmagic;
ArenaCIG *cig;
int ncig;
};
struct ArenaCIG
{
u64int offset;
};
struct ArenaHead
{
u32int version;
char name[ANameSize];
u32int blocksize;
u64int size;
u32int clumpmagic;
};
struct ClumpInfo
{
u8int type;
u16int size;
u16int uncsize;
u8int score[VtScoreSize];
};
struct Clump
{
ClumpInfo info;
u8int encoding;
u32int creator;
u32int time;
};
struct Index
{
u32int div;
u32int buckets;
u32int blocksize;
u32int tabsize;
int mapalloc;
Arena **arenas;
ISect **sects;
Bloom *bloom;
u32int version;
char name[ANameSize];
int nsects;
AMap *smap;
int narenas;
AMap *amap;
QLock writing;
};
struct ISect
{
Part *part;
int blocklog;
int buckmax;
u32int tabbase;
u32int tabsize;
Channel *writechan;
Channel *writedonechan;
void *ig;
int ng;
u32int version;
u32int bucketmagic;
char name[ANameSize];
char index[ANameSize];
u32int blocksize;
u32int blockbase;
u32int blocks;
u32int start;
u32int stop;
};
struct IAddr
{
u64int addr;
u16int size;
u8int type;
u8int blocks;
};
struct IEntry
{
u8int score[VtScoreSize];
IAddr ia;
IEntry *nexthash;
IEntry *nextdirty;
IEntry *next;
IEntry *prev;
u8int state;
};
enum {
IEClean = 0,
IEDirty = 1,
IESummary = 2,
};
struct IBucket
{
u16int n;
u32int buck;
u8int *data;
};
struct ZBlock
{
u32int len;
u32int _size;
u8int *data;
u8int *free;
};
struct IFile
{
char *name;
ZBlock *b;
u32int pos;
};
struct Statdesc
{
char *name;
ulong max;
};
enum
{
StatRpcTotal,
StatRpcRead,
StatRpcReadOk,
StatRpcReadFail,
StatRpcReadBytes,
StatRpcReadTime,
StatRpcReadCached,
StatRpcReadCachedTime,
StatRpcReadUncached,
StatRpcReadUncachedTime,
StatRpcWrite,
StatRpcWriteNew,
StatRpcWriteOld,
StatRpcWriteFail,
StatRpcWriteBytes,
StatRpcWriteTime,
StatRpcWriteNewTime,
StatRpcWriteOldTime,
StatLcacheHit,
StatLcacheMiss,
StatLcacheRead,
StatLcacheWrite,
StatLcacheSize,
StatLcacheStall,
StatLcacheReadTime,
StatDcacheHit,
StatDcacheMiss,
StatDcacheLookup,
StatDcacheRead,
StatDcacheWrite,
StatDcacheDirty,
StatDcacheSize,
StatDcacheFlush,
StatDcacheStall,
StatDcacheLookupTime,
StatDblockStall,
StatLumpStall,
StatIcacheHit,
StatIcacheMiss,
StatIcacheRead,
StatIcacheWrite,
StatIcacheFill,
StatIcachePrefetch,
StatIcacheDirty,
StatIcacheSize,
StatIcacheFlush,
StatIcacheStall,
StatIcacheReadTime,
StatIcacheLookup,
StatScacheHit,
StatScachePrefetch,
StatBloomHit,
StatBloomMiss,
StatBloomFalseMiss,
StatBloomLookup,
StatBloomOnes,
StatBloomBits,
StatApartRead,
StatApartReadBytes,
StatApartWrite,
StatApartWriteBytes,
StatIsectRead,
StatIsectReadBytes,
StatIsectWrite,
StatIsectWriteBytes,
StatSumRead,
StatSumReadBytes,
StatCigLoad,
StatCigLoadTime,
NStat
};
extern Statdesc statdesc[NStat];
struct Stats
{
ulong now;
ulong n[NStat];
};
struct Statbin
{
uint nsamp;
uint min;
uint max;
uint avg;
};
struct Graph
{
long (*fn)(Stats*, Stats*, void*);
void *arg;
long t0;
long t1;
long min;
long max;
long wid;
long ht;
int fill;
};
struct Round
{
QLock lock;
Rendez start;
Rendez finish;
Rendez delaywait;
int delaytime;
int delaykick;
char* name;
int last;
int current;
int next;
int doanother;
};
struct Bloom
{
RWLock lk;
QLock mod;
int nhash;
ulong size;
ulong bitmask;
u8int *data;
Part *part;
Channel *writechan;
Channel *writedonechan;
};
extern Index *mainindex;
extern u32int maxblocksize;
extern int paranoid;
extern int queuewrites;
extern int readonly;
extern Stats stats;
extern u8int zeroscore[VtScoreSize];
extern int compressblocks;
extern int writestodevnull;
extern int collectstats;
extern QLock memdrawlock;
extern int icachesleeptime;
extern int minicachesleeptime;
extern int arenasumsleeptime;
extern int manualscheduling;
extern int l0quantum;
extern int l1quantum;
extern int ignorebloom;
extern int icacheprefetch;
extern int syncwrites;
extern int debugarena;
extern Stats *stathist;
extern int nstathist;
extern ulong stattime;
#ifndef PLAN9PORT
#pragma varargck type "V" uchar*
#define ODIRECT 0
#endif