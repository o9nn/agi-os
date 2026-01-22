typedef struct Arch Arch;
typedef struct BList BList;
typedef struct Block Block;
typedef struct Cache Cache;
typedef struct Disk Disk;
typedef struct Entry Entry;
typedef struct Fsck Fsck;
typedef struct Header Header;
typedef struct Label Label;
typedef struct Periodic Periodic;
typedef struct Snap Snap;
typedef struct Source Source;
typedef struct Super Super;
typedef struct WalkPtr WalkPtr;
#pragma incomplete Arch
#pragma incomplete BList
#pragma incomplete Cache
#pragma incomplete Disk
#pragma incomplete Periodic
#pragma incomplete Snap
enum {
BytesPerEntry = 100,
FullPercentage = 80,
FlushSize = 200,
DirtyPercentage = 50,
};
enum {
Nowaitlock,
Waitlock,
NilBlock = (~0UL),
MaxBlock = (1UL<<31),
};
enum {
HeaderMagic = 0x3776ae89,
HeaderVersion = 1,
HeaderOffset = 128*1024,
HeaderSize = 512,
SuperMagic = 0x2340a3b1,
SuperSize = 512,
SuperVersion = 1,
LabelSize = 14,
};
enum {
BadTag = 0,
RootTag = 1,
EnumTag,
UserTag = 32,
};
struct Super {
u16int version;
u32int epochLow;
u32int epochHigh;
u64int qid;
u32int active;
u32int next;
u32int current;
uchar last[VtScoreSize];
char name[128];
};
struct Fs {
Arch *arch;
Cache *cache;
int mode;
int noatimeupd;
int blockSize;
VtSession *z;
Snap *snap;
char *name;
Periodic *metaFlush;
VtLock *elk;
u32int ehi;
u32int elo;
int halted;
Source *source;
File *file;
};
struct Entry {
u32int gen;
ushort psize;
ushort dsize;
uchar depth;
uchar flags;
uvlong size;
uchar score[VtScoreSize];
u32int tag;
u32int snap;
uchar archive;
};
struct Source {
Fs *fs;
int mode;
int issnapshot;
u32int gen;
int dsize;
int dir;
Source *parent;
File *file;
VtLock *lk;
int ref;
u32int epoch;
Block *b;
uchar score[VtScoreSize];
u32int scoreEpoch;
int epb;
u32int tag;
u32int offset;
};
struct Header {
ushort version;
ushort blockSize;
ulong super;
ulong label;
ulong data;
ulong end;
};
struct DirEntryEnum {
File *file;
u32int boff;
int i, n;
DirEntry *buf;
};
enum {
BsFree = 0,
BsBad = 0xFF,
BsAlloc = 1<<0,
BsCopied = 1<<1,
BsVenti = 1<<2,
BsClosed = 1<<3,
BsMask = BsAlloc|BsCopied|BsVenti|BsClosed,
};
enum {
BtData,
BtDir = 1<<3,
BtLevelMask = 7,
BtMax = 1<<4,
};
enum {
BioEmpty,
BioLabel,
BioClean,
BioDirty,
BioReading,
BioWriting,
BioReadError,
BioVentiError,
BioMax
};
struct Label {
uchar type;
uchar state;
u32int tag;
u32int epoch;
u32int epochClose;
};
struct Block {
Cache *c;
int ref;
int nlock;
uintptr pc;
VtLock *lk;
int part;
u32int addr;
uchar score[VtScoreSize];
Label l;
uchar *dmap;
uchar *data;
Block *next;
Block **prev;
u32int heap;
u32int used;
u32int vers;
BList *uhead;
BList *utail;
BList *prior;
Block *ionext;
int iostate;
VtRendez *ioready;
};
struct WalkPtr
{
uchar *data;
int isEntry;
int n;
int m;
Entry e;
uchar type;
u32int tag;
};
enum
{
DoClose = 1<<0,
DoClre = 1<<1,
DoClri = 1<<2,
DoClrp = 1<<3,
};
struct Fsck
{
int printblocks;
int useventi;
int flags;
int printdirs;
int printfiles;
int walksnapshots;
int walkfs;
Fs *fs;
int (*print)(char*, ...);
void (*clre)(Fsck*, Block*, int);
void (*clrp)(Fsck*, Block*, int);
void (*close)(Fsck*, Block*, u32int);
void (*clri)(Fsck*, char*, MetaBlock*, int, Block*);
Cache *cache;
uchar *amap;
uchar *emap;
uchar *xmap;
uchar *errmap;
uchar *smap;
int nblocks;
int bsize;
int walkdepth;
u32int hint;
int nseen;
int quantum;
int nclre;
int nclrp;
int nclose;
int nclri;
};
enum {
PartError,
PartSuper,
PartLabel,
PartData,
PartVenti,
};
extern vtType[BtMax];