typedef struct DirEntry DirEntry;
typedef struct MetaBlock MetaBlock;
typedef struct MetaEntry MetaEntry;
enum {
MetaMagic = 0x5656fc7a,
MetaHeaderSize = 12,
MetaIndexSize = 4,
IndexEntrySize = 8,
DirMagic = 0x1c4d9072,
};
enum {
ModeOtherExec = (1<<0),
ModeOtherWrite = (1<<1),
ModeOtherRead = (1<<2),
ModeGroupExec = (1<<3),
ModeGroupWrite = (1<<4),
ModeGroupRead = (1<<5),
ModeOwnerExec = (1<<6),
ModeOwnerWrite = (1<<7),
ModeOwnerRead = (1<<8),
ModeSticky = (1<<9),
ModeSetUid = (1<<10),
ModeSetGid = (1<<11),
ModeAppend = (1<<12),
ModeExclusive = (1<<13),
ModeLink = (1<<14),
ModeDir	= (1<<15),
ModeHidden = (1<<16),
ModeSystem = (1<<17),
ModeArchive = (1<<18),
ModeTemporary = (1<<19),
ModeSnapshot = (1<<20),
};
enum {
DePlan9 = 1,
DeNT,
DeQidSpace,
DeGen,
};
struct DirEntry {
char *elem;
ulong entry;
ulong gen;
ulong mentry;
ulong mgen;
uvlong size;
uvlong qid;
char *uid;
char *gid;
char *mid;
ulong mtime;
ulong mcount;
ulong ctime;
ulong atime;
ulong mode;
int plan9;
uvlong p9path;
ulong p9version;
int qidSpace;
uvlong qidOffset;
uvlong qidMax;
};
struct MetaEntry {
uchar *p;
ushort size;
};
struct MetaBlock {
int maxsize;
int size;
int free;
int maxindex;
int nindex;
int botch;
uchar *buf;
};
void	deCleanup(DirEntry*);
void	deCopy(DirEntry*, DirEntry*);
int	deSize(DirEntry*);
void	dePack(DirEntry*, MetaEntry*);
int	deUnpack(DirEntry*, MetaEntry*);
void	mbInit(MetaBlock*, uchar*, int, int);
int	mbUnpack(MetaBlock*, uchar*, int);
void	mbInsert(MetaBlock*, int, MetaEntry*);
void	mbDelete(MetaBlock*, int);
void	mbPack(MetaBlock*);
uchar	*mbAlloc(MetaBlock*, int);
int	mbResize(MetaBlock*, MetaEntry*, int);
int	mbSearch(MetaBlock*, char*, int*, MetaEntry*);
void	meUnpack(MetaEntry*, MetaBlock*, int);