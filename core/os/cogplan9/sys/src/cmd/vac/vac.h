typedef struct VacFs VacFs;
typedef struct VacDir VacDir;
typedef struct VacFile VacFile;
typedef struct VacDirEnum VacDirEnum;
#pragma incomplete VacFile
#pragma incomplete VacDirEnum
enum
{
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
ModeDevice = (1<<21),
ModeNamedPipe = (1<<22)
};
enum
{
MetaMagic = 0x5656fc79,
MetaHeaderSize = 12,
MetaIndexSize = 4,
IndexEntrySize = 8,
DirMagic = 0x1c4d9072
};
enum
{
DirPlan9Entry = 1,
DirNTEntry,
DirQidSpaceEntry,
DirGenEntry
};
struct VacDir
{
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
int qidspace;
uvlong qidoffset;
uvlong qidmax;
};
struct VacFs
{
char	name[128];
uchar	score[VtScoreSize];
VacFile	*root;
VtConn	*z;
int		mode;
int		bsize;
uvlong	qid;
VtCache	*cache;
};
VacFs	*vacfsopen(VtConn *z, char *file, int mode, int ncache);
VacFs	*vacfsopenscore(VtConn *z, u8int *score, int mode, int ncache);
VacFs	*vacfscreate(VtConn *z, int bsize, int ncache);
void		vacfsclose(VacFs *fs);
int		vacfssync(VacFs *fs);
int		vacfssnapshot(VacFs *fs, char *src, char *dst);
int		vacfsgetscore(VacFs *fs, u8int *score);
int		vacfsgetmaxqid(VacFs*, uvlong*);
void		vacfsjumpqid(VacFs*, uvlong);
VacFile *vacfsgetroot(VacFs *fs);
VacFile	*vacfileopen(VacFs *fs, char *path);
VacFile	*vacfilecreate(VacFile *file, char *elem, ulong perm);
VacFile	*vacfilewalk(VacFile *file, char *elem);
int		vacfileremove(VacFile *file);
int		vacfileread(VacFile *file, void *buf, int n, vlong offset);
int		vacfileblockscore(VacFile *file, u32int, u8int*);
int		vacfilewrite(VacFile *file, void *buf, int n, vlong offset);
uvlong	vacfilegetid(VacFile *file);
ulong	vacfilegetmcount(VacFile *file);
int		vacfileisdir(VacFile *file);
int		vacfileisroot(VacFile *file);
ulong	vacfilegetmode(VacFile *file);
int		vacfilegetsize(VacFile *file, uvlong *size);
int		vacfilegetdir(VacFile *file, VacDir *dir);
int		vacfilesetdir(VacFile *file, VacDir *dir);
VacFile	*vacfilegetparent(VacFile *file);
int		vacfileflush(VacFile*, int);
VacFile	*vacfileincref(VacFile*);
int		vacfiledecref(VacFile*);
int		vacfilesetsize(VacFile *f, uvlong size);
int		vacfilegetentries(VacFile *f, VtEntry *e, VtEntry *me);
int		vacfilesetentries(VacFile *f, VtEntry *e, VtEntry *me);
void		vdcleanup(VacDir *dir);
void		vdcopy(VacDir *dst, VacDir *src);
int		vacfilesetqidspace(VacFile*, u64int, u64int);
uvlong	vacfilegetqidoffset(VacFile*);
VacDirEnum	*vdeopen(VacFile*);
int			vderead(VacDirEnum*, VacDir *);
void			vdeclose(VacDirEnum*);
int	vdeunread(VacDirEnum*);
int	vacfiledsize(VacFile *f);
int	sha1matches(VacFile *f, ulong b, uchar *buf, int n);