#define NAMELEN 28
#define NDBLOCK 6
#define MAXDAT 8192
#define NTLOCK 200
typedef struct Fbuf Fbuf;
typedef struct Super1 Super1;
typedef struct Superb Superb;
typedef struct Dentry Dentry;
typedef struct Tag Tag;
typedef struct Device Device;
typedef struct Qid9p1 Qid9p1;
typedef struct File File;
typedef struct Filsys Filsys;
typedef struct Filta Filta;
typedef struct Filter Filter;
typedef ulong Float;
typedef struct Hiob Hiob;
typedef struct Iobuf Iobuf;
typedef struct P9call P9call;
typedef struct Tlock Tlock;
typedef struct Uid Uid;
typedef struct Wpath Wpath;
typedef struct AuthRpc AuthRpc;
struct Qid9p1
{
long path;
long version;
};
struct Dentry
{
char name[NAMELEN];
short uid;
short gid;
ushort mode;
#define DALLOC 0x8000
#define DDIR 0x4000
#define DAPND 0x2000
#define DLOCK 0x1000
#define DREAD 0x4
#define DWRITE 0x2
#define DEXEC 0x1
Qid9p1 qid;
long size;
long dblock[NDBLOCK];
long iblock;
long diblock;
long atime;
long mtime;
};
struct Tag
{
short pad;
short tag;
long path;
};
struct Super1
{
long fstart;
long fsize;
long tfree;
long qidgen;
long fsok;
long roraddr;
long last;
long next;
};
struct Fbuf
{
long nfree;
long free[1];
};
struct Superb
{
Super1;
Fbuf fbuf;
};
struct Device
{
char type;
char ctrl;
char unit;
char part;
};
struct Filter
{
ulong count;
ulong oldcount;
Float filter[3];
};
struct Filta
{
Filter* f;
int scale;
};
struct Tlock
{
Device dev;
long time;
long qpath;
File* file;
};
struct File
{
QLock;
Qid qid;
Wpath* wpath;
Chan* cp;
Tlock* tlock;
File* next;
File* list;
Filsys* fs;
long addr;
long slot;
long lastra;
short fid;
short uid;
char open;
#define FREAD 1
#define FWRITE 2
#define FREMOV 4
#define FWSTAT 8
long doffset;
ulong dvers;
long dslot;
AuthRpc *rpc;
short cuid;
};
struct Filsys
{
char* name;
Device dev;
int flags;
#define FREAM (1<<1)
#define FRECOVER (1<<2)
};
struct Hiob
{
Iobuf* link;
Lock;
};
struct Iobuf
{
QLock;
Device dev;
Iobuf *next;
Iobuf *fore;
Iobuf *back;
char *iobuf;
char *xiobuf;
long addr;
int flags;
};
struct P9call
{
uchar calln;
uchar rxflag;
short msize;
void (*func)(Chan*, int);
};
struct Uid
{
short uid;
short lead;
short offset;
};
struct Wpath
{
Wpath *up;
Wpath *list;
long addr;
long slot;
short refs;
};
#define MAXFDATA 8192
enum
{
Ebadspc = 1,
Efid,
Efidinuse,
Echar,
Eopen,
Ecount,
Ealloc,
Eqid,
Eauth,
Eauthmsg,
Eaccess,
Eentry,
Emode,
Edir1,
Edir2,
Ephase,
Eexist,
Edot,
Eempty,
Ebadu,
Enotu,
Enotg,
Ename,
Ewalk,
Eronly,
Efull,
Eoffset,
Elocked,
Ebroken,
Etoolong,
Ersc,
Eqidmode,
Econvert,
Enotm,
Enotd,
Enotl,
Enotw,
Esystem,
MAXERR
};
enum
{
Cwio1 = 1,
Cwio2,
Cwxx1,
Cwxx2,
Cwxx3,
Cwxx4,
Cwdump1,
Cwdump2,
Cuidbuf,
};
enum
{
Tnone = 0,
Tsuper,
Tdir,
Tind1,
Tind2,
Tfile,
Tfree,
Tbuck,
Tvirgo,
Tcache,
MAXTAG
};
enum
{
Bread = (1<<0),
Bprobe = (1<<1),
Bmod = (1<<2),
Bimm = (1<<3),
Bres = (1<<4),
};
enum
{
MREAD = 0,
MWRITE,
MBOTH,
MEXEC,
MTRUNC = (1<<4),
MCEXEC = (1<<5),
MRCLOSE = (1<<6),
};
enum
{
Crdall = (1<<0),
Ctag = (1<<1),
Cpfile = (1<<2),
Cpdir = (1<<3),
Cfree = (1<<4),
Cream = (1<<6),
Cbad = (1<<7),
Ctouch = (1<<8),
Cquiet = (1<<9),
};
extern int RBUFSIZE;
extern int BUFSIZE;
extern int DIRPERBUF;
extern int INDPERBUF;
extern int INDPERBUF2;
extern int FEPERBUF;