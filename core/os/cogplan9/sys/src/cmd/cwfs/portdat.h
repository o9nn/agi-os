enum {
SUPER_ADDR = 2,
ROOT_ADDR = 3,
};
typedef vlong Wideoff;
typedef short Userid;
typedef long Timet;
typedef vlong Devsize;
#define NEXT(x, l) (((x)+1) % (l))
#define PREV(x, l) ((x) == 0? (l)-1: (x)-1)
#define HOWMANY(x, y) (((x)+((y)-1)) / (y))
#define ROUNDUP(x, y) (HOWMANY((x), (y)) * (y))
#define TK2MS(t) (((ulong)(t)*1000)/HZ)
#define MS2TK(t) (((ulong)(t)*HZ)/1000)
#define TK2SEC(t) ((t)/HZ)
enum {
MAXDAT = 8192,
MAXMSG = 128,
MB = 1024*1024,
HZ = 1,
};
enum {
Maxword = 256,
NTLOCK = 200,
};
typedef struct Auth Auth;
typedef struct Bp Bp;
typedef struct Bucket Bucket;
typedef struct Cache Cache;
typedef struct Centry Centry;
typedef struct Chan Chan;
typedef struct Command Command;
typedef struct Conf Conf;
typedef struct Cons Cons;
typedef struct Dentry Dentry;
typedef struct Device Device;
typedef struct Fbuf Fbuf;
typedef struct File File;
typedef struct Filsys Filsys;
typedef struct Filter Filter;
typedef struct Flag Flag;
typedef struct Hiob Hiob;
typedef struct Iobuf Iobuf;
typedef struct Lock Lock;
typedef struct Msgbuf Msgbuf;
typedef struct QLock QLock;
typedef struct Qid9p1 Qid9p1;
typedef struct Queue Queue;
typedef union Rabuf Rabuf;
typedef struct Rendez Rendez;
typedef struct Rtc Rtc;
typedef struct Startsb Startsb;
typedef struct Super1 Super1;
typedef struct Superb Superb;
typedef struct Tag Tag;
typedef struct Time Time;
typedef struct Tlock Tlock;
typedef struct Tm Tm;
typedef struct Uid Uid;
typedef struct Wpath Wpath;
#pragma incomplete Auth
struct Tag
{
short pad;
short tag;
Off path;
};
struct Qid9p1
{
Off path;
ulong version;
};
struct Super1
{
Off fstart;
Off fsize;
Off tfree;
Off qidgen;
Off cwraddr;
Off roraddr;
Off last;
Off next;
};
struct Centry
{
ushort age;
short state;
Off waddr;
};
struct Dentry
{
char name[NAMELEN];
Userid uid;
Userid gid;
ushort mode;
#define DALLOC 0x8000
#define DDIR 0x4000
#define DAPND 0x2000
#define DLOCK 0x1000
#define DREAD 0x4
#define DWRITE 0x2
#define DEXEC 0x1
Userid muid;
Qid9p1 qid;
Off size;
Off dblock[NDBLOCK];
Off iblocks[NIBLOCK];
long atime;
long mtime;
};
enum {
BUFSIZE = RBUFSIZE - sizeof(Tag),
DIRPERBUF = BUFSIZE / sizeof(Dentry),
INDPERBUF = BUFSIZE / sizeof(Off),
FEPERBUF = (BUFSIZE-sizeof(Super1)-sizeof(Off)) / sizeof(Off),
SMALLBUF = MAXMSG,
LARGEBUF = MAXMSG+MAXDAT+256,
RAGAP = (300*1024)/BUFSIZE,
BKPERBLK = 10,
CEPERBK = (BUFSIZE - BKPERBLK*sizeof(Off)) /
(sizeof(Centry)*BKPERBLK),
};
struct Queue
{
QLock;
Rendez empty;
Rendez full;
int waitedfor;
char* name;
int size;
int loc;
int count;
void* args[1];
};
struct Device
{
uchar type;
uchar init;
Device* link;
Device* dlink;
void* private;
Devsize size;
union {
struct {
int ctrl;
int targ;
int lun;
int mapped;
char* file;
int fd;
char* sddir;
char* sddata;
} wren;
struct {
Device* first;
Device* last;
int ndev;
} cat;
struct {
Device* c;
Device* w;
Device* ro;
} cw;
struct {
Device* j;
Device* m;
} j;
struct {
Device* parent;
} ro;
struct {
Device* fw;
} fw;
struct {
Device* d;
long base;
long size;
} part;
struct {
Device* d;
} swab;
};
};
typedef struct Sidestarts {
Devsize sstart;
Devsize s1start;
} Sidestarts;
union Rabuf {
struct {
Device* dev;
Off addr;
};
Rabuf* link;
};
struct Hiob
{
Iobuf* link;
Lock;
};
struct Chan
{
char type;
int (*protocol)(Msgbuf*);
int msize;
char whochan[50];
char whoname[NAMELEN];
void (*whoprint)(Chan*);
ulong flags;
int chan;
int nmsgs;
Timet whotime;
int nfile;
RWLock reflock;
Chan* next;
Queue* send;
Queue* reply;
uchar authinfo[64];
void* pdata;
};
struct Filsys
{
char* name;
char* conf;
Device* dev;
int flags;
#define FREAM (1<<0)
#define FRECOVER (1<<1)
#define FEDIT (1<<2)
};
struct Startsb
{
char* name;
Off startsb;
};
struct Time
{
Timet lasttoy;
Timet offset;
};
struct Tlock
{
Device* dev;
Timet time;
Off qpath;
File* file;
};
struct Cons
{
ulong flags;
QLock;
int uid;
int gid;
int nuid;
int ngid;
Off offset;
int chano;
Chan* chan;
Filsys* curfs;
int profile;
long* profbuf;
ulong minpc;
ulong maxpc;
ulong nprofbuf;
long nlarge;
long nsmall;
long nwormre;
long nwormwe;
long nwormhit;
long nwormmiss;
int noage;
long nwrenre;
long nwrenwe;
long nreseq;
};
struct File
{
QLock;
Qid qid;
Wpath* wpath;
Chan* cp;
Tlock* tlock;
File* next;
Filsys* fs;
Off addr;
long slot;
Off lastra;
ulong fid;
Userid uid;
Auth *auth;
char open;
#define FREAD 1
#define FWRITE 2
#define FREMOV 4
Off doffset;
ulong dvers;
long dslot;
};
struct Wpath
{
Wpath* up;
Off addr;
long slot;
short refs;
};
struct Iobuf
{
QLock;
Device* dev;
Iobuf* fore;
Iobuf* back;
char* iobuf;
char* xiobuf;
Off addr;
int flags;
};
struct Uid
{
Userid uid;
Userid lead;
Userid *gtab;
int ngrp;
char name[NAMELEN];
};
struct Fbuf
{
Off nfree;
Off free[FEPERBUF];
};
struct Superb
{
Fbuf fbuf;
Super1;
};
struct Conf
{
ulong nmach;
ulong mem;
ulong nuid;
ulong nserve;
ulong nfile;
ulong nwpath;
ulong gidspace;
ulong nlgmsg;
ulong nsmmsg;
Off recovcw;
Off recovro;
Off firstsb;
Off recovsb;
ulong configfirst;
char *confdev;
char *devmap;
ulong nauth;
uchar nodump;
uchar dumpreread;
};
enum {
Mbmagic = 0xb0ffe3,
};
struct Msgbuf
{
ulong magic;
short count;
short flags;
#define LARGE (1<<0)
#define FREE (1<<1)
#define BFREE (1<<2)
#define BTRACE (1<<7)
Chan* chan;
Msgbuf* next;
uintptr param;
int category;
uchar* data;
uchar* xdata;
};
enum
{
Mxxx = 0,
Mbeth1,
Mbreply1,
Mbreply2,
Mbreply3,
Mbreply4,
MAXCAT,
};
enum { PRINTSIZE = 256 };
struct
{
Lock;
int machs;
int exiting;
} active;
struct Command
{
char* arg0;
char* help;
void (*func)(int, char*[]);
};
struct Flag
{
char* arg0;
char* help;
ulong flag;
};
struct Rtc
{
int sec;
int min;
int hour;
int mday;
int mon;
int year;
};
typedef struct
{
Dentry *d;
Iobuf *p;
int uid;
Off newsize;
Off lastblk;
Off relblk;
int pastlast;
int err;
} Truncstate;
struct Cache
{
Off maddr;
Off msize;
Off caddr;
Off csize;
Off fsize;
Off wsize;
Off wmax;
Off sbaddr;
Off cwraddr;
Off roraddr;
Timet toytime;
Timet time;
};
struct Bucket
{
long agegen;
Centry entry[CEPERBK];
};
enum { Labmagic = 0xfeedfacedeadbeefULL, };
typedef struct Label Label;
struct Label
{
uvlong magic;
ushort ord;
char service[64];
};
typedef struct Map Map;
struct Map {
char *from;
Device *fdev;
char *to;
Device *tdev;
Map *next;
};
enum
{
SCSIread = 0,
SCSIwrite = 1,
};
enum
{
Dead = 0,
Moribund,
Zombie,
Ready,
Scheding,
Running,
Queueing,
Sending,
Recving,
MMUing,
Exiting,
Inwait,
Wakeme,
Broken,
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
Cckbuf,
};
enum
{
Ebadspc = 1,
Efid,
Echar,
Eopen,
Ecount,
Ealloc,
Eqid,
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
Enoattach,
Ewstatb,
Ewstatd,
Ewstatg,
Ewstatl,
Ewstatm,
Ewstato,
Ewstatp,
Ewstatq,
Ewstatu,
Ewstatv,
Ename,
Ewalk,
Eronly,
Efull,
Eoffset,
Elocked,
Ebroken,
Eauth,
Eauth2,
Efidinuse,
Etoolong,
Econvert,
Eversion,
Eauthdisabled,
Eauthnone,
Eauthfile,
Eedge,
MAXERR
};
enum
{
Devnone = 0,
Devcon,
Devwren,
Devworm,
Devlworm,
Devfworm,
Devjuke,
Devcw,
Devro,
Devmcat,
Devmlev,
Devnet,
Devpart,
Devfloppy,
Devswab,
Devmirr,
MAXDEV
};
enum
{
Tnone = 0,
Tsuper,
#ifdef COMPAT32
Tdir,
Tind1,
Tind2,
#else
Tdirold,
Tind1old,
Tind2old,
#endif
Tfile,
Tfree,
Tbuck,
Tvirgo,
Tcache,
Tconfig,
#ifndef COMPAT32
Tdir,
Tind1,
Tind2,
Tind3,
Tind4,
Maxtind,
#endif
Tlabel = 32,
MAXTAG,
#ifdef COMPAT32
Tmaxind = Tind2,
#else
Tmaxind = Maxtind - 1,
#endif
};
enum
{
Brd = (1<<0),
Bprobe = (1<<1),
Bmod = (1<<2),
Bimm = (1<<3),
Bres = (1<<4),
};
Conf conf;
Cons cons;
#pragma varargck type "Z" Device*
#pragma varargck type "T" Timet
#pragma varargck type "I" uchar*
#pragma varargck type "E" uchar*
#pragma varargck type "G" int
extern char *annstrs[];
extern Biobuf bin;
extern Map *devmap;
extern int (*fsprotocol[])(Msgbuf*);