typedef struct Dosboot Dosboot;
typedef struct Dosboot32 Dosboot32;
typedef struct Dosbpb Dosbpb;
typedef struct Dosdir Dosdir;
typedef struct Dospart Dospart;
typedef struct Dosptr Dosptr;
typedef struct Fatinfo Fatinfo;
typedef struct Xfs Xfs;
typedef struct Xfile Xfile;
struct Dospart{
uchar active;
uchar hstart;
uchar cylstart[2];
uchar type;
uchar hend;
uchar cylend[2];
uchar start[4];
uchar length[4];
};
enum
{
FAT12 = 0x01,
FAT16 = 0x04,
FATHUGE = 0x06,
FAT32 = 0x0b,
FAT32X = 0x0c,
FATHUGEX = 0x0e,
DMDDO = 0x54,
FATRESRV = 2,
};
struct Dosboot{
uchar magic[3];
uchar version[8];
uchar sectsize[2];
uchar clustsize;
uchar nresrv[2];
uchar nfats;
uchar rootsize[2];
uchar volsize[2];
uchar mediadesc;
uchar fatsize[2];
uchar trksize[2];
uchar nheads[2];
uchar nhidden[4];
uchar bigvolsize[4];
uchar driveno;
uchar reserved0;
uchar bootsig;
uchar volid[4];
uchar label[11];
uchar reserved1[8];
};
enum
{
NOFATMIRROR = 0x0080,
ACTFATMASK = 0x000f,
};
struct Dosboot32{
uchar magic[3];
uchar version[8];
uchar sectsize[2];
uchar clustsize;
uchar nresrv[2];
uchar nfats;
uchar rootsize[2];
uchar volsize[2];
uchar mediadesc;
uchar fatsize[2];
uchar trksize[2];
uchar nheads[2];
uchar nhidden[4];
uchar bigvolsize[4];
uchar fatsize32[4];
uchar extflags[2];
uchar version1[2];
uchar rootstart[4];
uchar infospec[2];
uchar backupboot[2];
uchar reserved[12];
};
enum
{
FATINFOSIG1 = 0x41615252UL,
FATINFOSIG = 0x61417272UL,
};
struct Fatinfo
{
uchar sig1[4];
uchar pad[480];
uchar sig[4];
uchar freeclust[4];
uchar nextfree[4];
uchar resrv[4*3];
};
struct Dosbpb{
MLock;
int sectsize;
int clustsize;
int nresrv;
int nfats;
int rootsize;
long volsize;
int mediadesc;
long fatsize;
int fatclusters;
int fatbits;
long fataddr;
long rootaddr;
long rootstart;
long dataaddr;
long freeptr;
long freeclusters;
int fatinfo;
};
enum
{
DOSDIRSIZE = 32,
DOSEMPTY = 0xe5,
DOSRUNE = 13,
DOSNAMELEN = 261
};
struct Dosdir{
uchar name[8];
uchar ext[3];
uchar attr;
uchar reserved[1];
uchar ctime[3];
uchar cdate[2];
uchar adate[2];
uchar hstart[2];
uchar time[2];
uchar date[2];
uchar start[2];
uchar length[4];
};
enum
{
DRONLY = 0x01,
DHIDDEN = 0x02,
DSYSTEM = 0x04,
DVLABEL = 0x08,
DDIR = 0x10,
DARCH = 0x20,
};
#define GSHORT(p) (((p)[0])|(p)[1]<<8)
#define GLONG(p) (((long)(p)[0])|(p)[1]<<8|(p)[2]<<16|(p)[3]<<24)
#define PSHORT(p,v) ((p)[0]=(v),(p)[1]=(v)>>8)
#define PLONG(p,v) ((p)[0]=(v),(p)[1]=(v)>>8,(p)[2]=(v)>>16,(p)[3]=(v)>>24)
struct Dosptr{
ulong addr;
ulong offset;
ulong paddr;
ulong poffset;
ulong iclust;
ulong clust;
ulong naddr;
ulong prevaddr;
Iosect *p;
Dosdir *d;
};
#define QIDPATH(p) ((p)->addr*(Sectorsize/DOSDIRSIZE) + \
(p)->offset/DOSDIRSIZE)
struct Xfs{
Xfs *next;
int omode;
char *name;
Qid qid;
long ref;
Qid rootqid;
uchar isfat32;
short dev;
short fmt;
long offset;
void *ptr;
};
struct Xfile{
Xfile *next;
long fid;
ulong flags;
Qid qid;
Xfs *xf;
Dosptr *ptr;
};
enum{
Asis, Clean, Clunk
};
enum{
Invalid, Short, ShortLower, Long
};
enum{
Oread = 1,
Owrite = 2,
Orclose = 4,
Omodes = 3,
};
enum{
Enevermind,
Eformat,
Eio,
Enoauth,
Enomem,
Enonexist,
Eperm,
Enofilsys,
Eauth,
Econtig,
Ebadfcall,
Ebadstat,
Eversion,
Etoolong,
Eerrstr,
ESIZE
};
extern int chatty;
extern int errno;
extern int readonly;
extern char *deffile;
extern int trspaces;