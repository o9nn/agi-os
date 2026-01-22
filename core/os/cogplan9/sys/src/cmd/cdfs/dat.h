enum {
Maxtrack = 200,
Ntrack = Maxtrack+1,
BScdrom = 2048,
BScdda = 2352,
BScdxa = 2336,
BSmax = 2352,
Maxfeatures = 512,
TypeDA = 0,
TypeSA = 1,
TypeWO = 4,
TypeCD = 5,
TypeMO = 7,
TypeMC = 8,
Mmcnone = 0,
Mmccd,
Mmcdvdminus,
Mmcdvdplus,
Mmcbd,
TypeNone = 0,
TypeAudio,
TypeAwritable,
TypeData,
TypeDwritable,
TypeDisk,
TypeBlank,
Readonly = 0,
Write1,
Erasewrite,
Ram,
Unset = -1,
No,
Yes,
Capread = 2,
Capwrite = 3,
Capmisc = 5,
Capcdr = 1<<0,
Capcdrw = 1<<1,
Captestwr = 1<<2,
Capdvdrom = 1<<3,
Capdvdr = 1<<4,
Capdvdram = 1<<5,
Capcdda = 1<<0,
Caprw = 1<<2,
Wpwrtype = 2,
Wptrkmode,
Wpdatblktype,
Wpsessfmt = 8,
Wppktsz = 10,
Bufe = 1<<6,
Msbits = 3<<6,
Msnonext= 0<<6,
Mscdnonext= 1<<6,
Msnext = 3<<6,
Fp = 1<<5,
Closetrack = 1,
Closesessfinal = 2,
Closefinaldvdrw = 3,
Closesessextdvdrdl = 4,
Closefinal30mm = 5,
Closedvdrbdfinal= 6,
Tocfmttoc = 0,
Tocfmtsessnos = 1,
Tocfmtqleadin = 2,
Tocfmtqpma = 3,
Tocfmtatip = 4,
Tocfmtcdtext = 5,
Msfbit = 1<<1,
Wtpkt = 0,
Wttrackonce,
Wtsessonce,
Wtraw,
Wtlayerjump,
Tmcdda = 0,
Tm2audio,
Tmunintr = 4,
Tmintr,
Dbraw = 0,
Db2kdata = 8,
Db2336,
Sfdata = 0,
Sfcdi = 0x10,
Sfcdxa = 0x20,
Ccrcd = 1<<0,
Ccmf = 1<<1,
Ccwce = 1<<2,
Ccsize = 1<<3,
Ccdisc = 1<<4,
Cccap = 1<<5,
Ccabpf = 1<<6,
Ccic = 1<<7,
Cwrite = 1<<0,
Ccdda = 1<<1,
CDNblock = 12,
DVDNblock = 16,
BDNblock = 32,
Readblock = 8192/BScdrom,
};
typedef struct Buf Buf;
typedef struct Dev Dev;
typedef struct Drive Drive;
typedef struct Msf Msf;
typedef struct Otrack Otrack;
typedef struct Track Track;
typedef schar Tristate;
struct Msf {
int m;
int s;
int f;
};
struct Track
{
vlong size;
long bs;
ulong beg;
ulong end;
int type;
Msf mbeg;
Msf mend;
char name[32];
int mode;
ulong mtime;
};
struct DTrack
{
uchar name[32];
uchar beg[4];
uchar end[4];
uchar size[8];
uchar magic[4];
};
struct Otrack
{
Track *track;
Drive *drive;
int nchange;
int omode;
Buf *buf;
int nref;
};
struct Dev
{
Otrack* (*openrd)(Drive *d, int trackno);
Otrack* (*create)(Drive *d, int bs);
long (*read)(Otrack *t, void *v, long n, vlong off);
long (*write)(Otrack *t, void *v, long n);
void (*close)(Otrack *t);
int (*gettoc)(Drive*);
int (*fixate)(Drive *d);
char* (*ctl)(Drive *d, int argc, char **argv);
char* (*setspeed)(Drive *d, int r, int w);
};
struct Drive
{
QLock;
Scsi;
int type;
int mmctype;
char *dvdtype;
char *laysfx;
int firsttrack;
int invistrack;
int ntrack;
int nchange;
ulong changetime;
int relearn;
int nameok;
int writeok;
Tristate recordable;
Tristate erasable;
Track track[Ntrack];
ulong end;
ulong cap;
uchar blkbuf[BScdda];
int maxreadspeed;
int maxwritespeed;
int readspeed;
int writespeed;
Dev;
uchar features[Maxfeatures/8];
void *aux;
};
struct Buf
{
uchar *data;
vlong off;
int bs;
long ndata;
int nblock;
int omode;
long (*fn)(Buf*, void*, long, ulong);
Otrack *otrack;
};
extern int vflag;