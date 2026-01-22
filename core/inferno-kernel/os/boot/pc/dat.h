typedef struct List {
void *next;
} List;
typedef struct Alarm Alarm;
typedef struct Alarm {
List;
int busy;
long dt;
void (*f)(Alarm*);
void *arg;
} Alarm;
typedef struct Apminfo {
int haveinfo;
int ax;
int cx;
int dx;
int di;
int ebx;
int esi;
} Apminfo;
typedef struct Block Block;
struct Block {
Block* next;
uchar* rp;
uchar* wp;
uchar* lim;
uchar* base;
ulong flag;
};
#define BLEN(s) ((s)->wp - (s)->rp)
typedef struct IOQ IOQ;
typedef struct IOQ {
uchar buf[4096];
uchar *in;
uchar *out;
int state;
int (*getc)(IOQ*);
int (*putc)(IOQ*, int);
void *ptr;
};
enum {
Eaddrlen = 6,
ETHERMINTU = 60,
ETHERMAXTU = 1514,
ETHERHDRSIZE = 14,
MaxEther = 6,
};
typedef struct {
uchar d[Eaddrlen];
uchar s[Eaddrlen];
uchar type[2];
uchar data[1500];
uchar crc[4];
} Etherpkt;
extern uchar broadcast[Eaddrlen];
typedef struct Ureg Ureg;
#pragma incomplete Ureg
typedef struct Segdesc {
ulong d0;
ulong d1;
} Segdesc;
typedef struct Mach {
ulong ticks;
void *alarm;
} Mach;
extern Mach *m;
#define I_MAGIC ((((4*11)+0)*11)+7)
typedef struct Exec Exec;
struct Exec
{
uchar magic[4];
uchar text[4];
uchar data[4];
uchar bss[4];
uchar syms[4];
uchar entry[4];
uchar spsz[4];
uchar pcsz[4];
};
#define ISAOPTLEN 32
#define NISAOPT 8
typedef struct ISAConf {
char type[NAMELEN];
ulong port;
ulong irq;
ulong mem;
ulong size;
uchar ea[6];
int nopt;
char opt[NISAOPT][ISAOPTLEN];
} ISAConf;
typedef struct Pcidev Pcidev;
typedef struct PCMmap PCMmap;
typedef struct PCMslot PCMslot;
#define BOOTLINE ((char*)CONFADDR)
enum {
MB = (1024*1024),
};
#define ROUND(s, sz) (((s)+((sz)-1))&~((sz)-1))
typedef struct Type Type;
typedef struct Medium Medium;
typedef struct Boot Boot;
enum {
Tnil = 0x00,
Tfloppy = 0x01,
Tsd = 0x02,
Tether = 0x03,
Tcd = 0x04,
Tbios = 0x05,
Tany = -1,
};
enum {
Fnone = 0x00,
Nfs = 0x00,
Ffs = (1<<Nfs),
Nboot = 0x01,
Fboot = (1<<Nboot),
Nbootp = 0x02,
Fbootp = (1<<Nbootp),
NName = 3,
Fany = Fbootp|Fboot|Ffs,
Fini = 0x10,
Fprobe = 0x80,
};
typedef struct Type {
int type;
int flag;
int (*init)(void);
void (*initdev)(int, char*);
void* (*getfspart)(int, char*, int);
void (*addconf)(int);
int (*boot)(int, char*, Boot*);
void (*printdevs)(int);
char** parts;
char** inis;
int mask;
Medium* media;
} Type;
extern void (*etherdetach)(void);
extern void (*floppydetach)(void);
extern void (*sddetach)(void);
typedef struct Lock {
int locked;
int spl;
} Lock;
enum {
MORE, ENOUGH, FAIL
};
enum {
INITKERNEL,
READEXEC,
READ9TEXT,
READ9DATA,
READGZIP,
READEHDR,
READPHDR,
READEPAD,
READEDATA,
TRYBOOT,
INIT9LOAD,
READ9LOAD,
FAILED
};
struct Boot {
int state;
Exec exec;
char *bp;
char *wp;
char *ep;
};
extern int debug;
extern Apminfo apm;
extern char *defaultpartition;
extern int iniread;
extern int pxe;