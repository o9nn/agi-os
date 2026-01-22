typedef struct FController FController;
typedef struct FDrive FDrive;
typedef struct FType FType;
static void floppyintr(Ureg*);
static int floppyon(FDrive*);
static void floppyoff(FDrive*);
static void floppysetdef(FDrive*);
struct FDrive
{
FType *t;
int dt;
int dev;
ulong lasttouched;
int cyl;
int confused;
int offset;
int vers;
int maxtries;
int tcyl;
int thead;
int tsec;
long len;
uchar *cache;
int ccyl;
int chead;
void *aux;
};
struct FController
{
int ndrive;
FDrive *d;
FDrive *selected;
int rate;
uchar cmd[14];
int ncmd;
uchar stat[14];
int nstat;
int confused;
int motor;
};
struct FType
{
char *name;
int dt;
int bytes;
int sectors;
int heads;
int steps;
int tracks;
int gpl;
int fgpl;
int rate;
int bcode;
long cap;
long tsize;
};
enum
{
Psra= 0x3f0,
Psrb= 0x3f1,
Pdor= 0x3f2,
Fintena= 0x8,
Fena= 0x4,
Pmsr= 0x3f4,
Fready= 0x80,
Ffrom= 0x40,
Ffloppybusy= 0x10,
Pfdata= 0x3f5,
Frecal= 0x07,
Fseek= 0x0f,
Fsense= 0x08,
Fread= 0x66,
Freadid= 0x4a,
Fspec= 0x03,
Fwrite= 0x45,
Fformat= 0x4d,
Fmulti= 0x80,
Fdumpreg= 0x0e,
Pdir= 0x3F7,
Pdsr= 0x3F7,
Fchange= 0x80,
Drivemask= 3<<0,
Seekend= 1<<5,
Codemask= (3<<6)|(3<<3),
Cmdexec= 1<<6,
Overrun= 0x10,
};
enum
{
Tnone= 0,
T360kb= 1,
T1200kb= 2,
T720kb= 3,
T1440kb= 4,
};
static void
pcfloppyintr(Ureg *ur, void *a)
{
USED(a);
floppyintr(ur);
}
void
floppysetup0(FController *fl)
{
uchar equip;
equip = nvramread(0x10);
fl->ndrive = 1;
if(equip & 0xf)
fl->ndrive++;
fl->d = xalloc(fl->ndrive*sizeof(FDrive));
fl->d[0].dt = (equip >> 4) & 0xf;
if(fl->d[0].dt == Tnone)
fl->d[0].dt = T1440kb;
if(fl->ndrive == 2)
fl->d[1].dt = equip & 0xf;
}
void
floppysetup1(FController*)
{
setvec(VectorFLOPPY, pcfloppyintr, 0);
}
static vlong pcfloppyseek(FDrive*, vlong);
FController fl;
vlong
floppyseek(Fs *fs, vlong off)
{
FDrive *dp;
dp = &fl.d[fs->dev];
return pcfloppyseek(dp, off);
}