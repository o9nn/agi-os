enum {
Emed = 1<<0,
Enm = 1<<1,
Eabrt = 1<<2,
Emcr = 1<<3,
Eidnf = 1<<4,
Emc = 1<<5,
Eunc = 1<<6,
Ewp = 1<<6,
Eicrc = 1<<7,
Efatal = Eidnf|Eicrc,
};
enum {
ASerr = 1<<0,
ASdrq = 1<<3,
ASdf = 1<<5,
ASdrdy = 1<<6,
ASbsy = 1<<7,
ASobs = 1<<1|1<<2|1<<4,
};
enum {
Abar = 5,
};
enum {
Hs64a = 1<<31,
Hsncq = 1<<30,
Hssntf = 1<<29,
Hsmps = 1<<28,
Hsss = 1<<27,
Hsalp = 1<<26,
Hsal = 1<<25,
Hsclo = 1<<24,
Hiss = 1<<20,
Hsam = 1<<18,
Hspm = 1<<17,
Hpmb = 1<<15,
Hssc = 1<<14,
Hpsc = 1<<13,
Hncs = 1<<8,
Hcccs = 1<<7,
Hems = 1<<6,
Hsxs = 1<<5,
Hnp = 1<<0,
};
enum {
Hae = 1<<31,
Hie = 1<<1,
Hhr = 1<<0,
};
typedef struct {
ulong cap;
ulong ghc;
ulong isr;
ulong pi;
ulong ver;
ulong ccc;
ulong cccports;
ulong emloc;
ulong emctl;
} Ahba;
enum {
Acpds = 1<<31,
Atfes = 1<<30,
Ahbfs = 1<<29,
Ahbds = 1<<28,
Aifs = 1<<27,
Ainfs = 1<<26,
Aofs = 1<<24,
Aipms = 1<<23,
Aprcs = 1<<22,
Adpms = 1<<7,
Apcs = 1<<6,
Adps = 1<<5,
Aufs = 1<<4,
Asdbs = 1<<3,
Adss = 1<<2,
Apio = 1<<1,
Adhrs = 1<<0,
IEM = Acpds|Atfes|Ahbds|Ahbfs|Ahbds|Aifs|Ainfs|Aprcs|Apcs|Adps|
Aufs|Asdbs|Adss|Adhrs,
Ifatal = Atfes|Ahbfs|Ahbds|Aifs,
};
enum {
SerrX = 1<<26,
SerrF = 1<<25,
SerrT = 1<<24,
SerrS = 1<<23,
SerrH = 1<<22,
SerrC = 1<<21,
SerrD = 1<<20,
SerrB = 1<<19,
SerrW = 1<<18,
SerrI = 1<<17,
SerrN = 1<<16,
ErrE = 1<<11,
ErrP = 1<<10,
ErrC = 1<<9,
ErrT = 1<<8,
ErrM = 1<<1,
ErrI = 1<<0,
ErrAll = ErrE|ErrP|ErrC|ErrT|ErrM|ErrI,
SerrAll = SerrX|SerrF|SerrT|SerrS|SerrH|SerrC|SerrD|SerrB|SerrW|
SerrI|SerrN|ErrAll,
SerrBad = 0x7f<<19,
};
enum {
Aicc = 1<<28,
Aasp = 1<<27,
Aalpe = 1<<26,
Adlae = 1<<25,
Aatapi = 1<<24,
Aesp = 1<<21,
Acpd = 1<<20,
Ampsp = 1<<19,
Ahpcp = 1<<18,
Apma = 1<<17,
Acps = 1<<16,
Acr = 1<<15,
Afr = 1<<14,
Ampss = 1<<13,
Accs = 1<<8,
Afre = 1<<4,
Aclo = 1<<3,
Apod = 1<<2,
Asud = 1<<1,
Ast = 1<<0,
Arun = Ast|Acr|Afre|Afr,
};
enum {
Aipm = 1<<8,
Aspd = 1<<4,
Adet = 1<<0,
};
#define sstatus scr0
#define sctl scr2
#define serror scr1
#define sactive scr3
typedef struct {
ulong list;
ulong listhi;
ulong fis;
ulong fishi;
ulong isr;
ulong ie;
ulong cmd;
ulong res1;
ulong task;
ulong sig;
ulong scr0;
ulong scr2;
ulong scr1;
ulong scr3;
ulong ci;
ulong ntf;
uchar res2[8];
ulong vendor;
} Aport;
enum {
Intslumber = 0x600,
Intpartpwr = 0x200,
Intactive = 0x100,
Intpm = 0xf00,
Devphyoffline = 4,
Devphycomm = 2,
Devpresent = 1,
Devdet = Devpresent | Devphycomm | Devphyoffline,
};
typedef struct {
uchar *base;
uchar *d;
uchar *p;
uchar *r;
uchar *u;
ulong *devicebits;
} Afis;
enum {
Lprdtl = 1<<16,
Lpmp = 1<<12,
Lclear = 1<<10,
Lbist = 1<<9,
Lreset = 1<<8,
Lpref = 1<<7,
Lwrite = 1<<6,
Latapi = 1<<5,
Lcfl = 1<<0,
};
typedef struct {
ulong flags;
ulong len;
ulong ctab;
ulong ctabhi;
uchar reserved[16];
} Alist;
typedef struct {
ulong dba;
ulong dbahi;
ulong pad;
ulong count;
} Aprdt;
typedef struct {
uchar cfis[0x40];
uchar atapi[0x10];
uchar pad[0x30];
Aprdt prdt;
} Actab;
enum {
Ferror = 1,
Fdone = 2,
};
enum {
Dllba = 1,
Dsmart = 1<<1,
Dpower = 1<<2,
Dnop = 1<<3,
Datapi = 1<<4,
Datapi16= 1<<5,
};
typedef struct {
QLock;
Rendez;
uchar flag;
uchar feat;
uchar smart;
Afis fis;
Alist *list;
Actab *ctab;
} Aportm;
typedef struct {
Aport *p;
Aportm *m;
} Aportc;