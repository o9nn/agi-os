typedef struct Mii Mii;
typedef struct MiiPhy MiiPhy;
enum {
Bmcr = 0x00,
Bmsr = 0x01,
Phyidr1 = 0x02,
Phyidr2 = 0x03,
Anar = 0x04,
Anlpar = 0x05,
Aner = 0x06,
Annptr = 0x07,
Annprr = 0x08,
Mscr = 0x09,
Mssr = 0x0A,
Esr = 0x0F,
NMiiPhyr = 32,
NMiiPhy = 32,
};
enum {
BmcrSs1 = 0x0040,
BmcrCte = 0x0080,
BmcrDm = 0x0100,
BmcrRan = 0x0200,
BmcrI = 0x0400,
BmcrPd = 0x0800,
BmcrAne = 0x1000,
BmcrSs0 = 0x2000,
BmcrLe = 0x4000,
BmcrR = 0x8000,
};
enum {
BmsrEc = 0x0001,
BmsrJd = 0x0002,
BmsrLs = 0x0004,
BmsrAna = 0x0008,
BmsrRf = 0x0010,
BmsrAnc = 0x0020,
BmsrPs = 0x0040,
BmsrEs = 0x0100,
Bmsr100T2HD = 0x0200,
Bmsr100T2FD = 0x0400,
Bmsr10THD = 0x0800,
Bmsr10TFD = 0x1000,
Bmsr100TXHD = 0x2000,
Bmsr100TXFD = 0x4000,
Bmsr100T4 = 0x8000,
};
enum {
Ana10HD = 0x0020,
Ana10FD = 0x0040,
AnaTXHD = 0x0080,
AnaTXFD = 0x0100,
AnaT4 = 0x0200,
AnaP = 0x0400,
AnaAP = 0x0800,
AnaRf = 0x2000,
AnaAck = 0x4000,
AnaNp = 0x8000,
};
enum {
Mscr1000THD = 0x0100,
Mscr1000TFD = 0x0200,
};
enum {
Mssr1000THD = 0x0400,
Mssr1000TFD = 0x0800,
};
enum {
Esr1000THD = 0x1000,
Esr1000TFD = 0x2000,
Esr1000XHD = 0x4000,
Esr1000XFD = 0x8000,
};
typedef struct Mii {
Lock;
int nphy;
int mask;
MiiPhy* phy[NMiiPhy];
MiiPhy* curphy;
void* ctlr;
int (*mir)(Mii*, int, int);
int (*miw)(Mii*, int, int, int);
} Mii;
typedef struct MiiPhy {
Mii* mii;
int oui;
int phyno;
int anar;
int fc;
int mscr;
int link;
int speed;
int fd;
int rfc;
int tfc;
};
extern int mii(Mii*, int);
extern int miiane(Mii*, int, int, int);
extern int miimir(Mii*, int);
extern int miimiw(Mii*, int, int);
extern int miireset(Mii*);
extern int miistatus(Mii*);