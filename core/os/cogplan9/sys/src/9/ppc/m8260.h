typedef struct BD BD;
struct BD {
ushort status;
ushort length;
ulong addr;
};
enum{
BDEmpty= SBIT(0),
BDReady= SBIT(0),
BDWrap= SBIT(2),
BDInt= SBIT(3),
BDLast= SBIT(4),
BDFirst= SBIT(5),
};
typedef struct Ring Ring;
struct Ring {
BD* rdr;
void* rrb;
int rdrx;
int nrdre;
BD* tdr;
void** txb;
int tdrh;
int tdri;
int ntdre;
int ntq;
};
int ioringinit(Ring*, int, int, int);
typedef struct MCCparam MCCparam;
struct MCCparam {
ulong mccbase;
ushort mccstate;
ushort mrblr;
ushort grfthr;
ushort grfcnt;
ulong rinttmp;
ulong data0;
ulong data1;
ulong tintbase;
ulong tintptr;
ulong tinttmp;
ushort sctpbase;
ushort Rsvd26;
ulong cmask32;
ushort xtrabase;
ushort cmask16;
ulong rinttmp[4];
struct {
ulong base;
ulong ptr;
} rint[4];
ulong tstmp;
};
typedef struct IOCparam IOCparam;
struct IOCparam {
ushort rbase;
ushort tbase;
uchar rfcr;
uchar tfcr;
ushort mrblr;
ulong rstate;
ulong rxidp;
ushort rbptr;
ushort rxibc;
ulong rxtemp;
ulong tstate;
ulong txidp;
ushort tbptr;
ushort txibc;
ulong txtemp;
};
typedef struct SCCparam SCCparam;
struct SCCparam {
IOCparam;
ulong rcrc;
ulong tcrc;
};
typedef struct FCCparam FCCparam;
struct FCCparam {
ushort riptr;
ushort tiptr;
ushort Rsvd04;
ushort mrblr;
ulong rstate;
ulong rbase;
ushort rbdstat;
ushort rbdlen;
char* rdptr;
ulong tstate;
ulong tbase;
ushort tbdstat;
ushort tbdlen;
ulong tdptr;
ulong rbptr;
ulong tbptr;
ulong rcrc;
ulong Rsvd34;
ulong tcrc;
};
typedef struct SCC SCC;
struct SCC {
ulong gsmrl;
ulong gsmrh;
ushort psmr;
uchar rsvscc0[2];
ushort todr;
ushort dsr;
ushort scce;
uchar rsvscc1[2];
ushort sccm;
uchar rsvscc2;
uchar sccs;
ushort irmode;
ushort irsip;
uchar rsvscc3[4];
};
typedef struct FCC FCC;
struct FCC {
ulong gfmr;
ulong fpsmr;
ushort ftodr;
ushort Rsvd0A;
ushort fdsr;
ushort Rsvd0E;
ushort fcce;
ushort Rsvd12;
ushort fccm;
ushort Rsvd16;
uchar fccs;
uchar Rsvd19[3];
uchar ftirrphy[4];
};
typedef struct SMC SMC;
struct SMC {
ushort pad1;
ushort smcmr;
ushort pad2;
uchar smce;
uchar pad3[3];
uchar smcm;
uchar pad4[5];
};
typedef struct SPI SPI;
struct SPI {
ushort spmode;
uchar res1[4];
uchar spie;
uchar res2[3];
uchar spim;
uchar res3[2];
uchar spcom;
uchar res4[2];
};
typedef struct Bankmap Bankmap;
struct Bankmap {
ulong br;
ulong or;
};
typedef struct Port Port;
struct Port {
ulong pdir;
ulong ppar;
ulong psor;
ulong podr;
ulong pdat;
uchar Rsvd14[12];
};
typedef struct IDMA IDMA;
struct IDMA {
uchar idsr;
uchar Rsvd1[3];
uchar idmr;
uchar Rsvd5[3];
};
typedef struct PrmSCC PrmSCC;
struct PrmSCC {
uchar sccbytes[0x100];
};
typedef struct PrmFCC PrmFCC;
struct PrmFCC {
uchar fccbytes[0x100];
};
typedef struct Bases Bases;
struct Bases {
uchar mcc[0x80];
uchar Rsvd80[0x60];
uchar risctimers[0x10];
ushort revnum;
uchar Rsvdf2[6];
ulong rand;
ushort smcbase;
#define i2cbase smcbase
ushort idmabase;
};
typedef struct Uartsmc Uartsmc;
struct Uartsmc {
IOCparam;
ushort maxidl;
ushort idlc;
ushort brkln;
ushort brkec;
ushort brkcr;
ushort r_mask;
ulong sdminternal;
uchar Rsvd38[8];
};
typedef struct SI SI;
struct SI {
ushort siamr;
ushort sibmr;
ushort sicmr;
ushort sidmr;
uchar sigmr;
uchar Rsvd11B29;
ushort sicmdr;
ushort sistr;
ushort sirsr;
};
typedef struct IMM IMM;
struct IMM {
ulong siumcr;
ulong sypcr;
uchar Rsvd10008[0xe-0x8];
ushort swsr;
uchar Rsvd10010[0x14];
ulong bcr;
ulong PPC_ACR;
ulong PPCALRH;
ulong PPC_ALRL;
ulong LCL_ACR;
ulong LCL_ALRH;
ulong LCL_ALRL;
ulong TESCR1;
ulong TESCR2;
ulong L_TESCR1;
ulong L_TESCR2;
ulong pdtea;
uchar pdtem;
uchar Rsvd10055[3];
void* ldtea;
uchar ldtem;
uchar Rsvd1005D[163];
Bankmap bank[12];
uchar Rsvd10160[8];
void* MAR;
ulong Rsvd1016C;
ulong MAMR;
ulong MBMR;
ulong MCMR;
uchar Rsvd1017C[6];
ulong mptpr;
ulong mdr;
ulong Rsvd1018C;
ulong psdmr;
ulong lsdmr;
ulong PURT;
ulong PSRT;
ulong LURT;
ulong LSRT;
ulong immr;
uchar Rsvd101AC[84];
uchar Rsvd10200[32];
ulong TMCNTSC;
ulong TMCNT;
ulong Rsvd10228;
ulong TMCNTAL;
uchar Rsvd10230[0x10];
ulong PISCR;
ulong PITC;
ulong PITR;
uchar Rsvd1024C[94];
uchar Rsvd102AA[2390];
ushort sicr;
ushort Rsvd10C02;
ulong sivec;
ulong sipnr_h;
ulong sipnr_l;
ulong siprr;
ulong scprr_h;
ulong scprr_l;
ulong simr_h;
ulong simr_l;
ulong siexr;
uchar Rsvd10C28[88];
ulong sccr;
uchar Rsvd10C84[4];
ulong scmr;
uchar Rsvd10C8C[4];
ulong rsr;
ulong rmr;
uchar Rsvd10C98[104];
Port port[4];
uchar tgcr1;
uchar Rsvd10D81[3];
uchar tgcr2;
uchar Rsvd10D85[3];
uchar Rsvd10D88[8];
ushort tmr1;
ushort tmr2;
union{
struct {
ushort trr1;
ushort trr2;
};
ulong trrl1;
};
union{
struct {
ushort tcr1;
ushort tcr2;
};
ulong tcrl1;
};
union{
struct {
ushort tcn1;
ushort tcn2;
};
ulong tcnl1;
};
ushort tmr3;
ushort tmr4;
union{
struct {
ushort trr3;
ushort trr4;
};
ulong trrl3;
};
union{
struct {
ushort tcr3;
ushort tcr4;
};
ulong tcrl3;
};
union{
struct {
ushort tcn3;
ushort tcn4;
};
ulong tcnl3;
};
ushort ter1;
ushort ter2;
ushort ter3;
ushort ter4;
uchar Rsvd10DB8[608];
uchar sdsr;
uchar Rsvd11019[3];
uchar sdmr;
uchar Rsvd1101D[3];
IDMA idma[4];
uchar Rsvd11040[704];
FCC fcc[3];
uchar Rsvd11360[0x290];
ulong BRGC5;
ulong BRGC6;
ulong BRGC7;
ulong BRGC8;
uchar Rsvd11600[0x260];
uchar I2MOD;
uchar Rsvd11861[3];
uchar I2ADD;
uchar Rsvd11865[3];
uchar I2BRG;
uchar Rsvd11869[3];
uchar I2COM;
uchar Rsvd1186D[3];
uchar I2CER;
uchar Rsvd11871[3];
uchar I2CMR;
uchar Rsvd11875[331];
ulong cpcr;
ulong rccr;
uchar Rsvd119C8[14];
ushort rter;
ushort Rsvd119D8;
ushort rtmr;
ushort rtscr;
ushort Rsvd119DE;
ulong rtsr;
uchar Rsvd119E4[12];
ulong brgc[4];
SCC scc[4];
SMC smc[2];
SPI spi;
uchar Rsvd11AB0[80];
uchar cmxsi1cr;
uchar Rsvd11B01;
uchar cmxsi2cr;
uchar Rsvd11B03;
ulong cmxfcr;
ulong cmxscr;
uchar cmxsmr;
uchar Rsvd11B0D;
ushort cmxuar;
uchar Rsvd11B10[16];
SI si1;
ushort MCCE1;
ushort Rsvd11B32;
ushort MCCM1;
ushort Rsvd11B36;
uchar MCCF1;
uchar Rsvd11B39[7];
SI si2;
ushort MCCE2;
ushort Rsvd11B52;
ushort MCCM2;
ushort Rsvd11B56;
uchar MCCF2;
uchar Rsvd11B59[1191];
uchar SI1TxRAM[0x200];
uchar Rsvd12200[0x200];
uchar SI1RxRAM[0x200];
uchar Rsvd12600[0x200];
uchar SI2TxRAM[0x200];
uchar Rsvd12A00[0x200];
uchar SI2RxRAM[0x200];
uchar Rsvd12E00[0x200];
uchar Rsvd13000[0x800];
uchar Rsvd13800[0x800];
};
typedef struct FCCextra FCCextra;
struct FCCextra {
uchar ri[0x20];
uchar ti[0x20];
uchar pad[0x20];
};
typedef struct Imap Imap;
struct Imap {
uchar dpram1[0x3800];
FCCextra fccextra[4];
Uartsmc uartsmc[2];
uchar dsp1p[0x40];
uchar dsp2p[0x40];
BD bd[(0x04000-0x03a80)/sizeof(BD)];
uchar Rsvd4000[0x04000];
PrmSCC prmscc[4];
PrmFCC prmfcc[3];
Bases param[4];
uchar dpram2[0x500];
uchar Rsvd9000[0x2000];
uchar dpram3[0x1000];
uchar Rsvdc000[0x4000];
IMM;
};
enum {
cpm_rst = 0x80000000,
cpm_page = 0x7c000000,
cpm_sblock = 0x03e00000,
cpm_flg = 0x00010000,
cpm_mcn = 0x00003fc0,
cpm_opcode = 0x0000000f,
cpm_fcc1_sblock = 0x10,
cpm_fcc2_sblock = 0x11,
cpm_fcc3_sblock = 0x12,
cpm_scc1_sblock = 0x04,
cpm_scc2_sblock = 0x05,
cpm_scc3_sblock = 0x06,
cpm_scc4_sblock = 0x07,
cpm_smc1_sblock = 0x08,
cpm_smc2_sblock = 0x09,
cpm_rand_sblock = 0x0e,
cpm_spi_sblock = 0x0a,
cpm_i2c_sblock = 0x0b,
cpm_timer_sblock = 0x0f,
cpm_mcc1_sblock = 0x1c,
cpm_mcc2_sblock = 0x1d,
cpm_idma1_sblock = 0x14,
cpm_idma2_sblock = 0x15,
cpm_idma3_sblock = 0x16,
cpm_idma4_sblock = 0x17,
cpm_scc1_page = 0x00,
cpm_scc2_page = 0x01,
cpm_scc3_page = 0x02,
cpm_scc4_page = 0x03,
cpm_smc1_page = 0x07,
cpm_smc2_page = 0x08,
cpm_spi_page = 0x09,
cpm_i2c_page = 0x0a,
cpm_timer_page = 0x0a,
cpm_rand_page = 0x0a,
cpm_fcc1_page = 0x04,
cpm_fcc2_page = 0x05,
cpm_fcc3_page = 0x06,
cpm_idma1_page = 0x07,
cpm_idma2_page = 0x08,
cpm_idma3_page = 0x09,
cpm_idma4_page = 0x0a,
cpm_mcc1_page = 0x07,
cpm_mcc2_page = 0x08,
};
enum {
InitRxTx = 0,
InitRx = 1,
InitTx = 2,
EnterHunt= 3,
StopTx= 4,
GracefulStopTx = 5,
InitIDMA = 5,
RestartTx = 6,
CloseRxBD = 7,
SetGroupAddr = 8,
SetTimer = 8,
GCITimeout = 9,
GCIAbort = 10,
StopIDMA = 11,
StartDSP = 12,
ArmIDMA = 13,
InitDSP = 13,
USBCmd = 15,
SCC1ID= cpm_scc1_page << 5 | cpm_scc1_sblock,
SCC2ID= cpm_scc2_page << 5 | cpm_scc2_sblock,
SCC3ID= cpm_scc3_page << 5 | cpm_scc3_sblock,
SMC1ID= cpm_smc1_page << 5 | cpm_smc1_sblock,
SMC2ID= cpm_smc2_page << 5 | cpm_smc2_sblock,
FCC1ID= cpm_fcc1_page << 5 | cpm_fcc1_sblock,
FCC2ID= cpm_fcc2_page << 5 | cpm_fcc2_sblock,
FCC3ID= cpm_fcc3_page << 5 | cpm_fcc3_sblock,
BRG1 = 0,
BRG2 = 1,
BRG3 = 2,
BRG4 = 4,
CLK1 = 4,
CLK2 = 5,
CLK3 = 6,
CLK4 = 7,
};
extern IMM* iomem;
BD* bdalloc(int);
void cpmop(int, int, int);
void ioplock(void);
void iopunlock(void);
void kreboot(ulong);