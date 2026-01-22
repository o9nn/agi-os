#define DEBUG if(1){}else print
#define SEEKEYS 0
enum
{
WTyp_Stats = 0xf100,
WTyp_Scan = 0xf101,
WTyp_Link = 0xf200,
WTyp_Ptype = 0xfc00,
WTyp_Mac = 0xfc01,
WTyp_WantName = 0xfc02,
WTyp_Chan = 0xfc03,
WTyp_NetName = 0xfc04,
WTyp_ApDens = 0xfc06,
WTyp_MaxLen = 0xfc07,
WTyp_PM = 0xfc09,
WTyp_PMWait = 0xfc0c,
WTyp_NodeName = 0xfc0e,
WTyp_Crypt = 0xfc20,
WTyp_XClear = 0xfc22,
WTyp_CreateIBSS = 0xfc81,
WTyp_RtsThres = 0xfc83,
WTyp_TxRate = 0xfc84,
WTx1Mbps = 0x0,
WTx2Mbps = 0x1,
WTxAuto = 0x3,
WTyp_Prom = 0xfc85,
WTyp_Keys = 0xfcb0,
WTyp_TxKey = 0xfcb1,
WTyp_StationID = 0xfd20,
WTyp_CurName = 0xfd41,
WTyp_BaseID = 0xfd42,
WTyp_CurTxRate = 0xfd44,
WTyp_HasCrypt = 0xfd4f,
WTyp_Tick = 0xfce0,
};
enum
{
WDfltIRQ = 3,
WDfltIOB = 0x180,
WIOLen = 0x40,
WTmOut = 65536,
WPTypeManaged = 1,
WPTypeWDS = 2,
WPTypeAdHoc = 3,
WDfltPType = WPTypeManaged,
WDfltApDens = 1,
WDfltRtsThres = 2347,
WDfltTxRate = WTxAuto,
WMaxLen = 2304,
WNameLen = 32,
WNKeys = 4,
WKeyLen = 14,
WMinKeyLen = 5,
WMaxKeyLen = 13,
WR_Cmd = 0x00,
WCmdIni = 0x0000,
WCmdEna = 0x0001,
WCmdDis = 0x0002,
WCmdTx = 0x000b,
WCmdMalloc = 0x000a,
WCmdEnquire = 0x0011,
WCmdMsk = 0x003f,
WCmdAccRd = 0x0021,
WCmdReclaim = 0x0100,
WCmdAccWr = 0x0121,
WCmdBusy = 0x8000,
WR_Parm0 = 0x02,
WR_Parm1 = 0x04,
WR_Parm2 = 0x06,
WR_Sts = 0x08,
WR_InfoId = 0x10,
WR_Sel0 = 0x18,
WR_Sel1 = 0x1a,
WR_Off0 = 0x1c,
WR_Off1 = 0x1e,
WBusyOff = 0x8000,
WErrOff = 0x4000,
WResSts = 0x7f00,
WR_RXId = 0x20,
WR_Alloc = 0x22,
WR_EvSts = 0x30,
WR_IntEna = 0x32,
WCmdEv = 0x0010,
WRXEv = 0x0001,
WTXEv = 0x0002,
WTxErrEv = 0x0004,
WAllocEv = 0x0008,
WInfoEv = 0x0080,
WIDropEv = 0x2000,
WTickEv = 0x8000,
WEvs = WRXEv|WTXEv|WAllocEv|WInfoEv|WIDropEv,
WR_EvAck = 0x34,
WR_Data0 = 0x36,
WR_Data1 = 0x38,
WR_PciCor = 0x26,
WR_PciHcr = 0x2E,
WF_Err = 0x0003,
WF_1042 = 0x2000,
WF_Tunnel = 0x4000,
WF_WMP = 0x6000,
WF_Data = 0x0008,
WSnapK1 = 0xaa,
WSnapK2 = 0x00,
WSnapCtlr = 0x03,
WSnap0 = (WSnapK1|(WSnapK1<<8)),
WSnap1 = (WSnapK2|(WSnapCtlr<<8)),
WSnapHdrLen = 6,
WF_802_11_Off = 0x44,
WF_802_3_Off = 0x2e,
};
typedef struct Ctlr Ctlr;
typedef struct Wltv Wltv;
typedef struct WFrame WFrame;
typedef struct Stats Stats;
typedef struct WStats WStats;
typedef struct WScan WScan;
typedef struct WKey WKey;
struct WStats
{
ulong ntxuframes;
ulong ntxmframes;
ulong ntxfrags;
ulong ntxubytes;
ulong ntxmbytes;
ulong ntxdeferred;
ulong ntxsretries;
ulong ntxmultiretries;
ulong ntxretrylimit;
ulong ntxdiscards;
ulong nrxuframes;
ulong nrxmframes;
ulong nrxfrags;
ulong nrxubytes;
ulong nrxmbytes;
ulong nrxfcserr;
ulong nrxdropnobuf;
ulong nrxdropnosa;
ulong nrxcantdecrypt;
ulong nrxmsgfrag;
ulong nrxmsgbadfrag;
ulong end;
};
struct WScan
{
ushort chan;
ushort noise;
ushort signal;
uchar bssid[Eaddrlen];
ushort interval;
ushort capinfo;
ushort ssid_len;
char ssid[WNameLen];
};
struct WFrame
{
ushort sts;
ushort rsvd0;
ushort rsvd1;
ushort qinfo;
ushort rsvd2;
ushort rsvd3;
ushort txctl;
ushort framectl;
ushort id;
uchar addr1[Eaddrlen];
uchar addr2[Eaddrlen];
uchar addr3[Eaddrlen];
ushort seqctl;
uchar addr4[Eaddrlen];
ushort dlen;
uchar dstaddr[Eaddrlen];
uchar srcaddr[Eaddrlen];
ushort len;
ushort dat[3];
ushort type;
};
struct WKey
{
ushort len;
char dat[WKeyLen];
};
struct Wltv
{
ushort len;
ushort type;
union
{
struct {
ushort val;
ushort pad;
};
struct {
uchar addr[8];
};
struct {
ushort slen;
char s[WNameLen];
};
struct {
char name[WNameLen];
};
struct {
WKey keys[WNKeys];
};
};
};
struct Stats
{
ulong nints;
ulong ndoubleint;
ulong nrx;
ulong ntx;
ulong ntxrq;
ulong nrxerr;
ulong ntxerr;
ulong nalloc;
ulong ninfo;
ulong nidrop;
ulong nwatchdogs;
int ticks;
int tickintr;
int signal;
int noise;
};
enum {
Attached = 0x01,
Power = 0x02,
};
struct Ctlr
{
Lock;
int state;
int slot;
int iob;
int createibss;
int ptype;
int apdensity;
int rtsthres;
int txbusy;
int txrate;
int txdid;
int txmid;
int txtmout;
int maxlen;
int chan;
int pmena;
int pmwait;
Proc *timerproc;
int scanticks;
char netname[WNameLen];
char wantname[WNameLen];
char nodename[WNameLen];
WFrame txf;
uchar txbuf[1536];
int hascrypt;
int crypt;
int txkey;
Wltv keys;
int xclear;
int ctlrno;
ushort *mmb;
Ctlr *next;
int active;
Pcidev *pcidev;
Stats;
WStats;
};
extern char* wavenames[];
void csr_outs(Ctlr*, int, ushort);
ushort csr_ins(Ctlr*, int);
void w_intdis(Ctlr*);
int w_cmd(Ctlr *, ushort, ushort);
void ltv_outs(Ctlr*, int, ushort);
int ltv_ins(Ctlr*, int);
int w_option(Ctlr*, char*, long);
int w_inltv(Ctlr*, Wltv*);
void w_attach(Ether*);
void w_interrupt(Ureg*,void*);
void w_transmit(Ether*);
long w_ifstat(Ether*, void*, long, ulong);
long w_ctl(Ether*, void*, long);
void w_promiscuous(void*, int);
void w_multicast(void*, uchar*, int);
int wavelanreset(Ether*, Ctlr*);