typedef struct Hub Hub;
typedef struct Port Port;
typedef struct DHub DHub;
typedef struct Devtab Devtab;
typedef struct Usbfs Usbfs;
enum
{
Stack = 32*1024,
Dhub = 0x29,
Dhublen = 9,
Fhublocalpower = 0,
Fhubovercurrent = 1,
Fportconnection = 0,
Fportenable = 1,
Fportsuspend = 2,
Fportovercurrent = 3,
Fportreset = 4,
Fportpower = 8,
Fportlowspeed = 9,
Fcportconnection = 16,
Fcportenable = 17,
Fcportsuspend = 18,
Fcportovercurrent= 19,
Fcportreset = 20,
Fportindicator = 22,
PSpresent = 0x0001,
PSenable = 0x0002,
PSsuspend = 0x0004,
PSovercurrent = 0x0008,
PSreset = 0x0010,
PSpower = 0x0100,
PSslow = 0x0200,
PShigh = 0x0400,
PSstatuschg = 0x10000,
PSchange = 0x20000,
Pdisabled = 0,
Pattached,
Pconfiged,
Spawndelay = 100,
Connectdelay = 500,
Resetdelay = 20,
Enabledelay = 20,
Powerdelay = 100,
Pollms = 250,
Chgdelay = 100,
Chgtmout = 1000,
DCL = 0x01000000,
DSC = 0x02000000,
DPT = 0x04000000,
};
struct Hub
{
uchar pwrmode;
uchar compound;
uchar pwrms;
uchar maxcurrent;
int leds;
int maxpkt;
uchar nport;
Port *port;
int failed;
int isroot;
Dev *dev;
Hub *next;
};
struct Port
{
int state;
int sts;
uchar removable;
uchar pwrctl;
Dev *dev;
Hub *hub;
int devnb;
uvlong *devmaskp;
};
struct DHub
{
uchar bLength;
uchar bDescriptorType;
uchar bNbrPorts;
uchar wHubCharacteristics[2];
uchar bPwrOn2PwrGood;
uchar bHubContrCurrent;
uchar DeviceRemovable[1];
};
struct Devtab
{
char *name;
int (*init)(Dev*, int, char**);
int csps[4];
int vid;
int did;
char *args;
uvlong devmask;
int noauto;
};
Hub* newhub(char *fn, Dev *d);
int startdev(Port *pp);
int getdevnb(uvlong *maskp);
void putdevnb(uvlong *maskp, int nb);
void threadmain(int argc, char **argv);
extern Usbfs usbdfsops;