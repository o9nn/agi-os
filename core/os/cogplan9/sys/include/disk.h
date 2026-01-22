#pragma src "/sys/src/libdisk"
#pragma lib "libdisk.a"
typedef struct Scsi Scsi;
struct Scsi {
QLock;
char*	inquire;
int	rawfd;
int	nchange;
ulong	changetime;
};
enum {
Sread = 0,
Swrite,
Snone,
};
char*	scsierror(int, int);
int		scsicmd(Scsi*, uchar*, int, void*, int, int);
int		scsi(Scsi*, uchar*, int, void*, int, int);
Scsi*		openscsi(char*);
void		closescsi(Scsi*);
int		scsiready(Scsi*);
extern int		scsiverbose;
typedef struct Disk Disk;
struct Disk {
char *prefix;
char *part;
int fd;
int wfd;
int ctlfd;
int rdonly;
int type;
vlong secs;
vlong secsize;
vlong size;
vlong offset;
int width;
int c;
int h;
int s;
int chssrc;
};
Disk*	opendisk(char*, int, int);
enum {
Tfile = 0,
Tsd,
Tfloppy,
Gpart = 0,
Gdisk,
Gguess,
};
enum {
ScmdTur		= 0x00,
ScmdRewind	= 0x01,
ScmdRsense	= 0x03,
ScmdFormat	= 0x04,
ScmdRblimits	= 0x05,
ScmdRead	= 0x08,
ScmdWrite	= 0x0A,
ScmdSeek	= 0x0B,
ScmdFmark	= 0x10,
ScmdSpace	= 0x11,
ScmdInq		= 0x12,
ScmdMselect6	= 0x15,
ScmdMselect10	= 0x55,
ScmdMsense6	= 0x1A,
ScmdMsense10	= 0x5A,
ScmdStart	= 0x1B,
ScmdRcapacity	= 0x25,
ScmdRcapacity16	= 0x9e,
ScmdRformatcap	= 0x23,
ScmdExtread	= 0x28,
ScmdRead16	= 0x88,
ScmdExtwrite	= 0x2A,
ScmdExtwritever = 0x2E,
ScmdWrite16	= 0x8A,
ScmdExtseek	= 0x2B,
ScmdSynccache	= 0x35,
ScmdRTOC	= 0x43,
ScmdRdiscinfo	= 0x51,
ScmdRtrackinfo	= 0x52,
ScmdReserve	= 0x53,
ScmdBlank	= 0xA1,
ScmdCDpause	= 0x4B,
ScmdCDstop	= 0x4E,
ScmdCDplay	= 0xA5,
ScmdCDload	= 0xA6,
ScmdCDscan	= 0xBA,
ScmdCDstatus	= 0xBD,
Scmdgetconf	= 0x46,
ScmdEInitialise	= 0x07,
ScmdMMove	= 0xA5,
ScmdEStatus	= 0xB8,
ScmdMExchange	= 0xA6,
ScmdEposition	= 0x2B,
ScmdReadDVD	= 0xAD,
ScmdReportKey	= 0xA4,
ScmdSendKey	= 0xA3,
ScmdClosetracksess= 0x5B,
ScmdRead12	= 0xA8,
ScmdSetcdspeed	= 0xBB,
ScmdReadcd	= 0xBE,
ScmdFwaddr	= 0xE2,
ScmdTreserve	= 0xE4,
ScmdTinfo	= 0xE5,
ScmdTwrite	= 0xE6,
ScmdMload	= 0xE7,
ScmdFixation	= 0xE9,
};
typedef void Protoenum(char *new, char *old, Dir *d, void *a);
typedef void Protowarn(char *msg, void *a);
int rdproto(char*, char*, Protoenum*, Protowarn*, void*);