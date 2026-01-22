typedef struct Umsc Umsc;
#pragma incomplete Umsc
enum {
NTargetID = 8,
CtlrID = 7,
MaxDirData = 255,
LBsize = 512,
};
typedef struct {
uchar *p;
long count;
uchar write;
} ScsiPtr;
typedef struct {
int flags;
char *unit;
int lun;
ulong lbsize;
ulong offset;
int fd;
Umsc *umsc;
ScsiPtr cmd;
ScsiPtr data;
int status;
uchar sense[MaxDirData];
uchar inquiry[MaxDirData];
int readblock;
} ScsiReq;
enum {
Fopen = 0x0001,
Fseqdev = 0x0002,
Fwritten = 0x0004,
Fronly = 0x0008,
Fwormdev = 0x0010,
Fprintdev = 0x0020,
Fbfixed = 0x0040,
Fchanger = 0x0080,
Finqok = 0x0100,
Fmode6 = 0x0200,
Frw10 = 0x0400,
Fusb = 0x0800,
};
enum {
STnomem =-4,
STharderr =-3,
STtimeout =-2,
STok = 0,
STcheck = 0x02,
STcondmet = 0x04,
STbusy = 0x08,
STintok = 0x10,
STintcondmet = 0x14,
STresconf = 0x18,
STterminated = 0x22,
STqfull = 0x28,
};
enum {
Status_SD = 0x80,
Status_SW = 0x83,
Status_BADARG = 0x84,
Status_RO = 0x85,
};
enum {
Sd0valid = 0x80,
Sd2ili = 0x20,
Sd2eom = 0x40,
Sd2filemark = 0x80,
Cmd1fixed = 1,
Cmd1sili = 2,
Max24off = (1<<21) - 1,
Allmodepages = 0x3F,
};
#define GETBELONG(p) ((ulong)(p)[0]<<24 | (ulong)(p)[1]<<16 | (p)[2]<<8 | (p)[3])
#define PUTBELONG(p, ul) ((p)[0] = (ul)>>24, (p)[1] = (ul)>>16, \
(p)[2] = (ul)>>8, (p)[3] = (ul))
#define GETBE24(p) ((ulong)(p)[0]<<16 | (p)[1]<<8 | (p)[2])
#define PUTBE24(p, ul) ((p)[0] = (ul)>>16, (p)[1] = (ul)>>8, (p)[2] = (ul))
extern long maxiosize;
long SRready(ScsiReq*);
long SRrewind(ScsiReq*);
long SRreqsense(ScsiReq*);
long SRformat(ScsiReq*);
long SRrblimits(ScsiReq*, uchar*);
long SRread(ScsiReq*, void*, long);
long SRwrite(ScsiReq*, void*, long);
long SRseek(ScsiReq*, long, int);
long SRfilemark(ScsiReq*, ulong);
long SRspace(ScsiReq*, uchar, long);
long SRinquiry(ScsiReq*);
long SRmodeselect6(ScsiReq*, uchar*, long);
long SRmodeselect10(ScsiReq*, uchar*, long);
long SRmodesense6(ScsiReq*, uchar, uchar*, long);
long SRmodesense10(ScsiReq*, uchar, uchar*, long);
long SRstart(ScsiReq*, uchar);
long SRrcapacity(ScsiReq*, uchar*);
long SRblank(ScsiReq*, uchar, uchar);
long SRsynccache(ScsiReq*);
long SRTOC(ScsiReq*, void*, int, uchar, uchar);
long SRrdiscinfo(ScsiReq*, void*, int);
long SRrtrackinfo(ScsiReq*, void*, int, int);
long SRcdpause(ScsiReq*, int);
long SRcdstop(ScsiReq*);
long SRcdload(ScsiReq*, int, int);
long SRcdplay(ScsiReq*, int, long, long);
long SRcdstatus(ScsiReq*, uchar*, int);
long SRgetconf(ScsiReq*, uchar*, int);
long SRfwaddr(ScsiReq*, uchar, uchar, uchar, uchar*);
long SRtreserve(ScsiReq*, long);
long SRtinfo(ScsiReq*, uchar, uchar*);
long SRwtrack(ScsiReq*, void*, long, uchar, uchar);
long SRmload(ScsiReq*, uchar);
long SRfixation(ScsiReq*, uchar);
long SReinitialise(ScsiReq*);
long SRestatus(ScsiReq*, uchar, uchar*, int);
long SRmmove(ScsiReq*, int, int, int, int);
long SRrequest(ScsiReq*);
int SRclose(ScsiReq*);
int SRopenraw(ScsiReq*, char*);
int SRopen(ScsiReq*, char*);
void makesense(ScsiReq*);
long umsrequest(struct Umsc*, ScsiPtr*, ScsiPtr*, int*);