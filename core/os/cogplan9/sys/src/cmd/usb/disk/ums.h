typedef struct Umsc Umsc;
typedef struct Ums Ums;
typedef struct Cbw Cbw;
typedef struct Csw Csw;
enum
{
Protocbi =	0,
Protocb =	1,
Protobulk =	0x50,
Subrbc =	1,
Subatapi =	2,
Subqic 	=	3,
Subufi =	4,
Sub8070 =	5,
Subscsi =	6,
Subisd200 =	7,
Subdev =	0xff,
Umsreset =	0xFF,
Getmaxlun =	0xFE,
Maxlun		= 32,
CMreset = 1,
Pcmd = 0,
Pdata,
Pstatus,
CbwLen		= 31,
CbwDataIn	= 0x80,
CbwDataOut	= 0x00,
CswLen		= 13,
CswOk		= 0,
CswFailed	= 1,
CswPhaseErr	= 2,
};
struct Umsc
{
ScsiReq;
uvlong	blocks;
vlong	capacity;
char	*bufp;
long	off;
long	nb;
uchar 	rawcmd[16];
uchar	phase;
char	*inq;
Ums	*ums;
Usbfs	fs;
char	buf[Maxiosize];
};
struct Ums
{
QLock;
Dev	*dev;
Dev	*epin;
Dev	*epout;
Umsc	*lun;
uchar	maxlun;
int	seq;
int	nerrs;
int	wrongresidues;
};
struct Cbw
{
char	signature[4];
long	tag;
long	datalen;
uchar	flags;
uchar	lun;
uchar	len;
char	command[16];
};
struct Csw
{
char	signature[4];
long	tag;
long	dataresidue;
uchar	status;
};
int	diskmain(Dev*, int, char**);