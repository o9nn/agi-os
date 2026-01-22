typedef struct ZipHead	ZipHead;
enum
{
ZHeader		= 0x04034b50,
ZCHeader	= 0x02014b50,
ZECHeader	= 0x06054b50,
ZEncrypted	= 1 << 0,
ZTrailInfo	= 1 << 3,
ZCompPatch	= 1 << 5,
ZCrcPoly	= 0xedb88320,
ZDeflate	= 8,
ZIsText		= 1 << 0,
ZDos		= 0,
ZAmiga		= 1,
ZVMS		= 2,
ZUnix		= 3,
ZVMCMS		= 4,
ZAtariST	= 5,
ZOS2HPFS	= 6,
ZMac		= 7,
ZZsys		= 8,
ZCPM		= 9,
ZNtfs		= 10,
ZDROnly		= 0x01,
ZDHidden	= 0x02,
ZDSystem	= 0x04,
ZDVLable	= 0x08,
ZDDir		= 0x10,
ZDArch		= 0x20,
ZHeadSize	= 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2,
ZHeadCrc	= 4 + 2 + 2 + 2 + 2 + 2,
ZTrailSize	= 4 + 4 + 4,
ZCHeadSize	= 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4,
ZECHeadSize	= 4 + 2 + 2 + 2 + 2 + 4 + 4 + 2,
};
struct ZipHead
{
int	madeos;
int	madevers;
int	extos;
int	extvers;
int	flags;
int	meth;
int	modtime;
int	moddate;
ulong	crc;
ulong	csize;
ulong	uncsize;
int	iattr;
ulong	eattr;
ulong	off;
char	*file;
};