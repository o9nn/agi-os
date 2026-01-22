#define MJPG_VERSION "LML33 v0.2"
#define NLML 2
#define I2C_DELAY 50
#define I2C_TIMEOUT 10000000
#define GUEST_TIMEOUT 10000000
#define STILL_TIMEOUT 1000000
#define MAX_CARDS 1
#define I2C_BUS		0x044
#define I2C_SCL		1
#define I2C_SDA		2
#define INTR_JPEGREP	0x08000000
#define INTR_GIRQ0	0x20000000
#define INTR_STAT	0x03c
typedef struct {
int	number;
char	*card_name;
int	zr060addr;
} Device;
#define VENDOR_ZORAN		0x11de
#define ZORAN_36057		0x6057
#define ZORAN_36067		ZORAN_36057
#define BT819Addr 0x8a
#define BT856Addr 0x88
#define NBUF 4
#define FRAGM_FINAL_B 1
#define STAT_BIT 1
typedef struct	HdrFragment		HdrFragment;
typedef struct	FrameHeader		FrameHeader;
typedef union	Fragment		Fragment;
typedef struct	FragmentTable		FragmentTable;
typedef struct	CodeData		CodeData;
#define MRK_SOI		0xD8FF
#define MRK_APP3	0xE3FF
#define APP_NAME	"LML"
struct FrameHeader {
ushort	mrkSOI;
ushort	mrkAPP3;
ushort	lenAPP3;
char	nm[4];
ushort	frameNo;
vlong	ftime;
ulong	frameSize;
ushort	frameSeqNo;
ushort	SOIfiller;
};
#define FRAGSIZE (128*1024)
union Fragment {
FrameHeader fh;
char	fb[FRAGSIZE];
};
struct HdrFragment {
uchar	hdr[sizeof(FrameHeader)];
Fragment;
};
struct FragmentTable {
ulong	addr;
ulong	leng;
};
struct CodeData {
ulong	pamjpg;
ulong	pagrab;
ulong	statCom[4];
FragmentTable fragdesc[4];
HdrFragment frag[4];
};
enum{
Codedatasize = (sizeof(CodeData) + BY2PG - 1) & ~(BY2PG - 1),
Grabdatasize = (730 * 568 * 2 * 2 + BY2PG - 1) & ~(BY2PG - 1),
};
#define POST_OFFICE		0x200
#define POST_PEND		0x02000000
#define POST_TIME		0x01000000
#define POST_DIR		0x00800000
#define GID060	0