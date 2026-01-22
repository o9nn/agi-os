typedef struct Dosboot	Dosboot;
typedef struct Dos	Dos;
typedef struct Dosdir	Dosdir;
typedef struct Dosfile	Dosfile;
typedef struct Dospart	Dospart;
struct Dospart
{
uchar flag;
uchar shead;
uchar scs[2];
uchar type;
uchar ehead;
uchar ecs[2];
uchar start[4];
uchar len[4];
};
#define FAT12	0x01
#define FAT16	0x04
#define FATHUGE	0x06
#define DMDDO	0x54
struct Dosboot{
uchar	magic[3];
uchar	version[8];
uchar	sectsize[2];
uchar	clustsize;
uchar	nresrv[2];
uchar	nfats;
uchar	rootsize[2];
uchar	volsize[2];
uchar	mediadesc;
uchar	fatsize[2];
uchar	trksize[2];
uchar	nheads[2];
uchar	nhidden[4];
uchar	bigvolsize[4];
uchar	driveno;
uchar	reserved0;
uchar	bootsig;
uchar	volid[4];
uchar	label[11];
uchar	reserved1[8];
};
struct Dosfile{
Dos	*dos;
char	name[8];
char	ext[3];
uchar	attr;
long	length;
long	pstart;
long	pcurrent;
long	lcurrent;
long	offset;
};
struct Dos{
int	dev;
long	(*read)(int, void*, long);
long	(*seek)(int, long);
int	start;
int	sectsize;
int	clustsize;
int	clustbytes;
int	nresrv;
int	nfats;
int	rootsize;
int	volsize;
int	mediadesc;
int	fatsize;
int	fatclusters;
int	fatbits;
long	fataddr;
long	rootaddr;
long	dataaddr;
long	freeptr;
Dosfile	root;
};
struct Dosdir{
uchar	name[8];
uchar	ext[3];
uchar	attr;
uchar	reserved[10];
uchar	time[2];
uchar	date[2];
uchar	start[2];
uchar	length[4];
};
#define	DRONLY	0x01
#define	DHIDDEN	0x02
#define	DSYSTEM	0x04
#define	DVLABEL	0x08
#define	DDIR	0x10
#define	DARCH	0x20
extern int chatty;
extern int dosboot(Dos*, char*);
extern int dosinit(Dos*, int, int);
extern long dosread(Dosfile*, void*, long);
extern int dosstat(Dos*, char*, Dosfile*);
extern int doswalk(Dosfile*, char*);
extern int plan9ini(Dos*, char*);