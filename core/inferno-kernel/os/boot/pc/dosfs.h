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
#define EXTEND	0x05
#define FATHUGE	0x06
#define FAT32	0x0b
#define FAT32X	0x0c
#define EXTHUGE	0x0f
#define DMDDO	0x54
#define PLAN9	0x39
#define LEXTEND 0x85
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
long	start;
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
long	rootclust;
long	dataaddr;
long	freeptr;
};
extern int	dosinit(Fs*);