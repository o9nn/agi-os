typedef	struct	Chan	Chan;
typedef struct	Command	Command;
typedef	struct	Conf	Conf;
typedef	struct	Cons	Cons;
typedef struct	Devcall	Devcall;
#define MAXBUFSIZE	(16*1024)
#include "portdat.h"
struct	Chan
{
int	chan;
QLock rlock, wlock;
int	type;
int	flags;
long	whotime;
File*	flist;
Lock	flock;
RWLock	reflock;
int	msize;
int	authed;
uchar	chal[8];
uchar	rchal[8];
int	idoffset;
int	idvec;
Lock	idlock;
};
enum
{
Fchat	= (1<<0),
Fuid	= (1<<2),
};
struct	Cons
{
int	flags;
int	uid;
int	gid;
int	allow;
long	offset;
char*	arg;
Chan	*chan;
Chan	*srvchan;
Filter	work;
Filter	rate;
Filter	bhit;
Filter	bread;
Filter	binit;
Filter	tags[MAXTAG];
};
struct	Conf
{
ulong	niobuf;
ulong	nuid;
ulong	uidspace;
ulong	gidspace;
ulong	nserve;
ulong	nfile;
ulong	nwpath;
ulong	bootsize;
};
struct	Command
{
char	*string;
void	(*func)(void);
char	*args;
};
struct Devcall
{
void	(*init)(Device);
void	(*ream)(Device);
int	(*check)(Device);
long	(*super)(Device);
long	(*root)(Device);
long	(*size)(Device);
int	(*read)(Device, long, void*);
int	(*write)(Device, long, void*);
};
enum
{
Devnone 	= 0,
Devwren,
MAXDEV
};
enum
{
MAXFILSYS = 4
};
#define	QPDIR	0x80000000L
#define	QPNONE	0
#define	QPROOT	1
#define	QPSUPER	2
#define	PDIR	(1L<<31)
#define	PAPND	(1L<<30)
#define	PLOCK	(1L<<29)
#define	NOF	(-1)
#define	FID1		1
#define	FID2		2
#define	FID3		3
#define SECOND(n) 	(n)
#define MINUTE(n)	(n*SECOND(60))
#define HOUR(n)		(n*MINUTE(60))
#define DAY(n)		(n*HOUR(24))
#define	TLOCK		MINUTE(5)
#define	CHAT(cp)	(chat)
#define	QID9P1(a,b)	(Qid9p1){a,b}
extern	Uid*	uid;
extern	char*	uidspace;
extern	short*	gidspace;
extern	char*	errstring[MAXERR];
extern	Chan*	chans;
extern	RWLock	mainlock;
extern	long	boottime;
extern	Tlock	*tlocks;
extern	Device	devnone;
extern	Filsys	filesys[];
extern	char	service[];
extern	char*	tagnames[];
extern	Conf	conf;
extern	Cons	cons;
extern	Command	command[];
extern	Chan	*chan;
extern	Devcall	devcall[];
extern	char	*progname;
extern	char	*procname;
extern	long	niob;
extern	long	nhiob;
extern	Hiob	*hiob;
extern	int	chat;
extern	int	writeallow;
extern	int	wstatallow;
extern	int	allownone;
extern	int	noatime;
extern	int	writegroup;
extern Lock wpathlock;