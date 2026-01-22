typedef struct Qid9p1 Qid9p1;
typedef struct Dentry Dentry;
typedef struct Kfsfile Kfsfile;
typedef struct Kfs Kfs;
struct	Qid9p1
{
long	path;
long	version;
};
#define	NDBLOCK		6
struct	Dentry
{
char	name[NAMELEN];
short	uid;
short	gid;
ushort	mode;
Qid9p1	qid;
long	size;
long	dblock[NDBLOCK];
long	iblock;
long	diblock;
long	atime;
long	mtime;
};
struct Kfsfile
{
Dentry;
long off;
};
struct Kfs
{
int	RBUFSIZE;
int	BUFSIZE;
int	DIRPERBUF;
int	INDPERBUF;
int	INDPERBUF2;
};
extern int kfsinit(Fs*);