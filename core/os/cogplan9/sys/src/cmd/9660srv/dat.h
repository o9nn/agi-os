typedef	struct Ioclust	Ioclust;
typedef	struct Iobuf	Iobuf;
typedef	struct Isofile	Isofile;
typedef struct Xdata	Xdata;
typedef struct Xfile	Xfile;
typedef struct Xfs	Xfs;
typedef struct Xfsub	Xfsub;
#pragma incomplete Isofile
enum
{
Sectorsize = 2048,
Maxname = 256,
};
struct Iobuf
{
Ioclust* clust;
long	addr;
uchar*	iobuf;
};
struct Ioclust
{
long	addr;
Xdata*	dev;
Ioclust* next;
Ioclust* prev;
int	busy;
int	nbuf;
Iobuf*	buf;
uchar*	iobuf;
};
struct Xdata
{
Xdata*	next;
char*	name;
Qid	qid;
short	type;
short	fdev;
int	ref;
int	dev;
};
struct Xfsub
{
void	(*reset)(void);
int	(*attach)(Xfile*);
void	(*clone)(Xfile*, Xfile*);
void	(*walkup)(Xfile*);
void	(*walk)(Xfile*, char*);
void	(*open)(Xfile*, int);
void	(*create)(Xfile*, char*, long, int);
long	(*readdir)(Xfile*, uchar*, long, long);
long	(*read)(Xfile*, char*, vlong, long);
long	(*write)(Xfile*, char*, vlong, long);
void	(*clunk)(Xfile*);
void	(*remove)(Xfile*);
void	(*stat)(Xfile*, Dir*);
void	(*wstat)(Xfile*, Dir*);
};
struct Xfs
{
Xdata*	d;
Xfsub*	s;
int	ref;
int	issusp;
long	suspoff;
int	isrock;
int	isplan9;
Qid	rootqid;
Isofile*	ptr;
};
struct Xfile
{
Xfile*	next;
Xfs*	xf;
long	fid;
ulong	flags;
Qid	qid;
int	len;
Isofile*	ptr;
};
enum
{
Asis,
Clean,
Clunk
};
enum
{
Oread = 1,
Owrite = 2,
Orclose = 4,
Omodes = 3,
};
extern char	Enonexist[];
extern char	Eperm[];
extern char	Enofile[];
extern char	Eauth[];
extern char	*srvname;
extern char	*deffile;
extern int	chatty;
extern jmp_buf	err_lab[];
extern int	nerr_lab;
extern char	err_msg[];
extern int nojoliet;
extern int noplan9;
extern int norock;