typedef struct Ibuf	Ibuf;
typedef struct Imap	Imap;
typedef struct Icache	Icache;
enum
{
Nicache=	64,
};
struct Ibuf
{
Lru;
int	inuse;
ulong	ino;
Inode	inode;
};
struct Imap
{
Lru;
Qid	qid;
Ibuf	*b;
int	inuse;
};
struct Icache
{
Disk;
int	nino;
ulong	ib0;
int	nib;
int	i2b;
Ibuf	ib[Nicache];
Lru	blru;
Imap	*map;
Lru	mlru;
};
Ibuf*	ialloc(Icache*, ulong);
Ibuf*	iget(Icache*, Qid);
Ibuf*	iread(Icache*, ulong);
int	iformat(Icache*, int, ulong, char*, int, int);
int	iinit(Icache*, int, int, char*);
int	iremove(Icache*, ulong);
int	iupdate(Icache*, ulong, Qid);
int	iwrite(Icache*, Ibuf*);
void	ifree(Icache*, Ibuf*);
void	iinc(Icache*, Ibuf*);