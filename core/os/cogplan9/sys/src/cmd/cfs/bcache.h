typedef struct Bbuf	Bbuf;
typedef struct Bcache	Bcache;
enum
{
Nbcache=	32,
};
struct Bbuf
{
Lru;
ulong	bno;
int	inuse;
Bbuf	*next;
int	dirty;
char	*data;
};
struct Bcache
{
Lru;
int	bsize;
int	f;
Bbuf	*dfirst;
Bbuf	*dlast;
Bbuf	bb[Nbcache];
};
int	bcinit(Bcache*, int, int);
Bbuf*	bcalloc(Bcache*, ulong);
Bbuf*	bcread(Bcache*, ulong);
void	bcmark(Bcache*, Bbuf*);
int	bcwrite(Bcache*, Bbuf*);
int	bcsync(Bcache*);
int	bread(Bcache*, ulong, void*);
int	bwrite(Bcache*, ulong, void*);
int	bref(Bcache*, Bbuf*);
void	error(char*, ...);
void	warning(char*);