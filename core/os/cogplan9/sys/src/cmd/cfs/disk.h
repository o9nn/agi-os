typedef struct Disk	Disk;
struct Disk
{
Bcache;
ulong	nb;
ulong	nab;
int	b2b;
int	p2b;
char	name[CACHENAMELEN];
};
int	dinit(Disk*, int, int, char*);
int	dformat(Disk*, int, char*, ulong, ulong);
ulong	dalloc(Disk*, Dptr*);
ulong	dpalloc(Disk*, Dptr*);
int	dfree(Disk*, Dptr*);
extern int debug;
#define DPRINT if(debug)fprint