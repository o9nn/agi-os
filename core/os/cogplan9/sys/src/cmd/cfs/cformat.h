typedef struct Dptr	Dptr;
typedef struct Dahdr	Dahdr;
typedef struct Dalloc	Dalloc;
typedef struct Fphdr	Fphdr;
typedef struct Fptr	Fptr;
typedef struct Inode	Inode;
typedef struct Dihdr	Dihdr;
typedef struct Dinode	Dinode;
enum
{
Amagic= 	0xbebeefed,
Imagic=		0xbadc00ce,
BtoUL=		8*sizeof(ulong),
CACHENAMELEN=	128
};
#define	Indbno		0x80000000
#define	Notabno		0xFFFFFFFF
struct Dahdr
{
ulong	magic;
ulong	bsize;
char	name[CACHENAMELEN];
short	nab;
};
struct Dalloc
{
Dahdr;
ulong	bits[1];
};
struct Dptr
{
ulong	fbno;
ulong	bno;
ushort	start;
ushort	end;
};
struct Inode
{
Qid	qid;
vlong	length;
Dptr	ptr;
char	inuse;
};
struct Dihdr
{
ulong	magic;
ulong	nino;
};
struct Dinode
{
Dihdr;
Inode	inode[1];
};