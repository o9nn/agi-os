#include <stdarg.h>
#pragma	src	"/usr/inferno/libbio"
typedef	struct	Biobuf	Biobuf;
enum
{
Bsize		= 8*1024,
Bungetsize	= UTFmax+1,
Bmagic		= 0x314159,
Beof		= -1,
Bbad		= -2,
Binactive	= 0,
Bractive,
Bwactive,
Bracteof,
Bend
};
struct	Biobuf
{
int	icount;
int	ocount;
int	rdline;
int	runesize;
int	state;
int	fid;
int	flag;
vlong	offset;
int	bsize;
uchar*	bbuf;
uchar*	ebuf;
uchar*	gbuf;
uchar	b[Bungetsize+Bsize];
};
#define	BGETC(bp)	Bgetc(bp)
#define	BPUTC(bp,c)	Bputc(bp,c)
#define	BOFFSET(bp)	Boffset(bp)
#define	BLINELEN(bp)	Blinelen(bp)
#define	BFILDES(bp)	Bfildes(bp)
int	Bbuffered(Biobuf*);
int	Bfildes(Biobuf*);
int	Bflush(Biobuf*);
int	Bgetc(Biobuf*);
int	Bgetd(Biobuf*, double*);
long	Bgetrune(Biobuf*);
int	Binit(Biobuf*, int, int);
int	Binits(Biobuf*, int, int, uchar*, int);
int	Blinelen(Biobuf*);
vlong	Boffset(Biobuf*);
Biobuf*	Bopen(char*, int);
int	Bprint(Biobuf*, char*, ...);
int	Bvprint(Biobuf*, char*, va_list);
int	Bputc(Biobuf*, int);
int	Bputrune(Biobuf*, long);
void*	Brdline(Biobuf*, int);
char*	Brdstr(Biobuf*, int, int);
long	Bread(Biobuf*, void*, long);
vlong	Bseek(Biobuf*, vlong, int);
int	Bterm(Biobuf*);
int	Bungetc(Biobuf*);
int	Bungetrune(Biobuf*);
long	Bwrite(Biobuf*, void*, long);
#pragma	varargck	argpos	Bprint	2