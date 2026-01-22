typedef struct Data	Data;
typedef struct Page	Page;
typedef struct Proc	Proc;
typedef struct Seg	Seg;
enum {
Psegment = 0,
Pfd,
Pfpregs,
Pkregs,
Pnoteid,
Pns,
Pproc,
Pregs,
Pstatus,
Npfile,
Pagesize = 1024,
};
struct Data {
ulong len;
char data[1];
};
struct Seg {
char*	name;
uvlong	offset;
uvlong	 len;
Page**	pg;
int	npg;
};
struct Page {
Page*	link;
ulong	len;
char*	data;
int	written;
int	type;
ulong	pid;
uvlong	offset;
};
struct Proc {
Proc *link;
long	pid;
Data*	d[Npfile];
Seg**	seg;
int	nseg;
Seg*	text;
};
extern char *pfile[Npfile];
Proc*	snap(long pid, int usetext);
void*	emalloc(ulong);
void*	erealloc(void*, ulong);
char*	estrdup(char*);
void	writesnap(Biobuf*, Proc*);
Page*	datapage(char *p, long len);
Proc*	readsnap(Biobuf *b);
Page*	findpage(Proc *plist, long pid, int type, uvlong off);
int	debug;