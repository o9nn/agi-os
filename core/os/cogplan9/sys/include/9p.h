#pragma src "/sys/src/lib9p"
#pragma lib "lib9p.a"
typedef struct Intmap	Intmap;
#pragma incomplete Intmap
Intmap*	allocmap(void (*inc)(void*));
void		freemap(Intmap*, void (*destroy)(void*));
void*	lookupkey(Intmap*, ulong);
void*	insertkey(Intmap*, ulong, void*);
int		caninsertkey(Intmap*, ulong, void*);
void*	deletekey(Intmap*, ulong);
typedef struct Fid		Fid;
typedef struct Req		Req;
typedef struct Fidpool	Fidpool;
typedef struct Reqpool	Reqpool;
typedef struct File		File;
typedef struct Filelist	Filelist;
typedef struct Tree		Tree;
typedef struct Readdir	Readdir;
typedef struct Srv Srv;
#pragma incomplete Filelist
#pragma incomplete Readdir
struct Fid
{
ulong	fid;
char		omode;
File*		file;
char*	uid;
Qid		qid;
void*	aux;
Readdir*	rdir;
Ref		ref;
Fidpool*	pool;
vlong	diroffset;
long		dirindex;
};
struct Req
{
ulong	tag;
void*	aux;
Fcall		ifcall;
Fcall		ofcall;
Dir		d;
Req*		oldreq;
Fid*		fid;
Fid*		afid;
Fid*		newfid;
Srv*		srv;
QLock	lk;
Ref		ref;
Reqpool*	pool;
uchar*	buf;
uchar	type;
uchar	responded;
char*	error;
void*	rbuf;
Req**	flush;
int		nflush;
};
struct Fidpool {
Intmap	*map;
void		(*destroy)(Fid*);
Srv		*srv;
};
struct Reqpool {
Intmap	*map;
void		(*destroy)(Req*);
Srv		*srv;
};
Fidpool*	allocfidpool(void (*destroy)(Fid*));
void		freefidpool(Fidpool*);
Fid*		allocfid(Fidpool*, ulong);
Fid*		lookupfid(Fidpool*, ulong);
void		closefid(Fid*);
Fid*		removefid(Fidpool*, ulong);
Reqpool*	allocreqpool(void (*destroy)(Req*));
void		freereqpool(Reqpool*);
Req*		allocreq(Reqpool*, ulong);
Req*		lookupreq(Reqpool*, ulong);
void		closereq(Req*);
Req*		removereq(Reqpool*, ulong);
typedef	int	Dirgen(int, Dir*, void*);
void		dirread9p(Req*, Dirgen*, void*);
struct File {
Ref;
Dir;
File *parent;
void *aux;
RWLock;
Filelist *filelist;
Tree *tree;
int nchild;
int allocd;
int nxchild;
Ref readers;
};
struct Tree {
File *root;
void	(*destroy)(File *file);
Lock genlock;
ulong qidgen;
ulong dirqidgen;
};
Tree*	alloctree(char*, char*, ulong, void(*destroy)(File*));
void		freetree(Tree*);
File*		createfile(File*, char*, char*, ulong, void*);
int		removefile(File*);
void		closefile(File*);
File*		walkfile(File*, char*);
Readdir*	opendirfile(File*);
long		readdirfile(Readdir*, uchar*, long);
void		closedirfile(Readdir*);
typedef struct Cmdbuf Cmdbuf;
typedef struct Cmdtab Cmdtab;
Cmdbuf*		parsecmd(char *a, int n);
void		respondcmderror(Req*, Cmdbuf*, char*, ...);
Cmdtab*	lookupcmd(Cmdbuf*, Cmdtab*, int);
#pragma varargck argpos respondcmderr 3
struct Cmdbuf
{
char	*buf;
char	**f;
int	nf;
};
struct Cmdtab
{
int	index;
char	*cmd;
int	narg;
};
struct Srv {
Tree*	tree;
void		(*destroyfid)(Fid*);
void		(*destroyreq)(Req*);
void		(*end)(Srv*);
void*	aux;
void		(*attach)(Req*);
void		(*auth)(Req*);
void		(*open)(Req*);
void		(*create)(Req*);
void		(*read)(Req*);
void		(*write)(Req*);
void		(*remove)(Req*);
void		(*flush)(Req*);
void		(*stat)(Req*);
void		(*wstat)(Req*);
void		(*walk)(Req*);
char*	(*clone)(Fid*, Fid*);
char*	(*walk1)(Fid*, char*, Qid*);
int		infd;
int		outfd;
int		nopipe;
int		srvfd;
int		leavefdsopen;
char*	keyspec;
Fidpool*	fpool;
Reqpool*	rpool;
uint		msize;
uchar*	rbuf;
QLock	rlock;
uchar*	wbuf;
QLock	wlock;
char*	addr;
};
void		srv(Srv*);
void		postmountsrv(Srv*, char*, char*, int);
void		_postmountsrv(Srv*, char*, char*, int);
void		listensrv(Srv*, char*);
void		_listensrv(Srv*, char*);
int 		postfd(char*, int);
int		chatty9p;
void		respond(Req*, char*);
void		responderror(Req*);
void		threadpostmountsrv(Srv*, char*, char*, int);
void		threadlistensrv(Srv *s, char *addr);
int		hasperm(File*, char*, int);
void*	emalloc9p(ulong);
void*	erealloc9p(void*, ulong);
char*	estrdup9p(char*);
enum {
OMASK = 3
};
void		readstr(Req*, char*);
void		readbuf(Req*, void*, long);
void		walkandclone(Req*, char*(*walk1)(Fid*,char*,void*),
char*(*clone)(Fid*,Fid*,void*), void*);
void		auth9p(Req*);
void		authread(Req*);
void		authwrite(Req*);
void		authdestroy(Fid*);
int		authattach(Req*);
extern void (*_forker)(void (*)(void*), void*, int);