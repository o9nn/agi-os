#include <auth.h>
#include <fcall.h>
enum {
NFidHash	= 503,
};
typedef struct Con Con;
typedef struct DirBuf DirBuf;
typedef struct Excl Excl;
typedef struct Fid Fid;
typedef struct Fsys Fsys;
typedef struct Msg Msg;
#pragma incomplete DirBuf
#pragma incomplete Excl
#pragma incomplete Fsys
struct Msg {
uchar*	data;
u32int	msize;
Fcall	t;
Fcall	r;
Con*	con;
Msg*	anext;
Msg*	mnext;
Msg* 	mprev;
int	state;
Msg*	flush;
Msg*	rwnext;
int	nowq;
};
enum {
MsgN		= 0,
MsgR		= 1,
Msg9		= 2,
MsgW		= 3,
MsgF		= 4,
};
enum {
ConNoneAllow	= 1<<0,
ConNoAuthCheck	= 1<<1,
ConNoPermCheck	= 1<<2,
ConWstatAllow	= 1<<3,
ConIPCheck	= 1<<4,
};
struct Con {
char*	name;
uchar*	data;
int	isconsole;
int	flags;
char	remote[128];
VtLock*	lock;
int	state;
int	fd;
Msg*	version;
u32int	msize;
VtRendez* rendez;
Con*	anext;
Con*	cnext;
Con*	cprev;
VtLock*	alock;
int	aok;
VtLock*	mlock;
Msg*	mhead;
Msg*	mtail;
VtRendez* mrendez;
VtLock*	wlock;
Msg*	whead;
Msg*	wtail;
VtRendez* wrendez;
VtLock*	fidlock;
Fid*	fidhash[NFidHash];
Fid*	fhead;
Fid*	ftail;
int	nfid;
};
enum {
ConDead		= 0,
ConNew		= 1,
ConDown		= 2,
ConInit		= 3,
ConUp		= 4,
ConMoribund	= 5,
};
struct Fid {
VtLock*	lock;
Con*	con;
u32int	fidno;
int	ref;
int	flags;
int	open;
Fsys*	fsys;
File*	file;
Qid	qid;
char*	uid;
char*	uname;
DirBuf*	db;
Excl*	excl;
VtLock*	alock;
AuthRpc* rpc;
char*	cuname;
Fid*	sort;
Fid*	hash;
Fid*	next;
Fid*	prev;
};
enum {
FidFCreate	= 0x01,
FidFWlock	= 0x02,
};
enum {
FidOCreate	= 0x01,
FidORead	= 0x02,
FidOWrite	= 0x04,
FidORclose	= 0x08,
};
extern int (*rFcall[Tmax])(Msg*);
extern int validFileName(char*);
extern int authCheck(Fcall*, Fid*, Fsys*);
extern int authRead(Fid*, void*, int);
extern int authWrite(Fid*, void*, int);
extern void dirBufFree(DirBuf*);
extern int dirDe2M(DirEntry*, uchar*, int);
extern int dirRead(Fid*, uchar*, int, vlong);
extern int exclAlloc(Fid*);
extern void exclFree(Fid*);
extern void exclInit(void);
extern int exclUpdate(Fid*);
extern void fidClunk(Fid*);
extern void fidClunkAll(Con*);
extern Fid* fidGet(Con*, u32int, int);
extern void fidInit(void);
extern void fidPut(Fid*);
extern void fsysFsRlock(Fsys*);
extern void fsysFsRUnlock(Fsys*);
extern Fs* fsysGetFs(Fsys*);
extern Fsys* fsysGet(char*);
extern char* fsysGetName(Fsys*);
extern File* fsysGetRoot(Fsys*, char*);
extern Fsys* fsysIncRef(Fsys*);
extern int fsysInit(void);
extern int fsysNoAuthCheck(Fsys*);
extern int fsysNoPermCheck(Fsys*);
extern void fsysPut(Fsys*);
extern int fsysWstatAllow(Fsys*);
extern int lstnInit(void);
extern Con* conAlloc(int, char*, int);
extern void conInit(void);
extern void msgFlush(Msg*);
extern void msgInit(void);
extern int srvInit(void);
extern int groupLeader(char*, char*);
extern int groupMember(char*, char*);
extern int groupWriteMember(char*);
extern char* unameByUid(char*);
extern char* uidByUname(char*);
extern int usersInit(void);
extern int usersFileRead(char*);
extern int validUserName(char*);
extern char* uidadm;
extern char* unamenone;
extern char* uidnoworld;
extern int cliAddCmd(char*, int (*)(int, char*[]));
extern int cliError(char*, ...);
extern int cliInit(void);
extern int cliExec(char*);
#pragma	varargck	argpos	cliError	1
extern int cmdInit(void);
extern int consPrompt(char*);
extern int consInit(void);
extern int consOpen(int, int, int);
extern int consTTY(void);
extern int consWrite(char*, int);
extern int consPrint(char*, ...);
extern int consVPrint(char*, va_list);
#pragma	varargck	argpos	consPrint	1
extern int Dflag;