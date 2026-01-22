#pragma lib "libc.a"
#pragma src "/sys/src/libc"
#define nelem(x) (sizeof(x)/sizeof((x)[0]))
#define offsetof(s, m) (ulong)(&(((s*)0)->m))
#define assert(x) if(x){}else _assert("x")
extern void* memccpy(void*, void*, int, ulong);
extern void* memset(void*, int, ulong);
extern int memcmp(void*, void*, ulong);
extern void* memcpy(void*, void*, ulong);
extern void* memmove(void*, void*, ulong);
extern void* memchr(void*, int, ulong);
extern char* strcat(char*, char*);
extern char* strchr(char*, int);
extern int strcmp(char*, char*);
extern char* strcpy(char*, char*);
extern char* strecpy(char*, char*, char*);
extern char* strdup(char*);
extern char* strncat(char*, char*, long);
extern char* strncpy(char*, char*, long);
extern int strncmp(char*, char*, long);
extern char* strpbrk(char*, char*);
extern char* strrchr(char*, int);
extern char* strtok(char*, char*);
extern long strlen(char*);
extern long strspn(char*, char*);
extern long strcspn(char*, char*);
extern char* strstr(char*, char*);
extern int cistrncmp(char*, char*, int);
extern int cistrcmp(char*, char*);
extern char* cistrstr(char*, char*);
extern int tokenize(char*, char**, int);
enum
{
UTFmax = 4,
Runesync = 0x80,
Runeself = 0x80,
Runeerror = 0xFFFD,
Runemax = 0x10FFFF,
Runemask = 0x1FFFFF,
};
extern int runetochar(char*, Rune*);
extern int chartorune(Rune*, char*);
extern int runelen(long);
extern int runenlen(Rune*, int);
extern int fullrune(char*, int);
extern int utflen(char*);
extern int utfnlen(char*, long);
extern char* utfrune(char*, long);
extern char* utfrrune(char*, long);
extern char* utfutf(char*, char*);
extern char* utfecpy(char*, char*, char*);
extern Rune* runestrcat(Rune*, Rune*);
extern Rune* runestrchr(Rune*, Rune);
extern int runestrcmp(Rune*, Rune*);
extern Rune* runestrcpy(Rune*, Rune*);
extern Rune* runestrncpy(Rune*, Rune*, long);
extern Rune* runestrecpy(Rune*, Rune*, Rune*);
extern Rune* runestrdup(Rune*);
extern Rune* runestrncat(Rune*, Rune*, long);
extern int runestrncmp(Rune*, Rune*, long);
extern Rune* runestrrchr(Rune*, Rune);
extern long runestrlen(Rune*);
extern Rune* runestrstr(Rune*, Rune*);
extern Rune tolowerrune(Rune);
extern Rune totitlerune(Rune);
extern Rune toupperrune(Rune);
extern Rune tobaserune(Rune);
extern int isalpharune(Rune);
extern int isbaserune(Rune);
extern int isdigitrune(Rune);
extern int islowerrune(Rune);
extern int isspacerune(Rune);
extern int istitlerune(Rune);
extern int isupperrune(Rune);
extern void* malloc(ulong);
extern void* mallocz(ulong, int);
extern void free(void*);
extern ulong msize(void*);
extern void* mallocalign(ulong, ulong, long, ulong);
extern void* calloc(ulong, ulong);
extern void* realloc(void*, ulong);
extern void setmalloctag(void*, ulong);
extern void setrealloctag(void*, ulong);
extern ulong getmalloctag(void*);
extern ulong getrealloctag(void*);
extern void* malloctopoolblock(void*);
typedef struct Fmt Fmt;
struct Fmt{
uchar runes;
void *start;
void *to;
void *stop;
int (*flush)(Fmt *);
void *farg;
int nfmt;
va_list args;
int r;
int width;
int prec;
ulong flags;
};
enum{
FmtWidth = 1,
FmtLeft = FmtWidth << 1,
FmtPrec = FmtLeft << 1,
FmtSharp = FmtPrec << 1,
FmtSpace = FmtSharp << 1,
FmtSign = FmtSpace << 1,
FmtZero = FmtSign << 1,
FmtUnsigned = FmtZero << 1,
FmtShort = FmtUnsigned << 1,
FmtLong = FmtShort << 1,
FmtVLong = FmtLong << 1,
FmtComma = FmtVLong << 1,
FmtByte = FmtComma << 1,
FmtFlag = FmtByte << 1
};
extern int print(char*, ...);
extern char* seprint(char*, char*, char*, ...);
extern char* vseprint(char*, char*, char*, va_list);
extern int snprint(char*, int, char*, ...);
extern int vsnprint(char*, int, char*, va_list);
extern char* smprint(char*, ...);
extern char* vsmprint(char*, va_list);
extern int sprint(char*, char*, ...);
extern int fprint(int, char*, ...);
extern int vfprint(int, char*, va_list);
extern int runesprint(Rune*, char*, ...);
extern int runesnprint(Rune*, int, char*, ...);
extern int runevsnprint(Rune*, int, char*, va_list);
extern Rune* runeseprint(Rune*, Rune*, char*, ...);
extern Rune* runevseprint(Rune*, Rune*, char*, va_list);
extern Rune* runesmprint(char*, ...);
extern Rune* runevsmprint(char*, va_list);
extern int fmtfdinit(Fmt*, int, char*, int);
extern int fmtfdflush(Fmt*);
extern int fmtstrinit(Fmt*);
extern char* fmtstrflush(Fmt*);
extern int runefmtstrinit(Fmt*);
extern Rune* runefmtstrflush(Fmt*);
#pragma varargck argpos fmtprint 2
#pragma varargck argpos fprint 2
#pragma varargck argpos print 1
#pragma varargck argpos runeseprint 3
#pragma varargck argpos runesmprint 1
#pragma varargck argpos runesnprint 3
#pragma varargck argpos runesprint 2
#pragma varargck argpos seprint 3
#pragma varargck argpos smprint 1
#pragma varargck argpos snprint 3
#pragma varargck argpos sprint 2
#pragma varargck type "lld" vlong
#pragma varargck type "llo" vlong
#pragma varargck type "llx" vlong
#pragma varargck type "llb" vlong
#pragma varargck type "lld" uvlong
#pragma varargck type "llo" uvlong
#pragma varargck type "llx" uvlong
#pragma varargck type "llb" uvlong
#pragma varargck type "ld" long
#pragma varargck type "lo" long
#pragma varargck type "lx" long
#pragma varargck type "lb" long
#pragma varargck type "ld" ulong
#pragma varargck type "lo" ulong
#pragma varargck type "lx" ulong
#pragma varargck type "lb" ulong
#pragma varargck type "d" int
#pragma varargck type "o" int
#pragma varargck type "x" int
#pragma varargck type "c" int
#pragma varargck type "C" int
#pragma varargck type "b" int
#pragma varargck type "d" uint
#pragma varargck type "x" uint
#pragma varargck type "c" uint
#pragma varargck type "C" uint
#pragma varargck type "b" uint
#pragma varargck type "f" double
#pragma varargck type "e" double
#pragma varargck type "g" double
#pragma varargck type "s" char*
#pragma varargck type "q" char*
#pragma varargck type "S" Rune*
#pragma varargck type "Q" Rune*
#pragma varargck type "r" void
#pragma varargck type "%" void
#pragma varargck type "n" int*
#pragma varargck type "p" uintptr
#pragma varargck type "p" void*
#pragma varargck flag ','
#pragma varargck flag ' '
#pragma varargck flag 'h'
#pragma varargck type "<" void*
#pragma varargck type "[" void*
#pragma varargck type "H" void*
#pragma varargck type "lH" void*
extern int fmtinstall(int, int (*)(Fmt*));
extern int dofmt(Fmt*, char*);
extern int dorfmt(Fmt*, Rune*);
extern int fmtprint(Fmt*, char*, ...);
extern int fmtvprint(Fmt*, char*, va_list);
extern int fmtrune(Fmt*, int);
extern int fmtstrcpy(Fmt*, char*);
extern int fmtrunestrcpy(Fmt*, Rune*);
extern int errfmt(Fmt *f);
extern char *unquotestrdup(char*);
extern Rune *unquoterunestrdup(Rune*);
extern char *quotestrdup(char*);
extern Rune *quoterunestrdup(Rune*);
extern int quotestrfmt(Fmt*);
extern int quoterunestrfmt(Fmt*);
extern void quotefmtinstall(void);
extern int (*doquote)(int);
extern int needsrcquote(int);
extern void srand(long);
extern int rand(void);
extern int nrand(int);
extern long lrand(void);
extern long lnrand(long);
extern double frand(void);
extern ulong truerand(void);
extern ulong ntruerand(ulong);
extern ulong getfcr(void);
extern void setfsr(ulong);
extern ulong getfsr(void);
extern void setfcr(ulong);
extern double NaN(void);
extern double Inf(int);
extern int isNaN(double);
extern int isInf(double, int);
extern ulong umuldiv(ulong, ulong, ulong);
extern long muldiv(long, long, long);
extern double pow(double, double);
extern double atan2(double, double);
extern double fabs(double);
extern double atan(double);
extern double log(double);
extern double log10(double);
extern double exp(double);
extern double floor(double);
extern double ceil(double);
extern double hypot(double, double);
extern double sin(double);
extern double cos(double);
extern double tan(double);
extern double asin(double);
extern double acos(double);
extern double sinh(double);
extern double cosh(double);
extern double tanh(double);
extern double sqrt(double);
extern double fmod(double, double);
#define HUGE 3.4028234e38
#define PIO2 1.570796326794896619231e0
#define PI (PIO2+PIO2)
typedef
struct Tm
{
int sec;
int min;
int hour;
int mday;
int mon;
int year;
int wday;
int yday;
char zone[4];
int tzoff;
} Tm;
extern Tm* gmtime(long);
extern Tm* localtime(long);
extern char* asctime(Tm*);
extern char* ctime(long);
extern double cputime(void);
extern long times(long*);
extern long tm2sec(Tm*);
extern vlong nsec(void);
extern void cycles(uvlong*);
enum
{
PNPROC = 1,
PNGROUP = 2,
};
extern void _assert(char*);
extern int abs(int);
extern int atexit(void(*)(void));
extern void atexitdont(void(*)(void));
extern int atnotify(int(*)(void*, char*), int);
extern double atof(char*);
extern int atoi(char*);
extern long atol(char*);
extern vlong atoll(char*);
extern double charstod(int(*)(void*), void*);
extern char* cleanname(char*);
extern int decrypt(void*, void*, int);
extern int encrypt(void*, void*, int);
extern int dec64(uchar*, int, char*, int);
extern int enc64(char*, int, uchar*, int);
extern int dec32(uchar*, int, char*, int);
extern int enc32(char*, int, uchar*, int);
extern int dec16(uchar*, int, char*, int);
extern int enc16(char*, int, uchar*, int);
extern int encodefmt(Fmt*);
extern void exits(char*);
extern double frexp(double, int*);
extern uintptr getcallerpc(void*);
extern char* getenv(char*);
extern int getfields(char*, char**, int, int, char*);
extern int gettokens(char *, char **, int, char *);
extern char* getuser(void);
extern char* getwd(char*, int);
extern int iounit(int);
extern long labs(long);
extern double ldexp(double, int);
extern void longjmp(jmp_buf, int);
extern char* mktemp(char*);
extern double modf(double, double*);
extern int netcrypt(void*, void*);
extern void notejmp(void*, jmp_buf, int);
extern void perror(char*);
extern int postnote(int, int, char *);
extern double pow10(int);
extern int putenv(char*, char*);
extern void qsort(void*, long, long, int (*)(void*, void*));
extern int setjmp(jmp_buf);
extern double strtod(char*, char**);
extern long strtol(char*, char**, int);
extern ulong strtoul(char*, char**, int);
extern vlong strtoll(char*, char**, int);
extern uvlong strtoull(char*, char**, int);
extern void sysfatal(char*, ...);
#pragma varargck argpos sysfatal 1
extern void syslog(int, char*, char*, ...);
#pragma varargck argpos syslog 3
extern long time(long*);
extern int tolower(int);
extern int toupper(int);
enum {
Profoff,
Profuser,
Profkernel,
Proftime,
Profsample,
};
extern void prof(void (*fn)(void*), void *arg, int entries, int what);
long ainc(long*);
long adec(long*);
int cas32(u32int*, u32int, u32int);
int casp(void**, void*, void*);
int casl(ulong*, ulong, ulong);
typedef
struct Lock {
long key;
long sem;
} Lock;
extern int _tas(int*);
extern void lock(Lock*);
extern void unlock(Lock*);
extern int canlock(Lock*);
typedef struct QLp QLp;
struct QLp
{
int inuse;
QLp *next;
char state;
};
typedef
struct QLock
{
Lock lock;
int locked;
QLp *head;
QLp *tail;
} QLock;
extern void qlock(QLock*);
extern void qunlock(QLock*);
extern int canqlock(QLock*);
extern void _qlockinit(void* (*)(void*, void*));
typedef
struct RWLock
{
Lock lock;
int readers;
int writer;
QLp *head;
QLp *tail;
} RWLock;
extern void rlock(RWLock*);
extern void runlock(RWLock*);
extern int canrlock(RWLock*);
extern void wlock(RWLock*);
extern void wunlock(RWLock*);
extern int canwlock(RWLock*);
typedef
struct Rendez
{
QLock *l;
QLp *head;
QLp *tail;
} Rendez;
extern void rsleep(Rendez*);
extern int rwakeup(Rendez*);
extern int rwakeupall(Rendez*);
extern void** privalloc(void);
extern void privfree(void**);
#define NETPATHLEN 40
extern int accept(int, char*);
extern int announce(char*, char*);
extern int dial(char*, char*, char*, int*);
extern void setnetmtpt(char*, int, char*);
extern int hangup(int);
extern int listen(char*, char*);
extern char* netmkaddr(char*, char*, char*);
extern int reject(int, char*, char*);
extern int pushssl(int, char*, char*, char*, int*);
extern int pushtls(int, char*, char*, int, char*, char*);
typedef struct NetConnInfo NetConnInfo;
struct NetConnInfo
{
char *dir;
char *root;
char *spec;
char *lsys;
char *lserv;
char *rsys;
char *rserv;
char *laddr;
char *raddr;
};
extern NetConnInfo* getnetconninfo(char*, int);
extern void freenetconninfo(NetConnInfo*);
#define STATMAX 65535U
#define DIRMAX (sizeof(Dir)+STATMAX)
#define ERRMAX 128
#define MORDER 0x0003
#define MREPL 0x0000
#define MBEFORE 0x0001
#define MAFTER 0x0002
#define MCREATE 0x0004
#define MCACHE 0x0010
#define MMASK 0x0017
#define OREAD 0
#define OWRITE 1
#define ORDWR 2
#define OEXEC 3
#define OTRUNC 16
#define OCEXEC 32
#define ORCLOSE 64
#define OEXCL 0x1000
#define AEXIST 0
#define AEXEC 1
#define AWRITE 2
#define AREAD 4
#define SG_RONLY 0040
#define SG_CEXEC 0100
#define NCONT 0
#define NDFLT 1
#define NSAVE 2
#define NRSTR 3
#define QTDIR 0x80
#define QTAPPEND 0x40
#define QTEXCL 0x20
#define QTMOUNT 0x10
#define QTAUTH 0x08
#define QTTMP 0x04
#define QTFILE 0x00
#define DMDIR 0x80000000
#define DMAPPEND 0x40000000
#define DMEXCL 0x20000000
#define DMMOUNT 0x10000000
#define DMAUTH 0x08000000
#define DMTMP 0x04000000
#define DMREAD 0x4
#define DMWRITE 0x2
#define DMEXEC 0x1
enum
{
RFNAMEG = (1<<0),
RFENVG = (1<<1),
RFFDG = (1<<2),
RFNOTEG = (1<<3),
RFPROC = (1<<4),
RFMEM = (1<<5),
RFNOWAIT = (1<<6),
RFCNAMEG = (1<<10),
RFCENVG = (1<<11),
RFCFDG = (1<<12),
RFREND = (1<<13),
RFNOMNT = (1<<14)
};
typedef
struct Qid
{
uvlong path;
ulong vers;
uchar type;
} Qid;
typedef
struct Dir {
ushort type;
uint dev;
Qid qid;
ulong mode;
ulong atime;
ulong mtime;
vlong length;
char *name;
char *uid;
char *gid;
char *muid;
} Dir;
typedef
struct Waitmsg
{
int pid;
ulong time[3];
char *msg;
} Waitmsg;
typedef
struct IOchunk
{
void *addr;
ulong len;
} IOchunk;
extern void _exits(char*);
extern void abort(void);
extern int access(char*, int);
extern long alarm(ulong);
extern int await(char*, int);
extern int bind(char*, char*, int);
extern int brk(void*);
extern int chdir(char*);
extern int close(int);
extern int create(char*, int, ulong);
extern int dup(int, int);
extern int errstr(char*, uint);
extern int exec(char*, char*[]);
extern int execl(char*, ...);
extern int fork(void);
extern int rfork(int);
extern int fauth(int, char*);
extern int fstat(int, uchar*, int);
extern int fwstat(int, uchar*, int);
extern int fversion(int, int, char*, int);
extern int mount(int, int, char*, int, char*);
extern int unmount(char*, char*);
extern int noted(int);
extern int notify(void(*)(void*, char*));
extern int open(char*, int);
extern int fd2path(int, char*, int);
extern int pipe(int*);
extern long pread(int, void*, long, vlong);
extern long preadv(int, IOchunk*, int, vlong);
extern long pwrite(int, void*, long, vlong);
extern long pwritev(int, IOchunk*, int, vlong);
extern long read(int, void*, long);
extern long readn(int, void*, long);
extern long readv(int, IOchunk*, int);
extern int remove(char*);
extern void* sbrk(ulong);
extern long oseek(int, long, int);
extern vlong seek(int, vlong, int);
extern void* segattach(int, char*, void*, ulong);
extern void* segbrk(void*, void*);
extern int segdetach(void*);
extern int segflush(void*, ulong);
extern int segfree(void*, ulong);
extern int semacquire(long*, int);
extern long semrelease(long*, long);
extern int sleep(long);
extern int stat(char*, uchar*, int);
extern int tsemacquire(long*, ulong);
extern Waitmsg* wait(void);
extern int waitpid(void);
extern long write(int, void*, long);
extern long writev(int, IOchunk*, int);
extern int wstat(char*, uchar*, int);
extern void* rendezvous(void*, void*);
extern Dir* dirstat(char*);
extern Dir* dirfstat(int);
extern int dirwstat(char*, Dir*);
extern int dirfwstat(int, Dir*);
extern long dirread(int, Dir**);
extern void nulldir(Dir*);
extern long dirreadall(int, Dir**);
extern int getpid(void);
extern int getppid(void);
extern void rerrstr(char*, uint);
extern char* sysname(void);
extern void werrstr(char*, ...);
#pragma varargck argpos werrstr 1
extern char *argv0;
#define ARGBEGIN for((argv0||(argv0=*argv)),argv++,argc--;\
argv[0] && argv[0][0]=='-' && argv[0][1];\
argc--, argv++) {\
char *_args, *_argt;\
Rune _argc;\
_args = &argv[0][1];\
if(_args[0]=='-' && _args[1]==0){\
argc--; argv++; break;\
}\
_argc = 0;\
while(*_args && (_args += chartorune(&_argc, _args)))\
switch(_argc)
#define ARGEND SET(_argt);USED(_argt,_argc,_args);}USED(argv, argc);
#define ARGF() (_argt=_args, _args="",\
(*_argt? _argt: argv[1]? (argc--, *++argv): 0))
#define EARGF(x) (_argt=_args, _args="",\
(*_argt? _argt: argv[1]? (argc--, *++argv): ((x), abort(), (char*)0)))
#define ARGC() _argc
extern char end[];