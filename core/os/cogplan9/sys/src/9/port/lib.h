#define nelem(x) (sizeof(x)/sizeof((x)[0]))
#define offsetof(s, m) (ulong)(&(((s*)0)->m))
#define assert(x) if(x){}else _assert("x")
extern void* memccpy(void*, void*, int, ulong);
extern void* memset(void*, int, ulong);
extern int memcmp(void*, void*, ulong);
extern void* memmove(void*, void*, ulong);
extern void* memchr(void*, int, ulong);
extern char* strcat(char*, char*);
extern char* strchr(char*, int);
extern char* strrchr(char*, int);
extern int strcmp(char*, char*);
extern char* strcpy(char*, char*);
extern char* strecpy(char*, char*, char*);
extern char* strncat(char*, char*, long);
extern char* strncpy(char*, char*, long);
extern int strncmp(char*, char*, long);
extern long strlen(char*);
extern char* strstr(char*, char*);
extern int atoi(char*);
extern int fullrune(char*, int);
extern int cistrcmp(char*, char*);
extern int cistrncmp(char*, char*, int);
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
extern char* utfrune(char*, long);
extern int utflen(char*);
extern int utfnlen(char*, long);
extern int runelen(long);
extern int abs(int);
typedef struct Fmt Fmt;
typedef int (*Fmts)(Fmt*);
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
extern int print(char*, ...);
extern char* seprint(char*, char*, char*, ...);
extern char* vseprint(char*, char*, char*, va_list);
extern int snprint(char*, int, char*, ...);
extern int vsnprint(char*, int, char*, va_list);
extern int sprint(char*, char*, ...);
#pragma varargck argpos fmtprint 2
#pragma varargck argpos print 1
#pragma varargck argpos seprint 3
#pragma varargck argpos snprint 3
#pragma varargck argpos sprint 2
#pragma varargck type "lld" vlong
#pragma varargck type "llx" vlong
#pragma varargck type "lld" uvlong
#pragma varargck type "llx" uvlong
#pragma varargck type "ld" long
#pragma varargck type "lx" long
#pragma varargck type "ld" ulong
#pragma varargck type "lx" ulong
#pragma varargck type "d" int
#pragma varargck type "x" int
#pragma varargck type "c" int
#pragma varargck type "C" int
#pragma varargck type "d" uint
#pragma varargck type "x" uint
#pragma varargck type "c" uint
#pragma varargck type "C" uint
#pragma varargck type "s" char*
#pragma varargck type "q" char*
#pragma varargck type "S" Rune*
#pragma varargck type "%" void
#pragma varargck type "p" uintptr
#pragma varargck type "p" void*
#pragma varargck flag ','
extern int fmtstrinit(Fmt*);
extern int fmtinstall(int, int (*)(Fmt*));
extern void quotefmtinstall(void);
extern int fmtprint(Fmt*, char*, ...);
extern int fmtstrcpy(Fmt*, char*);
extern char* fmtstrflush(Fmt*);
extern char* cleanname(char*);
extern ulong getcallerpc(void*);
extern long strtol(char*, char**, int);
extern ulong strtoul(char*, char**, int);
extern vlong strtoll(char*, char**, int);
extern uvlong strtoull(char*, char**, int);
extern char etext[];
extern char edata[];
extern char end[];
extern int getfields(char*, char**, int, int, char*);
extern int tokenize(char*, char**, int);
extern int dec64(uchar*, int, char*, int);
extern int encodefmt(Fmt*);
extern void qsort(void*, long, long, int (*)(void*, void*));
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
#define NCONT 0
#define NDFLT 1
#define NSAVE 2
#define NRSTR 3
typedef struct Qid Qid;
typedef struct Dir Dir;
typedef struct OWaitmsg OWaitmsg;
typedef struct Waitmsg Waitmsg;
#define ERRMAX 128
#define KNAMELEN 28
#define QTDIR 0x80
#define QTAPPEND 0x40
#define QTEXCL 0x20
#define QTMOUNT 0x10
#define QTAUTH 0x08
#define QTFILE 0x00
#define DMDIR 0x80000000
#define DMAPPEND 0x40000000
#define DMEXCL 0x20000000
#define DMMOUNT 0x10000000
#define DMREAD 0x4
#define DMWRITE 0x2
#define DMEXEC 0x1
struct Qid
{
uvlong path;
ulong vers;
uchar type;
};
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
};
struct OWaitmsg
{
char pid[12];
char time[3*12];
char msg[64];
};
struct Waitmsg
{
int pid;
ulong time[3];
char msg[ERRMAX];
};