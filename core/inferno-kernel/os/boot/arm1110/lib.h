extern void *memccpy(void*, void*, int, long);
extern void *memset(void*, int, long);
extern int memcmp(void*, void*, long);
extern void *memmove(void*, void*, long);
extern void *memchr(void*, int, long);
extern char *strcat(char*, char*);
extern char *strchr(char*, char);
extern char *strrchr(char*, char);
extern int strcmp(char*, char*);
extern char *strcpy(char*, char*);
extern char *strncat(char*, char*, long);
extern char *strncpy(char*, char*, long);
extern int strncmp(char*, char*, long);
extern long strlen(char*);
extern char* strstr(char*, char*);
extern int atoi(char*);
enum
{
UTFmax = 3,
Runesync = 0x80,
Runeself = 0x80,
Runeerror = 0x80,
};
extern int runetochar(char*, Rune*);
extern int chartorune(Rune*, char*);
extern char* utfrune(char*, long);
extern int utflen(char*);
extern int runelen(long);
extern int abs(int);
typedef struct Cconv Fconv;
extern char* donprint(char*, char*, char*, void*);
extern int sprint(char*, char*, ...);
extern char* seprint(char*, char*, char*, ...);
extern int snprint(char*, int, char*, ...);
extern int print(char*, ...);
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
#define MORDER 0x0003
#define MREPL 0x0000
#define MBEFORE 0x0001
#define MAFTER 0x0002
#define MCREATE 0x0004
#define MCACHE 0x0010
#define MMASK 0x001F
#define OREAD 0
#define OWRITE 1
#define ORDWR 2
#define OEXEC 3
#define OTRUNC 16
#define OCEXEC 32
#define ORCLOSE 64
#define NCONT 0
#define NDFLT 1
#define NSAVE 2
#define NRSTR 3
typedef struct Qid Qid;
typedef struct Dir Dir;
typedef struct Waitmsg Waitmsg;
#define ERRLEN 64
#define DIRLEN 116
#define NAMELEN 28
struct Qid
{
ulong path;
ulong vers;
};
struct Dir
{
char name[NAMELEN];
char uid[NAMELEN];
char gid[NAMELEN];
Qid qid;
ulong mode;
long atime;
long mtime;
vlong length;
short type;
short dev;
};
struct Waitmsg
{
char pid[12];
char time[3*12];
char msg[ERRLEN];
};
typedef
struct Lock {
int val;
} Lock;
extern int _tas(int*);
extern void lock(Lock*);
extern void unlock(Lock*);
extern int canlock(Lock*);