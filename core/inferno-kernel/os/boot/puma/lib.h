extern void* memccpy(void*, void*, int, long);
extern void* memset(void*, int, long);
extern int memcmp(void*, void*, long);
extern void* memmove(void*, void*, long);
extern void* memchr(void*, int, long);
extern char* strcat(char*, char*);
extern char* strchr(char*, char);
extern int strcmp(char*, char*);
extern char* strcpy(char*, char*);
extern char* strncat(char*, char*, long);
extern char* strncpy(char*, char*, long);
extern int strncmp(char*, char*, long);
extern long strlen(char*);
extern char* strrchr(char*, char);
extern char* strstr(char*, char*);
typedef struct Fconv Fconv;
extern char* donprint(char*, char*, char*, void*);
extern int sprint(char*, char*, ...);
extern int print(char*, ...);
#define PRINTSIZE 256
extern int atoi(char*);
extern long strtol(char*, char**, int);
extern ulong strtoul(char*, char**, int);
extern char end[];
extern char edata[];
#define MORDER 0x0003
#define MREPL 0x0000
#define MBEFORE 0x0001
#define MAFTER 0x0002
#define MCREATE 0x0004
#define MMASK 0x0007
#define OREAD 0
#define OWRITE 1
#define ORDWR 2
#define OEXEC 3
#define OTRUNC 16
#define OCEXEC 32
#define ORCLOSE 64
#define NCONT 0
#define NDFLT 1
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
ulong length;
short type;
short dev;
};
struct Waitmsg
{
int pid;
int status;
ulong time[3];
char msg[ERRLEN];
};
#define nelem(x) (sizeof(x)/sizeof((x)[0]))