#include <u.h>
#include <libc.h>
#include <auth.h>
#include <fcall.h>
#include <bio.h>
enum {
MAXRPC = 8192,
Qroot = 1,
Qallow,
Qdelay,
Qblock,
Qdial,
Qdeny,
Qtrusted,
Qctl,
Qdummy,
Qaddr,
Qtrustedfile = 100,
Qaddrfile = 1000,
Directory = 0,
Addrdir,
IPaddr,
Acctaddr,
Trusted,
Trustedperm,
Trustedtemp,
Ctlfile,
Dummynode,
};
typedef struct Fid Fid;
typedef struct Node Node;
typedef struct Address Address;
typedef struct Cidraddr Cidraddr;
typedef struct Keyword Keyword;
struct Fid
{
int fid;
int dirindex;
Node *node;
int busy;
int open;
char *name;
char *uid;
Fid *next;
};
struct Cidraddr
{
ulong ipaddr;
ulong mask;
};
struct Address
{
char *name;
Cidraddr ip;
};
struct Node
{
Dir d;
int count;
int allocated;
ulong baseqid;
Node *parent;
Node *sibs;
union {
Node *children;
Address *addrs;
Cidraddr ip;
};
};
struct Keyword {
char *name;
int code;
};
Node *root;
Node dummy;
int srvfd;
uchar rbuf[IOHDRSZ+MAXRPC+1];
int debugfd;
char *ctlfile;
char *conffile;
long lastconftime;
long lastctltime;
int trustedqid;
char* atom(char*);
void cidrparse(Cidraddr*, char*);
void cleantrusted(void);
Node* dirwalk(char*, Node*);
int dread(Fid*, int);
void fatal(char*, ...);
Node* finddir(int);
int findkey(char*, Keyword*);
void getconf(void);
int hread(Fid*, int);
void io(void);
Node* newnode(Node*, char*, ushort, int, ulong);
void printfid(Fid*);
void printnode(Node*);
void printtree(Node*);
void reload(void);
char* subslash(char*);
char* walk(char*, Fid*);