typedef struct Alarms Alarms;
typedef struct Block Block;
typedef struct Bkpt Bkpt;
typedef struct BkptCond BkptCond;
typedef struct Chan Chan;
typedef struct Cmdbuf Cmdbuf;
typedef struct Cmdtab Cmdtab;
typedef struct Cname Cname;
typedef struct Crypt Crypt;
typedef struct Dev Dev;
typedef struct DevConf DevConf;
typedef struct Dirtab Dirtab;
typedef struct Edf Edf;
typedef struct Egrp Egrp;
typedef struct Evalue Evalue;
typedef struct Fgrp Fgrp;
typedef struct List List;
typedef struct Log Log;
typedef struct Logflag Logflag;
typedef struct Mntcache Mntcache;
typedef struct Mntparam Mntparam;
typedef struct Mount Mount;
typedef struct Mntrpc Mntrpc;
typedef struct Mntwalk Mntwalk;
typedef struct Mnt Mnt;
typedef struct Mhead Mhead;
typedef struct Osenv Osenv;
typedef struct Pgrp Pgrp;
typedef struct Proc Proc;
typedef struct QLock QLock;
typedef struct Queue Queue;
typedef struct Ref Ref;
typedef struct Rendez Rendez;
typedef struct Rept Rept;
typedef struct Rootdata Rootdata;
typedef struct RWlock RWlock;
typedef struct Signerkey Signerkey;
typedef struct Skeyset Skeyset;
typedef struct Talarm Talarm;
typedef struct Timer Timer;
typedef struct Timers Timers;
typedef struct Uart Uart;
typedef struct Walkqid Walkqid;
typedef struct Atom Atom;
typedef struct AtomSpace AtomSpace;
typedef struct CognitiveState CognitiveState;
typedef struct Goal Goal;
typedef struct OpenCogKernel OpenCogKernel;
typedef struct PatternMatcher PatternMatcher;
typedef struct ReasoningEngine ReasoningEngine;
typedef int Devgen(Chan*, char*, Dirtab*, int, int, Dir*);
#pragma incomplete DevConf
#pragma incomplete Edf
#pragma incomplete Mntcache
#pragma incomplete Mntrpc
#pragma incomplete Queue
#pragma incomplete Timers
#include "fcall.h"
#include <pool.h>
struct Ref
{
Lock l;
long ref;
};
struct Rendez
{
Lock;
Proc *p;
};
struct Rept
{
Lock l;
Rendez r;
void *o;
int t;
int (*active)(void*);
int (*ck)(void*, int);
void (*f)(void*);
};
struct Osenv
{
char *syserrstr;
char *errstr;
char errbuf0[ERRMAX];
char errbuf1[ERRMAX];
Pgrp* pgrp;
Fgrp* fgrp;
Egrp* egrp;
Skeyset* sigs;
Rendez* rend;
Queue* waitq;
Queue* childq;
void* debug;
int uid;
int gid;
char* user;
FPenv fpu;
};
enum
{
Nopin = -1
};
struct QLock
{
Lock use;
Proc *head;
Proc *tail;
int locked;
};
struct RWlock
{
Lock;
QLock x;
QLock k;
int readers;
};
struct Talarm
{
Lock;
Proc* list;
};
struct Alarms
{
QLock;
Proc* head;
};
struct Rootdata
{
int dotdot;
void *ptr;
int size;
int *sizep;
};
enum
{
Aaccess,
Abind,
Atodir,
Aopen,
Amount,
Acreate,
Aremove,
COPEN = 0x0001,
CMSG = 0x0002,
CCEXEC = 0x0008,
CFREE = 0x0010,
CRCLOSE = 0x0020,
CCACHE = 0x0080,
};
enum
{
BINTR = (1<<0),
BFREE = (1<<1),
Bipck = (1<<2),
Budpck = (1<<3),
Btcpck = (1<<4),
Bpktck = (1<<5),
};
struct Block
{
Block* next;
Block* list;
uchar* rp;
uchar* wp;
uchar* lim;
uchar* base;
void (*free)(Block*);
ushort flag;
ushort checksum;
};
#define BLEN(s) ((s)->wp - (s)->rp)
#define BALLOC(s) ((s)->lim - (s)->base)
struct Chan
{
Lock;
Ref;
Chan* next;
Chan* link;
vlong offset;
ushort type;
ulong dev;
ushort mode;
ushort flag;
Qid qid;
int fid;
ulong iounit;
Mhead* umh;
Chan* umc;
QLock umqlock;
int uri;
int dri;
ulong mountid;
Mntcache *mcp;
Mnt *mux;
union {
void* aux;
char tag[4];
};
Chan* mchan;
Qid mqid;
Cname *name;
};
struct Cname
{
Ref;
int alen;
int len;
char *s;
};
struct Dev
{
int dc;
char* name;
void (*reset)(void);
void (*init)(void);
void (*shutdown)(void);
Chan* (*attach)(char*);
Walkqid* (*walk)(Chan*, Chan*, char**, int);
int (*stat)(Chan*, uchar*, int);
Chan* (*open)(Chan*, int);
void (*create)(Chan*, char*, int, ulong);
void (*close)(Chan*);
long (*read)(Chan*, void*, long, vlong);
Block* (*bread)(Chan*, long, ulong);
long (*write)(Chan*, void*, long, vlong);
long (*bwrite)(Chan*, Block*, ulong);
void (*remove)(Chan*);
int (*wstat)(Chan*, uchar*, int);
void (*power)(int);
int (*config)(int, char*, DevConf*);
};
struct Dirtab
{
char name[KNAMELEN];
Qid qid;
vlong length;
long perm;
};
struct Walkqid
{
Chan *clone;
int nqid;
Qid qid[1];
};
enum
{
NSMAX = 1000,
NSLOG = 7,
NSCACHE = (1<<NSLOG),
};
struct Mntwalk
{
int cddone;
ulong id;
Mhead* mh;
Mount* cm;
};
struct Mount
{
ulong mountid;
Mount* next;
Mhead* head;
Mount* copy;
Mount* order;
Chan* to;
int mflag;
char *spec;
};
struct Mhead
{
Ref;
RWlock lock;
Chan* from;
Mount* mount;
Mhead* hash;
};
struct Mnt
{
Lock;
Chan *c;
Proc *rip;
Mntrpc *queue;
ulong id;
Mnt *list;
int flags;
int msize;
char *version;
Queue *q;
};
enum
{
RENDLOG = 5,
RENDHASH = 1<<RENDLOG,
MNTLOG = 5,
MNTHASH = 1<<MNTLOG,
DELTAFD= 20,
MAXNFD = 4000,
MAXKEY = 8,
};
#define MOUNTH(p,qid) ((p)->mnthash[(qid).path&((1<<MNTLOG)-1)])
struct Mntparam {
Chan* chan;
Chan* authchan;
char* spec;
int flags;
};
struct Pgrp
{
Ref;
ulong pgrpid;
QLock debug;
RWlock ns;
QLock nsh;
Mhead* mnthash[MNTHASH];
int progmode;
Chan* dot;
Chan* slash;
int nodevs;
int pin;
};
struct Fgrp
{
Lock;
Ref;
Chan** fd;
int nfd;
int maxfd;
int minfd;
};
struct Evalue
{
char *var;
char *val;
int len;
Qid qid;
Evalue *next;
};
struct Egrp
{
Ref;
QLock;
Evalue *entries;
ulong path;
ulong vers;
};
struct Signerkey
{
Ref;
char* owner;
ushort footprint;
ulong expires;
void* alg;
void* pk;
void (*pkfree)(void*);
};
struct Skeyset
{
Ref;
QLock;
ulong flags;
char* devs;
int nkey;
Signerkey *keys[MAXKEY];
};
enum {
Trelative,
Tabsolute,
Tperiodic,
};
struct Timer
{
int tmode;
vlong tns;
void (*tf)(Ureg*, Timer*);
void *ta;
Lock;
Timers *tt;
vlong twhen;
Timer *tnext;
};
enum
{
Dead = 0,
Moribund,
Ready,
Scheding,
Running,
Queueing,
Wakeme,
Broken,
Stopped,
Rendezvous,
Waitrelease,
Proc_stopme = 1,
Proc_exitme,
Proc_traceme,
Proc_exitbig,
NERR = 30,
Unknown = 0,
IdleGC,
Interp,
BusyGC,
PriLock = 0,
PriEdf,
PriRelease,
PriRealtime,
PriHicodec,
PriLocodec,
PriHi,
PriNormal,
PriLo,
PriBackground,
PriExtra,
Nrq
};
struct Proc
{
Label sched;
char* kstack;
Mach* mach;
char text[KNAMELEN];
Proc* rnext;
Proc* qnext;
QLock* qlock;
int state;
int type;
void* prog;
void* iprog;
Osenv* env;
Osenv defenv;
int swipend;
Lock sysio;
char* psstate;
ulong pid;
int fpstate;
int procctl;
ulong pc;
Lock rlock;
Rendez* r;
Rendez sleep;
int killed;
int kp;
ulong alarm;
int pri;
ulong twhen;
Rendez* trend;
Proc* tlink;
int (*tfn)(void*);
void (*kpfun)(void*);
void* arg;
FPU fpsave;
int scallnr;
int nerrlab;
Label errlab[NERR];
char genbuf[128];
Mach* mp;
Mach* wired;
ulong movetime;
ulong delaysched;
int preempted;
ulong qpc;
void* dbgreg;
int dbgstop;
Edf* edf;
CognitiveState* cognitive;
};
enum
{
KPDUPPG = (1<<0),
KPDUPFDG = (1<<1),
KPDUPENVG = (1<<2),
KPDUP = KPDUPPG | KPDUPFDG | KPDUPENVG
};
enum {
BrkSched,
BrkNoSched,
};
struct BkptCond
{
uchar op;
ulong val;
BkptCond *next;
};
struct Bkpt
{
int id;
ulong addr;
BkptCond *conditions;
Instr instr;
void (*handler)(Bkpt*);
void *aux;
Bkpt *next;
Bkpt *link;
};
enum
{
PRINTSIZE = 256,
NUMSIZE = 12,
MB = (1024*1024),
READSTR = 1000,
};
extern Conf conf;
extern char* conffile;
extern int consoleprint;
extern Dev* devtab[];
extern char* eve;
extern int hwcurs;
extern FPU initfp;
extern Queue *kbdq;
extern Queue *kscanq;
extern Ref noteidalloc;
extern Queue *printq;
extern uint qiomaxatomic;
extern char* statename[];
extern char* sysname;
extern Talarm talarm;
extern OpenCogKernel opencog_kernel;
extern Lock opencog_lock;
struct Log {
Lock;
int opens;
char* buf;
char *end;
char *rptr;
int len;
int nlog;
int minread;
int logmask;
QLock readq;
Rendez readr;
};
struct Logflag {
char* name;
int mask;
};
struct Cmdbuf
{
char *buf;
char **f;
int nf;
};
struct Cmdtab
{
int index;
char *cmd;
int narg;
};
enum
{
MAXPOOL = 8,
};
extern Pool* mainmem;
extern Pool* heapmem;
extern Pool* imagmem;
enum
{
Qstarve = (1<<0),
Qmsg = (1<<1),
Qclosed = (1<<2),
Qflow = (1<<3),
Qcoalesce = (1<<4),
Qkick = (1<<5),
};
#define DEVDOTDOT -1
enum AtomType
{
NODE_ATOM = 0,
LINK_ATOM,
CONCEPT_NODE,
PREDICATE_NODE,
EVALUATION_LINK,
INHERITANCE_LINK,
SIMILARITY_LINK,
IMPLICATION_LINK,
EXECUTION_LINK,
PROCEDURAL_ATOM,
GOAL_ATOM,
SATISFACTION_LINK
};
struct TruthValue
{
float strength;
float confidence;
float count;
};
struct Atom
{
ulong id;
int type;
char* name;
struct TruthValue tv;
struct Atom** outgoing;
int arity;
struct Atom* next;
Lock;
};
struct AtomSpace
{
Lock;
Atom** atoms;
int natoms;
int maxatoms;
ulong next_id;
struct AtomSpace* parent;
struct AtomSpace** children;
int nchildren;
};
struct Goal
{
ulong id;
char* description;
float urgency;
float importance;
struct TruthValue satisfaction;
struct Atom* target;
struct Goal* subgoals;
struct Goal* next;
vlong created;
vlong deadline;
Lock;
};
struct PatternMatcher
{
Lock;
struct Atom** patterns;
int npatterns;
float (*similarity)(struct Atom*, struct Atom*);
int (*unify)(struct Atom*, struct Atom*, struct Atom***);
};
struct ReasoningEngine
{
Lock;
struct AtomSpace* atomspace;
struct PatternMatcher* pm;
struct Goal* goals;
int inference_steps;
float confidence_threshold;
vlong last_cycle;
};
struct CognitiveState
{
Lock;
struct AtomSpace* local_space;
struct Goal* active_goals;
struct ReasoningEngine* reasoner;
float attention_level;
float motivation;
struct Atom* context;
vlong think_time;
int cognitive_load;
};
struct OpenCogKernel
{
Lock;
struct AtomSpace* global_space;
struct ReasoningEngine* global_reasoner;
struct Goal* system_goals;
struct PatternMatcher* pm;
int cognitive_processes;
vlong total_atoms;
vlong reasoning_cycles;
float system_attention;
int distributed_nodes;
};
#pragma varargck argpos print 1
#pragma varargck argpos snprint 3
#pragma varargck argpos seprint 3
#pragma varargck argpos sprint 2
#pragma varargck argpos fprint 2
#pragma varargck argpos iprint 1
#pragma varargck argpos panic 1
#pragma varargck argpos kwerrstr 1
#pragma varargck argpos kprint 1
#pragma varargck type "lld" vlong
#pragma varargck type "llx" vlong
#pragma varargck type "lld" uvlong
#pragma varargck type "llx" uvlong
#pragma varargck type "lx" void*
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
#pragma varargck type "f" double
#pragma varargck type "e" double
#pragma varargck type "g" double
#pragma varargck type "s" char*
#pragma varargck type "S" Rune*
#pragma varargck type "r" void
#pragma varargck type "%" void
#pragma varargck type "I" uchar*
#pragma varargck type "V" uchar*
#pragma varargck type "E" uchar*
#pragma varargck type "M" uchar*
#pragma varargck type "p" void*
#pragma varargck type "q" char*