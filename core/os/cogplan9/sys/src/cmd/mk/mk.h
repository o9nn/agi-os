#include <u.h>
#include <libc.h>
#include <bio.h>
#include <regexp.h>
extern Biobuf bout;
typedef struct Bufblock
{
struct Bufblock *next;
char *start;
char *end;
char *current;
} Bufblock;
typedef struct Word
{
char *s;
struct Word *next;
} Word;
typedef struct Envy
{
char *name;
Word *values;
} Envy;
extern Envy *envy;
typedef struct Rule
{
char *target;
Word *tail;
char *recipe;
short attr;
short line;
char *file;
Word *alltargets;
int rule;
Reprog *pat;
char *prog;
struct Rule *chain;
struct Rule *next;
} Rule;
extern Rule *rules, *metarules, *patrule;
#define META 0x0001
#define UNUSED 0x0002
#define UPD 0x0004
#define QUIET 0x0008
#define VIR 0x0010
#define REGEXP 0x0020
#define NOREC 0x0040
#define DEL 0x0080
#define NOVIRT 0x0100
#define NREGEXP 10
typedef struct Arc
{
short flag;
struct Node *n;
Rule *r;
char *stem;
char *prog;
char *match[NREGEXP];
struct Arc *next;
} Arc;
#define TOGO 1
typedef struct Node
{
char *name;
ulong time;
ushort flags;
Arc *prereqs;
struct Node *next;
} Node;
#define VIRTUAL 0x0001
#define CYCLE 0x0002
#define READY 0x0004
#define CANPRETEND 0x0008
#define PRETENDING 0x0010
#define NOTMADE 0x0020
#define BEINGMADE 0x0040
#define MADE 0x0080
#define MADESET(n,m) n->flags = (n->flags&~(NOTMADE|BEINGMADE|MADE))|(m)
#define PROBABLE 0x0100
#define VACUOUS 0x0200
#define NORECIPE 0x0400
#define DELETE 0x0800
#define NOMINUSE 0x1000
typedef struct Job
{
Rule *r;
Node *n;
char *stem;
char **match;
Word *p;
Word *np;
Word *t;
Word *at;
int nproc;
struct Job *next;
} Job;
extern Job *jobs;
typedef struct Symtab
{
short space;
char *name;
union{
void *ptr;
uintptr value;
} u;
struct Symtab *next;
} Symtab;
enum {
S_VAR,
S_TARGET,
S_TIME,
S_PID,
S_NODE,
S_AGG,
S_BITCH,
S_NOEXPORT,
S_OVERRIDE,
S_OUTOFDATE,
S_MAKEFILE,
S_MAKEVAR,
S_EXPORTED,
S_BULKED,
S_WESET,
S_INTERNAL,
};
extern int debug;
extern int nflag, tflag, iflag, kflag, aflag, mflag;
extern int mkinline;
extern char *infile;
extern int nreps;
extern char *explain;
extern char *termchars;
extern char *shell;
extern char *shellname;
extern char *shflags;
extern int IWS;
#define SYNERR(l) (fprint(2, "mk: %s:%d: syntax error; ", infile, ((l)>=0)?(l):mkinline))
#define RERR(r) (fprint(2, "mk: %s:%d: rule error; ", (r)->file, (r)->line))
#define NAMEBLOCK 1000
#define BIGBLOCK 20000
#define SEP(c) (((c)==' ')||((c)=='\t')||((c)=='\n'))
#define WORDCHR(r) ((r) > ' ' && !utfrune("!\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~", (r)))
#define DEBUG(x) (debug&(x))
#define D_PARSE 0x01
#define D_GRAPH 0x02
#define D_EXEC 0x04
#define LSEEK(f,o,p) seek(f,o,p)
#define PERCENT(ch) (((ch) == '%') || ((ch) == '&'))
#include "fns.h"