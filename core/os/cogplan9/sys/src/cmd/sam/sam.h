#include <u.h>
#include <libc.h>
#include <plumb.h>
#include "errors.h"
#define BLOCKSIZE 2048
#define RUNESIZE sizeof(Rune)
#define NDISC 5
#define NBUFFILES 3+2*NDISC
#define NSUBEXP 10
#define TRUE 1
#define FALSE 0
#define INFINITY 0x7FFFFFFFL
#define INCR 25
#define STRSIZE (2*BLOCKSIZE)
typedef long Posn;
typedef ushort Mod;
typedef struct Address Address;
typedef struct Block Block;
typedef struct Buffer Buffer;
typedef struct Disk Disk;
typedef struct Discdesc Discdesc;
typedef struct File File;
typedef struct List List;
typedef struct Range Range;
typedef struct Rangeset Rangeset;
typedef struct String String;
enum State
{
Clean = ' ',
Dirty = '\'',
Unread = '-',
};
struct Range
{
Posn p1, p2;
};
struct Rangeset
{
Range p[NSUBEXP];
};
struct Address
{
Range r;
File *f;
};
struct String
{
short n;
short size;
Rune *s;
};
struct List
{
int type;
int nalloc;
int nused;
union{
void* listp;
void** voidp;
Posn* posnp;
String**stringp;
File** filep;
}g;
};
#define listptr g.listp
#define voidpptr g.voidp
#define posnptr g.posnp
#define stringpptr g.stringp
#define filepptr g.filep
enum
{
Blockincr = 256,
Maxblock = 8*1024,
BUFSIZE = Maxblock,
RBUFSIZE = BUFSIZE/sizeof(Rune),
};
enum
{
Null = '-',
Delete = 'd',
Insert = 'i',
Filename = 'f',
Dot = 'D',
Mark = 'm',
};
struct Block
{
uint addr;
union
{
uint n;
Block *next;
};
};
struct Disk
{
int fd;
uint addr;
Block *free[Maxblock/Blockincr+1];
};
Disk* diskinit(void);
Block* disknewblock(Disk*, uint);
void diskrelease(Disk*, Block*);
void diskread(Disk*, Block*, Rune*, uint);
void diskwrite(Disk*, Block**, Rune*, uint);
struct Buffer
{
uint nc;
Rune *c;
uint cnc;
uint cmax;
uint cq;
int cdirty;
uint cbi;
Block **bl;
uint nbl;
};
void bufinsert(Buffer*, uint, Rune*, uint);
void bufdelete(Buffer*, uint, uint);
uint bufload(Buffer*, uint, int, int*);
void bufread(Buffer*, uint, Rune*, uint);
void bufclose(Buffer*);
void bufreset(Buffer*);
struct File
{
Buffer;
Buffer delta;
Buffer epsilon;
String name;
uvlong qidpath;
uint mtime;
int dev;
int unread;
long seq;
long cleanseq;
int mod;
char rescuing;
Posn hiposn;
Address dot;
Address ndot;
Range tdot;
Range mark;
List *rasp;
short tag;
char closeok;
char deleted;
Range prevdot;
Range prevmark;
long prevseq;
int prevmod;
};
void fileclose(File*);
void filedelete(File*, uint, uint);
void fileinsert(File*, uint, Rune*, uint);
uint fileload(File*, uint, int, int*);
void filemark(File*);
void filereset(File*);
void filesetname(File*, String*);
void fileundelete(File*, Buffer*, uint, uint);
void fileuninsert(File*, Buffer*, uint, uint);
void fileunsetname(File*, Buffer*);
void fileundo(File*, int, int, uint*, uint*, int);
int fileupdate(File*, int, int);
int filereadc(File*, uint);
File *fileopen(void);
void loginsert(File*, uint, Rune*, uint);
void logdelete(File*, uint, uint);
void logsetname(File*, String*);
int fileisdirty(File*);
long undoseq(File*, int);
long prevseq(Buffer*);
void raspload(File*);
void raspstart(File*);
void raspdelete(File*, uint, uint, int);
void raspinsert(File*, uint, Rune*, uint, int);
void raspdone(File*, int);
void raspflush(File*);
void* fbufalloc(void);
void fbuffree(void*);
uint min(uint, uint);
void cvttorunes(char*, int, Rune*, int*, int*, int*);
#define runemalloc(a) (Rune*)emalloc((a)*sizeof(Rune))
#define runerealloc(a, b) (Rune*)realloc((a), (b)*sizeof(Rune))
#define runemove(a, b, c) memmove((a), (b), (c)*sizeof(Rune))
int alnum(int);
int Read(int, void*, int);
void Seek(int, long, int);
int plan9(File*, int, String*, int);
int Write(int, void*, int);
int bexecute(File*, Posn);
void cd(String*);
void closefiles(File*, String*);
void closeio(Posn);
void cmdloop(void);
void cmdupdate(void);
void compile(String*);
void copy(File*, Address);
File *current(File*);
void delete(File*);
void delfile(File*);
void dellist(List*, int);
void doubleclick(File*, Posn);
void dprint(char*, ...);
void edit(File*, int);
void *emalloc(ulong);
void *erealloc(void*, ulong);
void error(Err);
void error_c(Err, int);
void error_r(Err, char*);
void error_s(Err, char*);
int execute(File*, Posn, Posn);
int filematch(File*, String*);
void filename(File*);
void fixname(String*);
void fullname(String*);
void getcurwd(void);
File *getfile(String*);
int getname(File*, String*, int);
long getnum(int);
void hiccough(char*);
void inslist(List*, int, ...);
Address lineaddr(Posn, Address, int);
List *listalloc(int);
void listfree(List*);
void load(File*);
File *lookfile(String*);
void lookorigin(File*, Posn, Posn);
int lookup(int);
void move(File*, Address);
void moveto(File*, Range);
File *newfile(void);
void nextmatch(File*, String*, Posn, int);
void notifyf(void*, char*);
void panic(char*);
void printposn(File*, int);
void print_ss(char*, String*, String*);
void print_s(char*, String*);
int rcv(void);
Range rdata(List*, Posn, Posn);
Posn readio(File*, int*, int, int);
void rescue(void);
void resetcmd(void);
void resetsys(void);
void resetxec(void);
void rgrow(List*, Posn, Posn);
void samerr(char*);
void settempfile(void);
int skipbl(void);
void snarf(File*, Posn, Posn, Buffer*, int);
void sortname(File*);
void startup(char*, int, char**, char**);
void state(File*, int);
int statfd(int, ulong*, uvlong*, long*, long*, long*);
int statfile(char*, ulong*, uvlong*, long*, long*, long*);
void Straddc(String*, int);
void Strclose(String*);
int Strcmp(String*, String*);
void Strdelete(String*, Posn, Posn);
void Strdupl(String*, Rune*);
void Strduplstr(String*, String*);
void Strinit(String*);
void Strinit0(String*);
void Strinsert(String*, String*, Posn);
void Strinsure(String*, ulong);
int Strispre(String*, String*);
void Strzero(String*);
int Strlen(Rune*);
char *Strtoc(String*);
void syserror(char*);
void telldot(File*);
void tellpat(void);
String *tmpcstr(char*);
String *tmprstr(Rune*, int);
void freetmpstr(String*);
void termcommand(void);
void termwrite(char*);
File *tofile(String*);
void trytoclose(File*);
void trytoquit(void);
int undo(int);
void update(void);
char *waitfor(int);
void warn(Warn);
void warn_s(Warn, char*);
void warn_SS(Warn, String*, String*);
void warn_S(Warn, String*);
int whichmenu(File*);
void writef(File*);
Posn writeio(File*);
Discdesc *Dstart(void);
extern Rune samname[];
extern Rune *left[];
extern Rune *right[];
extern char RSAM[];
extern char SAMTERM[];
extern char HOME[];
extern char TMPDIR[];
extern char SH[];
extern char SHPATH[];
extern char RX[];
extern char RXPATH[];
extern char SAMSAVECMD[];
extern long seq;
extern Disk *disk;
extern char *rsamname;
extern char *samterm;
extern Rune genbuf[];
extern char *genc;
extern int io;
extern int patset;
extern int quitok;
extern Address addr;
extern Buffer snarfbuf;
extern Buffer plan9buf;
extern List file;
extern List tempfile;
extern File *cmd;
extern File *curfile;
extern File *lastfile;
extern Mod modnum;
extern Posn cmdpt;
extern Posn cmdptadv;
extern Rangeset sel;
extern String curwd;
extern String cmdstr;
extern String genstr;
extern String lastpat;
extern String lastregexp;
extern String plan9cmd;
extern int downloaded;
extern int eof;
extern int bpipeok;
extern int panicking;
extern Rune empty[];
extern int termlocked;
extern int outbuffered;
#include "mesg.h"
void outTs(Hmesg, int);
void outT0(Hmesg);
void outTl(Hmesg, long);
void outTslS(Hmesg, int, long, String*);
void outTS(Hmesg, String*);
void outTsS(Hmesg, int, String*);
void outTsllS(Hmesg, int, long, long, String*);
void outTsll(Hmesg, int, long, long);
void outTsl(Hmesg, int, long);
void outTsv(Hmesg, int, vlong);
void outflush(void);
int needoutflush(void);