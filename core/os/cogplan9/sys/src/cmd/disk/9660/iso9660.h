typedef struct Cdimg Cdimg;
typedef struct Cdinfo Cdinfo;
typedef struct Conform Conform;
typedef struct Direc Direc;
typedef struct Dumproot Dumproot;
typedef struct Voldesc Voldesc;
typedef struct XDir XDir;
#ifndef CHLINK
#define CHLINK 0
#endif
struct XDir {
char *name;
char *uid;
char *gid;
char *symlink;
ulong uidno;
ulong gidno;
ulong mode;
ulong atime;
ulong mtime;
ulong ctime;
vlong length;
};
struct Direc {
char *name;
char *confname;
char *srcfile;
ulong block;
ulong length;
int flags;
char *uid;
char *gid;
char *symlink;
ulong mode;
long atime;
long ctime;
long mtime;
ulong uidno;
ulong gidno;
Direc *child;
int nchild;
};
enum {
Dbadname = 1<<0,
};
struct Voldesc {
char *systemid;
char *volumeset;
char *publisher;
char *preparer;
char *application;
char *abstract;
char *biblio;
char *notice;
ulong pathsize;
ulong lpathloc;
ulong mpathloc;
Direc root;
};
struct Cdimg {
char *file;
int fd;
ulong dumpblock;
ulong nextblock;
ulong iso9660pvd;
ulong jolietsvd;
ulong pathblock;
uvlong rrcontin;
ulong nulldump;
ulong nconform;
uvlong bootcatptr;
ulong bootcatblock;
uvlong bootimageptr;
Direc *loaderdirec;
Direc *bootdirec;
char *bootimage;
char *loader;
Biobuf brd;
Biobuf bwr;
int flags;
Voldesc iso;
Voldesc joliet;
};
enum {
CDjoliet = 1<<0,
CDplan9 = 1<<1,
CDconform = 1<<2,
CDrockridge = 1<<3,
CDnew = 1<<4,
CDdump = 1<<5,
CDbootable = 1<<6,
CDbootnoemu = 1<<7,
CDpbs= 1<<8,
};
typedef struct Tx Tx;
struct Tx {
char *bad;
char *good;
};
struct Conform {
Tx *t;
int nt;
};
struct Cdinfo {
int flags;
char *volumename;
char *volumeset;
char *publisher;
char *preparer;
char *application;
char *bootimage;
char *loader;
};
typedef struct Dump Dump;
typedef struct Dumpdir Dumpdir;
struct Dump {
Cdimg *cd;
Dumpdir *md5root;
Dumpdir *blockroot;
};
struct Dumpdir {
char *name;
uchar md5[MD5dlen];
ulong block;
ulong length;
Dumpdir *md5left;
Dumpdir *md5right;
Dumpdir *blockleft;
Dumpdir *blockright;
};
struct Dumproot {
char *name;
int nkid;
Dumproot *kid;
Direc root;
Direc jroot;
};
typedef struct Cdir Cdir;
typedef struct Cpath Cpath;
typedef struct Cvoldesc Cvoldesc;
struct Cvoldesc {
uchar magic[8];
uchar systemid[32];
uchar volumeid[32];
uchar unused[8];
uchar volsize[8];
uchar charset[32];
uchar volsetsize[4];
uchar volseqnum[4];
uchar blocksize[4];
uchar pathsize[8];
uchar lpathloc[4];
uchar olpathloc[4];
uchar mpathloc[4];
uchar ompathloc[4];
uchar rootdir[34];
uchar volumeset[128];
uchar publisher[128];
uchar preparer[128];
uchar application[128];
uchar notice[37];
uchar abstract[37];
uchar biblio[37];
uchar cdate[17];
uchar mdate[17];
uchar xdate[17];
uchar edate[17];
uchar fsvers;
};
struct Cdir {
uchar len;
uchar xlen;
uchar dloc[8];
uchar dlen[8];
uchar date[7];
uchar flags;
uchar unitsize;
uchar gapsize;
uchar volseqnum[4];
uchar namelen;
uchar name[1];
};
struct Cpath {
uchar namelen;
uchar xlen;
uchar dloc[4];
uchar parent[2];
uchar name[1];
};
enum {
RR_PX = 1<<0,
RR_PN = 1<<1,
RR_SL = 1<<2,
RR_NM = 1<<3,
RR_CL = 1<<4,
RR_PL = 1<<5,
RR_RE = 1<<6,
RR_TF = 1<<7,
};
enum {
TFcreation = 1<<0,
TFmodify = 1<<1,
TFaccess = 1<<2,
TFattributes = 1<<3,
TFbackup = 1<<4,
TFexpiration = 1<<5,
TFeffective = 1<<6,
TFlongform = 1<<7,
};
enum {
NMcontinue = 1<<0,
NMcurrent = 1<<1,
NMparent = 1<<2,
NMroot = 1<<3,
NMvolroot = 1<<4,
NMhost = 1<<5,
};
void Cputbootvol(Cdimg*);
void Cputbootcat(Cdimg*);
void Cupdatebootvol(Cdimg*);
void Cupdatebootcat(Cdimg*);
void Cfillpbs(Cdimg*);
void findbootimage(Cdimg*, Direc*);
void findloader(Cdimg*, Direc*);
Cdimg *createcd(char*, Cdinfo);
Cdimg *opencd(char*, Cdinfo);
void Creadblock(Cdimg*, void*, ulong, ulong);
ulong big(void*, int);
ulong little(void*, int);
int parsedir(Cdimg*, Direc*, uchar*, int, char *(*)(uchar*, int));
void setroot(Cdimg*, ulong, ulong, ulong);
void setvolsize(Cdimg*, uvlong, ulong);
void setpathtable(Cdimg*, ulong, ulong, ulong, ulong);
void Cputc(Cdimg*, int);
void Cputnl(Cdimg*, uvlong, int);
void Cputnm(Cdimg*, uvlong, int);
void Cputn(Cdimg*, uvlong, int);
void Crepeat(Cdimg*, int, int);
void Cputs(Cdimg*, char*, int);
void Cwrite(Cdimg*, void*, int);
void Cputr(Cdimg*, Rune);
void Crepeatr(Cdimg*, Rune, int);
void Cputrs(Cdimg*, Rune*, int);
void Cputrscvt(Cdimg*, char*, int);
void Cpadblock(Cdimg*);
void Cputdate(Cdimg*, ulong);
void Cputdate1(Cdimg*, ulong);
void Cread(Cdimg*, void*, int);
void Cwflush(Cdimg*);
void Cwseek(Cdimg*, vlong);
uvlong Cwoffset(Cdimg*);
uvlong Croffset(Cdimg*);
int Cgetc(Cdimg*);
void Crseek(Cdimg*, vlong);
char *Crdline(Cdimg*, int);
int Clinelen(Cdimg*);
void rdconform(Cdimg*);
char *conform(char*, int);
void wrconform(Cdimg*, int, ulong*, uvlong*);
void mkdirec(Direc*, XDir*);
Direc *walkdirec(Direc*, char*);
Direc *adddirec(Direc*, char*, XDir*);
void copydirec(Direc*, Direc*);
void checknames(Direc*, int (*)(char*));
void convertnames(Direc*, char* (*)(char*, char*));
void dsort(Direc*, int (*)(const void*, const void*));
void setparents(Direc*);
ulong Cputdumpblock(Cdimg*);
int hasdump(Cdimg*);
Dump *dumpcd(Cdimg*, Direc*);
Dumpdir *lookupmd5(Dump*, uchar*);
void insertmd5(Dump*, char*, uchar*, ulong, ulong);
Direc readdumpdirs(Cdimg*, XDir*, char*(*)(uchar*,int));
char *adddumpdir(Direc*, ulong, XDir*);
void copybutname(Direc*, Direc*);
void readkids(Cdimg*, Direc*, char*(*)(uchar*,int));
void freekids(Direc*);
void readdumpconform(Cdimg*);
void rmdumpdir(Direc*, char*);
char *isostring(uchar*, int);
int isbadiso9660(char*);
int isocmp(const void*, const void*);
int isisofrog(char);
void Cputisopvd(Cdimg*, Cdinfo);
char *jolietstring(uchar*, int);
int isbadjoliet(char*);
int jolietcmp(const void*, const void*);
int isjolietfrog(Rune);
void Cputjolietsvd(Cdimg*, Cdinfo);
void writepathtables(Cdimg*);
void *emalloc(ulong);
void *erealloc(void*, ulong);
char *atom(char*);
char *struprcpy(char*, char*);
int chat(char*, ...);
void dirtoxdir(XDir*, Dir*);
void fdtruncate(int, ulong);
long uidno(char*);
long gidno(char*);
Rune *strtorune(Rune*, char*);
Rune *runechr(Rune*, Rune);
int runecmp(Rune*, Rune*);
int Cputsysuse(Cdimg*, Direc*, int, int, int);
void writefiles(Dump*, Cdimg*, Direc*);
void writedirs(Cdimg*, Direc*, int(*)(Cdimg*, Direc*, int, int, int));
void writedumpdirs(Cdimg*, Direc*, int(*)(Cdimg*, Direc*, int, int, int));
int Cputisodir(Cdimg*, Direc*, int, int, int);
int Cputjolietdir(Cdimg*, Direc*, int, int, int);
void Cputendvd(Cdimg*);
enum {
Blocksize = 2048,
Ndirblock = 16,
DTdot = 0,
DTdotdot,
DTiden,
DTroot,
DTrootdot,
};
extern ulong now;
extern Conform *map;
extern int chatty;
extern int docolon;
extern int mk9660;
extern int blocksize;