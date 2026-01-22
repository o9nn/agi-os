#pragma lib "libhttpd.a"
#pragma src "/sys/src/libhttpd"
typedef struct HConnect HConnect;
typedef struct HContent HContent;
typedef struct HContents HContents;
typedef struct HETag HETag;
typedef struct HFields HFields;
typedef struct Hio Hio;
typedef struct Htmlesc Htmlesc;
typedef struct HttpHead HttpHead;
typedef struct HttpReq HttpReq;
typedef struct HRange HRange;
typedef struct HSPairs HSPairs;
typedef struct Bin Bin;
#pragma incomplete Bin
enum
{
HMaxWord = 32*1024,
HBufSize = 32*1024,
HInternal = 0,
HTempFail,
HUnimp,
HBadReq,
HBadSearch,
HNotFound,
HUnauth,
HSyntax,
HNoSearch,
HNoData,
HExpectFail,
HUnkVers,
HBadCont,
HOK,
};
struct Htmlesc
{
char *name;
Rune value;
};
struct HContent
{
HContent *next;
char *generic;
char *specific;
float q;
int mxb;
};
struct HContents
{
HContent *type;
HContent *encoding;
};
struct HFields
{
char *s;
HSPairs *params;
HFields *next;
};
struct HSPairs
{
char *s;
char *t;
HSPairs *next;
};
struct HRange
{
int suffix;
ulong start;
ulong stop;
HRange *next;
};
struct HETag
{
char *etag;
int weak;
HETag *next;
};
enum
{
Hnone,
Hread,
Hend,
Hwrite,
Herr,
Hsize = HBufSize
};
struct Hio {
Hio *hh;
int fd;
ulong seek;
uchar state;
uchar xferenc;
uchar *pos;
uchar *stop;
uchar *start;
ulong bodylen;
uchar buf[Hsize+32];
};
struct HttpReq
{
char *meth;
char *uri;
char *urihost;
char *search;
int vermaj;
int vermin;
HSPairs *searchpairs;
};
struct HttpHead
{
int closeit;
uchar persist;
uchar expectcont;
uchar expectother;
ulong contlen;
HFields *transenc;
char *client;
char *host;
HContent *okencode;
HContent *oklang;
HContent *oktype;
HContent *okchar;
ulong ifmodsince;
ulong ifunmodsince;
ulong ifrangedate;
HETag *ifmatch;
HETag *ifnomatch;
HETag *ifrangeetag;
HRange *range;
char *authuser;
char *authpass;
HSPairs *cookie;
HSPairs *authinfo;
int fresh_thresh;
int fresh_have;
};
struct HConnect
{
void *private;
void (*replog)(HConnect*, char*, ...);
char *scheme;
char *port;
HttpReq req;
HttpHead head;
Bin *bin;
ulong reqtime;
char xferbuf[HBufSize];
uchar header[HBufSize + 2];
uchar *hpos;
uchar *hstop;
Hio hin;
Hio hout;
};
extern char* hmydomain;
extern char* hversion;
extern Htmlesc htmlesc[];
void *halloc(HConnect *c, ulong size);
Hio *hbodypush(Hio *hh, ulong len, HFields *te);
int hbuflen(Hio *h, void *p);
int hcheckcontent(HContent*, HContent*, char*, int);
void hclose(Hio*);
ulong hdate2sec(char*);
int hdatefmt(Fmt*);
int hfail(HConnect*, int, ...);
int hflush(Hio*);
int hgetc(Hio*);
int hgethead(HConnect *c, int many);
int hinit(Hio*, int, int);
int hiserror(Hio *h);
int hlflush(Hio*);
int hload(Hio*, char*);
char *hlower(char*);
HContent *hmkcontent(HConnect *c, char *generic, char *specific, HContent *next);
HFields *hmkhfields(HConnect *c, char *s, HSPairs *p, HFields *next);
char *hmkmimeboundary(HConnect *c);
HSPairs *hmkspairs(HConnect *c, char *s, char *t, HSPairs *next);
int hmoved(HConnect *c, char *uri);
void hokheaders(HConnect *c);
int hparseheaders(HConnect*, int timeout);
HSPairs *hparsequery(HConnect *c, char *search);
int hparsereq(HConnect *c, int timeout);
int hprint(Hio*, char*, ...);
int hputc(Hio*, int);
void *hreadbuf(Hio *h, void *vsave);
int hredirected(HConnect *c, char *how, char *uri);
void hreqcleanup(HConnect *c);
HFields *hrevhfields(HFields *hf);
HSPairs *hrevspairs(HSPairs *sp);
char *hstrdup(HConnect *c, char *s);
int http11(HConnect*);
int httpfmt(Fmt*);
char *httpunesc(HConnect *c, char *s);
int hunallowed(HConnect *, char *allowed);
int hungetc(Hio *h);
char *hunload(Hio*);
int hurlfmt(Fmt*);
char *hurlunesc(HConnect *c, char *s);
int hwrite(Hio*, void*, int);
int hxferenc(Hio*, int);
#pragma varargck argpos hprint 2
#pragma varargck type "D" long
#pragma varargck type "D" ulong
#pragma varargck type "U" char*
#pragma varargck type "H" char*