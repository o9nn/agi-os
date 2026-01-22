extern void *bbmalloc(int);
extern void bbfree(void *, int);
extern int bbonstack(void);
extern void bbexec(void(*)(void), int, int);
typedef struct GBitmap GBitmap;
typedef struct GFont GFont;
typedef struct GSubfont GSubfont;
typedef struct GCacheinfo GCacheinfo;
struct GBitmap
{
ulong *base;
long zero;
ulong width;
int ldepth;
Rectangle r;
Rectangle clipr;
GBitmap *cache;
};
struct GSubfont
{
short n;
char height;
char ascent;
Fontchar *info;
GBitmap *bits;
};
struct GCacheinfo
{
ulong xright;
Fontchar;
};
struct GFont
{
uchar height;
char ascent;
char width;
char ldepth;
short id;
int ncache;
GCacheinfo *cache;
GBitmap *b;
};
extern ulong *gaddr(GBitmap*, Point);
extern uchar *gbaddr(GBitmap*, Point);
extern void gbitblt(GBitmap*, Point, GBitmap*, Rectangle, Fcode);
extern void gbitbltclip(void*);
extern void gtexture(GBitmap*, Rectangle, GBitmap*, Fcode);
extern Point gsubfstrsize(GSubfont*, char*);
extern int gsubfstrwidth(GSubfont*, char*);
extern Point gsubfstring(GBitmap*, Point, GSubfont*, char*, Fcode);
extern Point gbitbltstring(GBitmap*, Point, GSubfont*, char*, Fcode);
extern void gsegment(GBitmap*, Point, Point, int, Fcode);
extern void gpoint(GBitmap*, Point, int, Fcode);
extern void gflushcpucache(void);
extern GBitmap* gballoc(Rectangle, int);
extern void gbfree(GBitmap*);