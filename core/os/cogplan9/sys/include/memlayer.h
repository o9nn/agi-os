#pragma src "/sys/src/libmemlayer"
#pragma lib "libmemlayer.a"
typedef struct Memscreen Memscreen;
typedef void (*Refreshfn)(Memimage*, Rectangle, void*);
struct Memscreen
{
Memimage *frontmost;
Memimage *rearmost;
Memimage *image;
Memimage *fill;
};
struct Memlayer
{
Rectangle screenr;
Point delta;
Memscreen *screen;
Memimage *front;
Memimage *rear;
int clear;
Memimage *save;
Refreshfn refreshfn;
void *refreshptr;
};
int memload(Memimage*, Rectangle, uchar*, int, int);
int memunload(Memimage*, Rectangle, uchar*, int);
void _memlayerop(void (*fn)(Memimage*, Rectangle, Rectangle, void*, int), Memimage*, Rectangle, Rectangle, void*);
Memimage* memlalloc(Memscreen*, Rectangle, Refreshfn, void*, ulong);
void memldelete(Memimage*);
void memlfree(Memimage*);
void memltofront(Memimage*);
void memltofrontn(Memimage**, int);
void _memltofrontfill(Memimage*, int);
void memltorear(Memimage*);
void memltorearn(Memimage**, int);
int memlsetrefresh(Memimage*, Refreshfn, void*);
void memlhide(Memimage*, Rectangle);
void memlexpose(Memimage*, Rectangle);
void _memlsetclear(Memscreen*);
int memlorigin(Memimage*, Point, Point);
void memlnorefresh(Memimage*, Rectangle, void*);