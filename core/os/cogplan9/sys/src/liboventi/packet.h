typedef struct Packet Packet;
typedef struct Mem Mem;
typedef struct Frag Frag;
enum {
BigMemSize = MaxFragSize,
SmallMemSize = BigMemSize/8,
NLocalFrag = 2,
};
enum {
PFront,
PMiddle,
PEnd,
};
struct Mem
{
Lock lk;
int ref;
uchar *bp;
uchar *ep;
uchar *rp;
uchar *wp;
Mem *next;
};
enum {
FragLocalFree,
FragLocalAlloc,
FragGlobal,
};
struct Frag
{
int state;
Mem *mem;
uchar *rp;
uchar *wp;
Frag *next;
};
struct Packet
{
int size;
int asize;
Packet *next;
Frag *first;
Frag *last;
Frag local[NLocalFrag];
};