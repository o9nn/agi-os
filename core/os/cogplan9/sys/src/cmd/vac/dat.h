typedef struct MetaBlock MetaBlock;
typedef struct MetaEntry MetaEntry;
#define MaxBlock (1UL<<31)
enum {
BytesPerEntry = 100,
FullPercentage = 80,
FlushSize = 200,
DirtyPercentage = 50
};
struct MetaEntry
{
uchar *p;
ushort size;
};
struct MetaBlock
{
int maxsize;
int size;
int free;
int maxindex;
int nindex;
int unbotch;
uchar *buf;
};
struct VacDirEnum
{
VacFile *file;
u32int boff;
int i, n;
VacDir *buf;
};