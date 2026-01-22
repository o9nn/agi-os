typedef struct Whack Whack;
typedef struct Unwhack Unwhack;
enum
{
WhackStats = 8,
WhackErrLen = 64,
WhackMaxOff = 16*1024,
HashLog = 14,
HashSize = 1<<HashLog,
HashMask = HashSize - 1,
MinMatch = 3,
MinDecode = 8,
MaxSeqMask = 8,
MaxSeqStart = 256
};
struct Whack
{
ushort begin;
ushort hash[HashSize];
ushort next[WhackMaxOff];
uchar *data;
};
struct Unwhack
{
char err[WhackErrLen];
};
void whackinit(Whack*, int level);
void unwhackinit(Unwhack*);
int whack(Whack*, uchar *dst, uchar *src, int nsrc, ulong stats[WhackStats]);
int unwhack(Unwhack*, uchar *dst, int ndst, uchar *src, int nsrc);
int whackblock(uchar *dst, uchar *src, int ssize);