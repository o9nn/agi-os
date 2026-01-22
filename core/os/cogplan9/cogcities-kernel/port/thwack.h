typedef struct Thwack Thwack;
typedef struct Unthwack Unthwack;
typedef struct ThwBlock ThwBlock;
typedef struct UnthwBlock UnthwBlock;
enum
{
ThwStats = 8,
ThwMaxBlock = 1600,
HashLog = 12,
HashSize = 1<<HashLog,
HashMask = HashSize - 1,
MinMatch = 3,
MaxOff = 8,
OffBase = 6,
MinDecode = 8,
EWinBlocks = 22,
DWinBlocks = 32,
CompBlocks = 10,
MaxSeqMask = 8,
MaxSeqStart = 256
};
struct ThwBlock
{
ulong seq;
uchar acked;
ushort begin;
uchar *edata;
ushort maxoff;
ushort *hash;
uchar *data;
};
struct Thwack
{
int slot;
ThwBlock blocks[EWinBlocks];
ushort hash[EWinBlocks][HashSize];
uchar data[EWinBlocks][ThwMaxBlock];
};
struct UnthwBlock
{
ulong seq;
ushort maxoff;
uchar *data;
};
struct Unthwack
{
int slot;
UnthwBlock blocks[DWinBlocks];
uchar data[DWinBlocks][ThwMaxBlock];
};
void thwackinit(Thwack*);
void unthwackinit(Unthwack*);
int thwack(Thwack*, uchar *dst, uchar *src, int nsrc, ulong seq, ulong stats[ThwStats]);
void thwackack(Thwack*, ulong seq, ulong mask);
int unthwack(Unthwack*, uchar *dst, int ndst, uchar *src, int nsrc, ulong seq);
ulong unthwackstate(Unthwack *ut, uchar *mask);