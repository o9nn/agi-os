typedef struct Thwack		Thwack;
typedef struct Unthwack		Unthwack;
typedef struct ThwBlock		ThwBlock;
typedef struct UnthwBlock	UnthwBlock;
enum
{
ThwStats	= 8,
ThwErrLen	= 64,
ThwMaxBlock	= 1600,
HashLog		= 12,
HashSize	= 1<<HashLog,
HashMask	= HashSize - 1,
MinMatch	= 3,
MaxOff		= 8,
OffBase		= 6,
MinDecode	= 8,
CompBlocks	= 10,
EWinBlocks	= 64,
DWinBlocks	= EWinBlocks,
MaxSeqMask	= 8,
MaxSeqStart	= 256
};
struct ThwBlock
{
ulong	seq;
uchar	acked;
ushort	begin;
uchar	*edata;
ushort	maxoff;
ushort	*hash;
uchar	*data;
};
struct Thwack
{
QLock		acklock;
int		slot;
ThwBlock	blocks[EWinBlocks];
ushort		hash[EWinBlocks][HashSize];
Block		*data[EWinBlocks];
};
struct UnthwBlock
{
ulong	seq;
ushort	maxoff;
uchar	*data;
};
struct Unthwack
{
int		slot;
char		err[ThwErrLen];
UnthwBlock	blocks[DWinBlocks];
uchar		data[DWinBlocks][ThwMaxBlock];
};
void	thwackinit(Thwack*);
void	thwackcleanup(Thwack *tw);
void	unthwackinit(Unthwack*);
int	thwack(Thwack*, int mustadd, uchar *dst, int ndst, Block *bsrc, ulong seq, ulong stats[ThwStats]);
void	thwackack(Thwack*, ulong seq, ulong mask);
int	unthwack(Unthwack*, uchar *dst, int ndst, uchar *src, int nsrc, ulong seq);
ulong	unthwackstate(Unthwack *ut, uchar *mask);
int	unthwackadd(Unthwack *ut, uchar *src, int nsrc, ulong seq);