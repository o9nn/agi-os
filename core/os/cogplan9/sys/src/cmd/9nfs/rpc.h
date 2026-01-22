enum Bool
{
FALSE	= 0,
TRUE	= 1
};
enum Auth_flavor
{
AUTH_NULL	= 0,
AUTH_UNIX	= 1,
AUTH_SHORT	= 2,
AUTH_DES	= 3
};
enum Msg_type
{
CALL	= 0,
REPLY	= 1
};
enum Reply_stat
{
MSG_ACCEPTED	= 0,
MSG_DENIED	= 1
};
enum Accept_stat
{
SUCCESS		= 0,
PROG_UNAVAIL	= 1,
PROG_MISMATCH	= 2,
PROC_UNAVAIL	= 3,
GARBAGE_ARGS	= 4
};
enum Reject_stat
{
RPC_MISMATCH	= 0,
AUTH_ERROR	= 1
};
enum Auth_stat
{
AUTH_BADCRED		= 1,
AUTH_REJECTEDCRED	= 2,
AUTH_BADVERF		= 3,
AUTH_REJECTEDVERF	= 4,
AUTH_TOOWEAK		= 5
};
enum
{
IPPROTO_TCP	= 6,
IPPROTO_UDP	= 17
};
#define	ROUNDUP(n)	((n) + ((-(n))&3))
#define	PLONG(x)	(dataptr[3] = ((ulong)(x)), dataptr[2] = ((ulong)(x))>>8, dataptr[1] = ((ulong)(x))>>16, dataptr[0] = ((ulong)(x))>>24, dataptr += 4)
#define	PPTR(x, n)	(memmove(dataptr, (x), n), dataptr += ROUNDUP(n))
#define	PBYTE(x)	(*dataptr++ = (x))
#define	GLONG()		(argptr += 4, (((uchar*)argptr)[-1] | (((uchar*)argptr)[-2]<<8) | (((uchar*)argptr)[-3]<<16) | (((uchar*)argptr)[-4]<<24)))
#define	GPTR(n)		(void *)(argptr); argptr += ROUNDUP(n)
#define	GBYTE()	(argptr++, ((uchar*)argptr)[-1])