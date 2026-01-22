enum
{
Bufsize	= 1024,
Nbuf		= 128,
Dma		= 6,
IrqAUDIO	= 7,
SBswab	= 0,
};
#define seteisadma(a, b)	dmainit(a, Bufsize);
#define UNCACHED(type, v)	(type*)((ulong)(v))
#define Int0vec
#define setvec(v, f, a)		intrenable(v, f, a, BUSUNKNOWN, "audio")