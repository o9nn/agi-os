struct Ureg
{
ulong	di;
ulong	si;
ulong	bp;
ulong	nsp;
ulong	bx;
ulong	dx;
ulong	cx;
ulong	ax;
ulong	gs;
ulong	fs;
ulong	es;
ulong	ds;
ulong	trap;
ulong	ecode;
ulong	pc;
ulong	cs;
ulong	flags;
union {
ulong	usp;
ulong	sp;
} u0;
ulong	ss;
};