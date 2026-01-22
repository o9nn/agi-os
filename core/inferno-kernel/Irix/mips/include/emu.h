extern	Proc**	Xup;
#define	up	(*Xup)
typedef	struct	FPU	FPU;
struct FPU
{
ulong	fcr31;
};
typedef sigjmp_buf osjmpbuf;
#define	ossetjmp(buf)	sigsetjmp(buf, 1)