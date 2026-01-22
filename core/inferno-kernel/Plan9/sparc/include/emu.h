extern	Proc**	Xup;
#define	up	(*Xup)
typedef	struct	FPU	FPU;
struct FPU
{
ulong   fsr;
};
typedef jmp_buf osjmpbuf;
#define	ossetjmp(buf)	setjmp(buf)