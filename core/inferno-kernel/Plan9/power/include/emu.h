extern	Proc**	Xup;
#define	up	(*Xup)
typedef	struct	FPU	FPU;
typedef struct FPU FPU;
struct FPU
{
double	fpreg[32];
union {
double	fpscrd;
struct {
ulong	pad;
ulong	fpscr;
};
};
};
typedef jmp_buf osjmpbuf;
#define	ossetjmp(buf)	setjmp(buf)