extern	Proc**	Xup;
#define	up	(*Xup)
typedef	struct	FPU	FPU;
struct FPU
{
uchar	env[28];
};
typedef jmp_buf osjmpbuf;
#define	ossetjmp(buf)	setjmp(buf)