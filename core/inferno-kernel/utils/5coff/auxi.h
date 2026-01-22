#define	COFFCVT
#define	Sym	Symx
#include "../5l/l.h"
#undef Sym
#include	<mach.h>
extern	Symx *hash[NHASH];
Symx	*lookupsym(char*, int);
void	beginsym(void);
void	endsym(void);
void	newsym(int, char*, long, int);
extern	long	autosize;
extern	Prog *firstp, *textp, *curtext, *lastp, *etextp;
void	coffhdr(void);
void	coffsym(void);
void	cofflc(void);
void	endsym(void);
void	cflush(void);
void	lput(long);
void	cput(int);
void	hputl(int);
void	lputl(long);
long	entryvalue(void);
void	diag(char*, ...);
extern	long	HEADR;
extern	long	INITDAT;
extern	long	INITRND;
extern	long	INITTEXT;
extern	long	INITENTRY;
extern	long	textsize;
extern	long	datsize;
extern	long	bsssize;
extern	int	cout;
extern	int	thumb;