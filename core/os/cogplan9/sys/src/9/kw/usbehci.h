#undef	dprint
#undef	ddprint
#undef	deprint
#undef	ddeprint
#define dprint		if(ehcidebug)print
#define ddprint		if(ehcidebug>1)print
#define deprint		if(ehcidebug || ep->debug)print
#define ddeprint	if(ehcidebug>1 || ep->debug>1)print
typedef struct Ctlr Ctlr;
typedef struct Eopio Eopio;
typedef struct Isoio Isoio;
typedef struct Poll Poll;
typedef struct Qh Qh;
typedef struct Qtree Qtree;
#pragma incomplete Ctlr;
#pragma incomplete Ecapio;
#pragma incomplete Eopio;
#pragma incomplete Edbgio;
#pragma incomplete Isoio;
#pragma incomplete Poll;
#pragma incomplete Qh;
#pragma incomplete Qtree;
struct Poll
{
Lock;
Rendez;
int	must;
int	does;
};
struct Ctlr
{
Rendez;
Lock;
QLock	portlck;
int	active;
Ecapio*	capio;
Eopio*	opio;
int	nframes;
ulong*	frames;
Qh*	qhs;
Qtree*	tree;
int	ntree;
Qh*	intrqhs;
Isoio*	iso;
ulong	load;
ulong	isoload;
int	nintr;
int	ntdintr;
int	nqhintr;
int	nisointr;
int	nreqs;
Poll	poll;
};
struct Eopio
{
ulong	cmd;
ulong	sts;
ulong	intr;
ulong	frno;
ulong	seg;
ulong	frbase;
ulong	link;
uchar	d2c[0x40-0x1c];
ulong	config;
ulong	portsc[1];
uchar	_pad0[0x64-0x48];
ulong	otgsc;
ulong	usbmode;
ulong	epsetupsts;
ulong	epprime;
ulong	epflush;
ulong	epsts;
ulong	epcompl;
ulong	epctl[6];
uchar	_pad1[0x2c0-0x98];
ulong	snoop1;
ulong	snoop2;
ulong	agecntthresh;
ulong	prictl;
ulong	sictl;
uchar	_pad2[0x3c0-0x2d4];
ulong	ctl;
};
extern int ehcidebug;
void	ehcilinkage(Hci *hp);
void	ehcimeminit(Ctlr *ctlr);
void	ehcirun(Ctlr *ctlr, int on);