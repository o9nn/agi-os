typedef struct {
uchar	signature[4];
long	physaddr;
uchar	length;
uchar	specrev;
uchar	checksum;
uchar	type;
uchar	imcrp;
uchar	reserved[3];
} _MP_;
typedef struct {
uchar	signature[4];
ushort	length;
uchar	version;
uchar	checksum;
uchar	product[20];
ulong	oemtable;
ushort	oemlength;
ushort	entry;
ulong	lapicbase;
ushort	xlength;
uchar	xchecksum;
uchar	reserved;
} PCMP;
typedef struct {
uchar	type;
uchar	apicno;
uchar	version;
uchar	flags;
uchar	signature[4];
ulong	feature;
uchar	reserved[8];
} PCMPprocessor;
typedef struct {
uchar	type;
uchar	busno;
char	string[6];
} PCMPbus;
typedef struct {
uchar	type;
uchar	apicno;
uchar	version;
uchar	flags;
ulong	addr;
} PCMPioapic;
typedef struct {
uchar	type;
uchar	intr;
ushort	flags;
uchar	busno;
uchar	irq;
uchar	apicno;
uchar	intin;
} PCMPintr;
typedef struct {
uchar	type;
uchar	length;
uchar	busno;
uchar	addrtype;
ulong	addrbase[2];
ulong	addrlength[2];
} PCMPsasm;
typedef struct {
uchar	type;
uchar	length;
uchar	busno;
uchar	info;
uchar	parent;
uchar	reserved[3];
} PCMPhierarchy;
typedef struct {
uchar	type;
uchar	length;
uchar	busno;
uchar	modifier;
ulong	range;
} PCMPcbasm;
enum {
PcmpPROCESSOR	= 0x00,
PcmpBUS		= 0x01,
PcmpIOAPIC	= 0x02,
PcmpIOINTR	= 0x03,
PcmpLINTR	= 0x04,
PcmpSASM	= 0x80,
PcmpHIERARCHY	= 0x81,
PcmpCBASM	= 0x82,
PcmpEN		= 0x01,
PcmpBP		= 0x02,
PcmpPOMASK	= 0x03,
PcmpHIGH	= 0x01,
PcmpLOW		= 0x03,
PcmpELMASK	= 0x0C,
PcmpEDGE	= 0x04,
PcmpLEVEL	= 0x0C,
PcmpINT		= 0x00,
PcmpNMI		= 0x01,
PcmpSMI		= 0x02,
PcmpExtINT	= 0x03,
PcmpIOADDR	= 0x00,
PcmpMADDR	= 0x01,
PcmpPADDR	= 0x02,
PcmpSD		= 0x01,
PcmpPR		= 0x01,
};
typedef struct Aintr Aintr;
typedef struct Bus Bus;
typedef struct Apic Apic;
typedef struct Bus {
uchar	type;
uchar	busno;
uchar	po;
uchar	el;
Aintr*	aintr;
Bus*	next;
} Bus;
typedef struct Aintr {
PCMPintr* intr;
Apic*	apic;
Aintr*	next;
};
typedef struct Apic {
int	type;
int	apicno;
ulong*	addr;
ulong	paddr;
int	flags;
Lock;
int	mre;
int	lintr[2];
int	machno;
int	online;
} Apic;
enum {
MaxAPICNO	= 254,
};
enum {
IoapicID	= 0x00,
IoapicVER	= 0x01,
IoapicARB	= 0x02,
IoapicRDT	= 0x10,
};
enum {
ApicFIXED	= 0x00000000,
ApicLOWEST	= 0x00000100,
ApicSMI		= 0x00000200,
ApicRR		= 0x00000300,
ApicNMI		= 0x00000400,
ApicINIT	= 0x00000500,
ApicSTARTUP	= 0x00000600,
ApicExtINT	= 0x00000700,
ApicPHYSICAL	= 0x00000000,
ApicLOGICAL	= 0x00000800,
ApicDELIVS	= 0x00001000,
ApicHIGH	= 0x00000000,
ApicLOW		= 0x00002000,
ApicRemoteIRR	= 0x00004000,
ApicEDGE	= 0x00000000,
ApicLEVEL	= 0x00008000,
ApicIMASK	= 0x00010000,
};
extern void ioapicinit(Apic*, int);
extern void ioapicrdtr(Apic*, int, int*, int*);
extern void ioapicrdtw(Apic*, int, int, int);
extern void lapicclock(Ureg*, void*);
extern int lapiceoi(int);
extern void lapicerror(Ureg*, void*);
extern void lapicicrw(ulong, ulong);
extern void lapicinit(Apic*);
extern void lapicintroff(void);
extern void lapicintron(void);
extern int lapicisr(int);
extern void lapicnmidisable(void);
extern void lapicnmienable(void);
extern void lapiconline(void);
extern void lapicspurious(Ureg*, void*);
extern void lapicstartap(Apic*, int);
extern void lapictimerset(uvlong);
extern void mpinit(void);
extern int mpintrenable(Vctl*);
extern void mpshutdown(void);
extern _MP_ *_mp_;