enum {
BOOTCS = 0,
FPGACS = 1,
DRAMCS = 2,
FPGACONFCS = 3,
CLOCKCS = 4,
};
enum {
VCLK=	SIBIT(5),
BCLK=	SIBIT(4),
EnableVCLK=	IBIT(30),
EnableEnet=	IBIT(29),
EnableRS232=	IBIT(28),
EnetFullDuplex=	IBIT(16),
nCONFIG = SIBIT(13),
USBFullSpeed=	SIBIT(12),
PDN=	SIBIT(5),
EnetLoopback=	SIBIT(4),
};