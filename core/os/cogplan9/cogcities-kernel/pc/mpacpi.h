typedef struct Dsdt Dsdt;
typedef struct Facp Facp;
typedef struct Hpet Hpet;
typedef struct Madt Madt;
typedef struct Mcfg Mcfg;
typedef struct Mcfgd Mcfgd;
typedef struct Rsd Rsd;
struct Dsdt {
uchar	sdthdr[36];
uchar	db[];
};
struct Facp {
uchar	sdthdr[36];
uchar	faddr[4];
uchar	dsdt[4];
uchar	pad[200];
};
struct Hpet {
uchar	sdthdr[36];
uchar	id[4];
uchar	addr[12];
uchar	seqno;
uchar	minticks[2];
uchar	attr;
};
struct Madt {
uchar	sdthdr[36];
uchar	addr[4];
uchar	flags[4];
uchar	structures[];
};
typedef struct Mcfg {
uchar	sdthdr[36];
uchar	pad[8];
Mcfgd	mcfgd[];
} Mcfg;
struct Mcfgd {
uchar	addr[8];
uchar	segno[2];
uchar	sbno;
uchar	ebno;
uchar	pad[4];
};
struct Rsd {
uchar	signature[8];
uchar	rchecksum;
uchar	oemid[6];
uchar	revision;
uchar	raddr[4];
uchar	length[4];
uchar	xaddr[8];
uchar	xchecksum;
uchar	pad[3];
};