#define KiB 1024u
#define MiB 1048576u
#define GiB 1073741824u
#define BITN(o) (1<<(o))
#define F(v, o, w) (((v) & ((1<<(w))-1))<<(o))
#define BY2PG (4*KiB)
#define PGSHIFT 12
#define MAXMACH 4
#define MACHSIZE BY2PG
#define L1SIZE (4 * BY2PG)
#define KSTKSIZE (16*KiB)
#define STACKALIGN(sp) ((sp) & ~7)
#define USER 9
#define MACH 10
#define KSEG0 0xC0000000
#define KSEGM 0xC0000000
#define KZERO KSEG0
#define L1 (KZERO+16*KiB)
#define CONFADDR (KZERO+0x400000)
#define CACHECONF (CONFADDR+48*KiB)
#define KTZERO (KZERO+0x410000)
#define L2pages (2*MiB)
#define RESRVDHIMEM (64*KiB + MiB + L2pages)
#define DRAMSIZE GiB
#define UZERO 0
#define UTZERO (UZERO+BY2PG)
#define UTROUND(t) ROUNDUP((t), BY2PG)
#define USTKTOP (0x40000000 - 64*KiB - MiB)
#define USTKSIZE (8*1024*1024)
#define TSTKTOP (USTKTOP-USTKSIZE)
#define TSTKSIZ 256
#define REBOOTADDR KADDR(0x100)
#define BLOCKALIGN CACHELINESZ
#define KSTACK KSTKSIZE
#define BI2BY 8
#define BY2SE 4
#define BY2WD 4
#define BY2V 8
#define CACHELINESZ 32
#define PTEMAPMEM (1024*1024)
#define PTEPERTAB (PTEMAPMEM/BY2PG)
#define SEGMAPSIZE 1984
#define SSEGMAPSIZE 16
#define PPN(x) ((x)&~(BY2PG-1))
#define PTEVALID (1<<0)
#define PTERONLY 0
#define PTEWRITE (1<<1)
#define PTEUNCACHED (1<<2)
#define PTEKERNEL (1<<3)
#define PHYSDRAM 0
#define PHYSIO 0x50000000
#define VIRTIO PHYSIO
#define PHYSL2BAG 0x50043000
#define PHYSEVP 0x6000f100
#define PHYSCONS 0x70006000
#define PHYSIOEND 0xc0000000
#define PHYSAHB 0xc0000000
#define VIRTAHB 0xb0000000
#define P2VAHB(pa) ((pa) - PHYSAHB + VIRTAHB)
#define PHYSNOR 0xd0000000
#define VIRTNOR 0x40000000