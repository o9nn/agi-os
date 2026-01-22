#define BI2BY 8
#define BI2WD 32
#define BY2WD 4
#define BY2V 8
#define BY2PG 4096
#define WD2PG (BY2PG/BY2WD)
#define BY2XPG (4096*1024)
#define PGSHIFT 12
#define CACHELINESZ 32
#define BLOCKALIGN 8
#define FPalign 16
#define MAXMACH 32
#define KSTACK 4096
#define HZ (100)
#define MS2HZ (1000/HZ)
#define TK2SEC(t) ((t)/HZ)
#define KZERO 0xF0000000
#define KTZERO (KZERO+0x100000)
#define VPT (KZERO-VPTSIZE)
#define VPTSIZE BY2XPG
#define NVPT (VPTSIZE/BY2WD)
#define KMAP (VPT-KMAPSIZE)
#define KMAPSIZE BY2XPG
#define VMAP (KMAP-VMAPSIZE)
#define VMAPSIZE (0x10000000-VPTSIZE-KMAPSIZE)
#define UZERO 0
#define UTZERO (UZERO+BY2PG)
#define UTROUND(t) ROUNDUP((t), BY2PG)
#define USTKTOP (VMAP-BY2PG)
#define USTKSIZE (16*1024*1024)
#define TSTKTOP (USTKTOP-USTKSIZE)
#define TSTKSIZ 100
#define CONFADDR (KZERO+0x1200)
#define TMPADDR (KZERO+0x2000)
#define APBOOTSTRAP (KZERO+0x3000)
#define RMUADDR (KZERO+0x7C00)
#define RMCODE (KZERO+0x8000)
#define RMBUF (KZERO+0x9000)
#define IDTADDR (KZERO+0x10800)
#define REBOOTADDR (0x11000)
#define CPU0PDB (KZERO+0x12000)
#define CPU0PTE (KZERO+0x13000)
#define CPU0GDT (KZERO+0x14000)
#define MACHADDR (KZERO+0x15000)
#define CPU0MACH (KZERO+0x16000)
#define MACHSIZE BY2PG
#define CPU0PTE1 (KZERO+0x17000)
#define CPU0END (CPU0PTE1+BY2PG)
#define NULLSEG 0
#define KDSEG 1
#define KESEG 2
#define UDSEG 3
#define UESEG 4
#define TSSSEG 5
#define APMCSEG 6
#define APMCSEG16 7
#define APMDSEG 8
#define KESEG16 9
#define NGDT 10
#define SELGDT (0<<2)
#define SELLDT (1<<2)
#define SELECTOR(i, t, p) (((i)<<3) | (t) | (p))
#define NULLSEL SELECTOR(NULLSEG, SELGDT, 0)
#define KDSEL SELECTOR(KDSEG, SELGDT, 0)
#define KESEL SELECTOR(KESEG, SELGDT, 0)
#define UESEL SELECTOR(UESEG, SELGDT, 3)
#define UDSEL SELECTOR(UDSEG, SELGDT, 3)
#define TSSSEL SELECTOR(TSSSEG, SELGDT, 0)
#define APMCSEL SELECTOR(APMCSEG, SELGDT, 0)
#define APMCSEL16 SELECTOR(APMCSEG16, SELGDT, 0)
#define APMDSEL SELECTOR(APMDSEG, SELGDT, 0)
#define SEGDATA (0x10<<8)
#define SEGEXEC (0x18<<8)
#define SEGTSS (0x9<<8)
#define SEGCG (0x0C<<8)
#define SEGIG (0x0E<<8)
#define SEGTG (0x0F<<8)
#define SEGTYPE (0x1F<<8)
#define SEGP (1<<15)
#define SEGPL(x) ((x)<<13)
#define SEGB (1<<22)
#define SEGG (1<<23)
#define SEGE (1<<10)
#define SEGW (1<<9)
#define SEGR (1<<9)
#define SEGD (1<<22)
#define PTEMAPMEM (1024*1024)
#define PTEPERTAB (PTEMAPMEM/BY2PG)
#define SEGMAPSIZE 1984
#define SSEGMAPSIZE 16
#define PPN(x) ((x)&~(BY2PG-1))
#define PTEVALID (1<<0)
#define PTEWT (1<<3)
#define PTEUNCACHED (1<<4)
#define PTEWRITE (1<<1)
#define PTERONLY (0<<1)
#define PTEKERNEL (0<<2)
#define PTEUSER (1<<2)
#define PTESIZE (1<<7)
#define PTEGLOBAL (1<<8)
#define PDX(va) ((((ulong)(va))>>22) & 0x03FF)
#define PTX(va) ((((ulong)(va))>>12) & 0x03FF)
#define getpgcolor(a) 0