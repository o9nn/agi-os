#define BI2BY 8
#define BI2WD 32
#define BY2WD 4
#define BY2V 8
#define BY2PG 4096
#define WD2PG (BY2PG/BY2WD)
#define PGSHIFT 12
#define ROUND(s, sz) (((s)+(sz-1))&~(sz-1))
#define PGROUND(s) ROUND(s, BY2PG)
#define BIT(n) (1<<n)
#define BITS(a,b) ((1<<(b+1))-(1<<a))
#define MAXMACH 1
#define HZ (100)
#define MS2HZ (1000/HZ)
#define TK2SEC(t) ((t)/HZ)
#define MS2TK(t) ((t)/MS2HZ)
#define CLOCKFREQ 3686400
#define MS2TMR(t) ((ulong)(((uvlong)(t)*CLOCKFREQ)/1000))
#define US2TMR(t) ((ulong)(((uvlong)(t)*CLOCKFREQ)/1000000))
#define KZERO 0xC0000000
#define MACHADDR (KZERO+0x00001000)
#define KTTB (KZERO+0x00004000)
#define KTZERO (KZERO+0x00008010)
#define KSTACK 8192
#define FLASHMEM 0x50000000
#define FLUSHMEM 0xE0000000
#define DCFADDR FLUSHMEM
#define MCFADDR (FLUSHMEM+(1<<20))
#define UCDRAMZERO 0xC8000000
#define AIVECADDR 0xFFFF0000
#define PHYSFLASH0 0x00000000
#define PHYSCS1 0x08000000
#define PHYSCS2 0x10000000
#define PHYSCS3 0x18000000
#define PHYSPCMCIA0 0x20000000
#define PHYSPCMCIA1 0x30000000
#define PCMCIASIZE 0x10000000
#define PHYSCS4 0x40000000
#define PHYSCS5 0x48000000
#define PHYSSERIAL(n) (0x80000000+0x10000*(n))
#define PHYSUSB 0x80000000
#define PHYSGPCLK 0x80020060
#define PHYSMCP 0x80060000
#define PHYSSSP 0x80070060
#define PHYSOSTMR 0x90000000
#define PHYSRTC 0x90010000
#define PHYSPOWER 0x90020000
#define PHYSRESET 0x90030000
#define PHYSGPIO 0x90040000
#define PHYSINTR 0x90050000
#define PHYSPPC 0x90060000
#define PHYSMEMCFG 0xA0000000
#define PHYSDMA 0xB0000000
#define PHYSLCD 0xB0100000
#define PHYSMEM0 0xC0000000
#define PHYSFLUSH0 0xE0000000
#define MDCNFG (PHYSMEMCFG)
#define MDCAS0 (PHYSMEMCFG+4)
#define MDCAS1 (PHYSMEMCFG+8)
#define MDCAS2 (PHYSMEMCFG+0xC)
#define MSC0 (PHYSMEMCFG+0x10)
#define MSC1 (PHYSMEMCFG+0x14)
#define MSC2 (PHYSMEMCFG+0x2C)
#define MSCx(RRR, RDN, RDF, RBW, RT) ((((RRR)&0x7)<<13)|(((RDN)&0x1F)<<8)|(((RDF)&0x1F)<<3)|(((RBW)&1)<<2)|((RT)&3))
#define CACHELINELOG 5
#define CACHELINESZ (1<<CACHELINELOG)
#define PsrMusr 0x10
#define PsrMfiq 0x11
#define PsrMirq 0x12
#define PsrMsvc 0x13
#define PsrMabt 0x17
#define PsrMund 0x1B
#define PsrMsys 0x1F
#define PsrMask 0x1F
#define PsrDfiq 0x00000040
#define PsrDirq 0x00000080
#define PsrV 0x10000000
#define PsrC 0x20000000
#define PsrZ 0x40000000
#define PsrN 0x80000000
#define CpCPUID 0
#define CpControl 1
#define CpTTB 2
#define CpDAC 3
#define CpFSR 5
#define CpFAR 6
#define CpCacheCtl 7
#define CpTLBops 8
#define CpRBops 9
#define CpPID 13
#define CpDebug 14
#define CpTest 15
#define CpMMU 15
#define CpPWR 15
#define CpCmmu 0x00000001
#define CpCalign 0x00000002
#define CpCDcache 0x00000004
#define CpCwb 0x00000008
#define CpCi32 0x00000010
#define CpCd32 0x00000020
#define CpCbe 0x00000080
#define CpCsystem 0x00000100
#define CpCrom 0x00000200
#define CpCIcache 0x00001000
#define CpCaltivec 0x00002000
#define MmuSection (1<<20)
#define MmuLargePage (1<<16)
#define MmuSmallPage (1<<12)
#define MmuTTB(pa) ((pa) & ~0x3FFF)
#define MmuL1x(pa) (((pa)>>20) & 0xFFF)
#define MmuPTBA(pa) ((pa) & ~0x3FF)
#define MmuL2x(pa) (((pa)>>12) & 0xFF)
#define MmuPBA(pa) ((pa) & ~0xFFF)
#define MmuSBA(pa) ((pa) & ~0xFFFFF)
#define MmuL1type 0x03
#define MmuL1page 0x01
#define MmuL1section 0x02
#define MmuL2invalid 0x000
#define MmuL2large 0x001
#define MmuL2small 0x002
#define MmuWB 0x004
#define MmuIDC 0x008
#define MmuDAC(d) (((d) & 0xF)<<5)
#define MmuAP(i, v) ((v)<<(((i)*2)+4))
#define MmuL1AP(v) MmuAP(3, (v))
#define MmuL2AP(v) MmuAP(3, (v))|MmuAP(2, (v))|MmuAP(1, (v))|MmuAP(0, (v))
#define MmuAPsro 0
#define MmuAPsrw 1
#define MmuAPuro 2
#define MmuAPurw 3