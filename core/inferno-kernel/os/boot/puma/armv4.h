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
#define CpTLBflush 5
#define CpFAR 6
#define CpTLBpurge 6
#define CpCacheCtl 7
#define CpDebug 14
#define CpMMU 15
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
#define CpDBAR 0
#define CpDBVR 1
#define CpDBMR 2
#define CpDBCR 3
#define CpIBCR 8
#define MmuTTB(pa) ((pa) & ~0x3FFF)
#define MmuL1x(pa) (((pa)>>20) & 0xFFF)
#define MmuPTBA(pa) ((pa) & ~0x3FF)
#define MmuL2x(pa) (((pa)>>12) & 0xFF)
#define MmuPBA(pa) ((pa) & ~0xFFF)
#define MmuSBA(pa) ((pa) & ~0xFFFFF)
#define MmuL1page 0x011
#define MmuL1section 0x012
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