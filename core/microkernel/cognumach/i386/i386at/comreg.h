#ifndef _COMREG_H_
#define _COMREG_H_
#define TXRX(addr) (addr + 0)
#define BAUD_LSB(addr) (addr + 0)
#define BAUD_MSB(addr) (addr + 1)
#define INTR_ENAB(addr) (addr + 1)
#define INTR_ID(addr) (addr + 2)
#define FIFO_CTL(addr) (addr + 2)
#define LINE_CTL(addr) (addr + 3)
#define MODEM_CTL(addr) (addr + 4)
#define LINE_STAT(addr) (addr + 5)
#define MODEM_STAT(addr)(addr + 6)
#define SCR(addr) (addr + 7)
#define MODi 0
#define TRAi 2
#define RECi 4
#define LINi 6
#define CTIi 0xc
#define MASKi 0xf
#define iWLS0 0x01
#define iWLS1 0x02
#define iSTB 0x04
#define iPEN 0x08
#define iEPS 0x10
#define iSP 0x20
#define iSETBREAK 0x40
#define iDLAB 0x80
#define i5BITS 0x00
#define i6BITS 0x01
#define i7BITS 0x02
#define i8BITS 0x03
#define iDR 0x01
#define iOR 0x02
#define iPE 0x04
#define iFE 0x08
#define iBRKINTR 0x10
#define iTHRE 0x20
#define iTSRE 0x40
#define iMODEM_INTR 0x01
#define iTX_INTR 0x02
#define iRX_INTR 0x04
#define iERROR_INTR 0x08
#define iRX_ENAB 0x01
#define iTX_ENAB 0x02
#define iERROR_ENAB 0x04
#define iMODEM_ENAB 0x08
#define iDTR 0x01
#define iRTS 0x02
#define iOUT1 0x04
#define iOUT2 0x08
#define iLOOP 0x10
#define iDCTS 0x01
#define iDDSR 0x02
#define iTERI 0x04
#define iDRLSD 0x08
#define iCTS 0x10
#define iDSR 0x20
#define iRI 0x40
#define iRLSD 0x80
#define iFIFOENA 0x01
#define iCLRRCVRFIFO 0x02
#define iCLRXMITFIFO 0x04
#define iDMAMODE 0x08
#define iFIFO1CH 0x00
#define iFIFO4CH 0x40
#define iFIFO8CH 0x80
#define iFIFO14CH 0xc0
#endif