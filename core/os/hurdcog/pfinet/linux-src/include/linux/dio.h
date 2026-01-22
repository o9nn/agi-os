#ifndef _LINUX_DIO_H
#define _LINUX_DIO_H
#ifdef __KERNEL__
#define DIO_IDOFF 0x01
#define DIO_IPLOFF 0x03
#define DIO_SECIDOFF 0x15
#define DIOII_SIZEOFF 0x101
#define DIO_IHPIBADDR 0x47800
#define DIO_IHPIBSCODE 7
#define CONFIG_IHPIB
#ifdef CONFIG_IHPIB
#define DIO_ISIHPIB(scode) ((scode) == DIO_IHPIBSCODE)
#else
#define DIO_ISIHPIB(scode) 0
#endif
#define DIO_VIRADDRBASE 0xf0000000
#define DIO_BASE 0x600000
#define DIO_END 0x1000000
#define DIO_DEVSIZE 0x10000
#define DIOII_BASE 0x01000000
#define DIOII_END 0x20000000
#define DIOII_DEVSIZE 0x00400000
#define DIO_SCMAX 32
#define DIOII_SCBASE 132
#define DIO_SCINHOLE(scode) (((scode) >= 32) && ((scode) < DIOII_SCBASE))
#define DIO_ID(baseaddr) readb((baseaddr) + DIO_IDOFF)
#define DIO_SECID(baseaddr) readb((baseaddr) + DIO_SECIDOFF)
#define DIO_IPL(baseaddr) (((readb((baseaddr) + DIO_IPLOFF) >> 4) & 0x03) + 3)
#define DIOII_SIZE(baseaddr) ((readb((baseaddr) + DIOII_SIZEOFF) + 1) * 0x100000)
#define DIO_SIZE(scode, base) (DIO_ISDIOII((scode)) ? DIOII_SIZE((base)) : DIO_DEVSIZE)
#define DIO_ENCODE_ID(pr,sec) ((((int)sec & 0xff) << 8) & ((int)pr & 0xff))
#define DIO_NEEDSSECID(id) ((id) == DIO_ID_FBUFFER)
#define DIO_ID_DCA0 0x02
#define DIO_DESC_DCA0 "98644A DCA0 serial"
#define DIO_ID_DCA0REM 0x82
#define DIO_DESC_DCA0REM "98644A DCA0REM serial"
#define DIO_ID_DCA1 0x42
#define DIO_DESC_DCA1 "98644A DCA1 serial"
#define DIO_ID_DCA1REM 0xc2
#define DIO_DESC_DCA1REM "98644A DCA1REM serial"
#define DIO_ID_DCM 0x05
#define DIO_DESC_DCM "98642A DCM serial MUX"
#define DIO_ID_DCMREM 0x85
#define DIO_DESC_DCMREM "98642A DCMREM serial MUX"
#define DIO_ID_LAN 0x15
#define DIO_DESC_LAN "98643A LAN"
#define DIO_ID_FHPIB 0x08
#define DIO_DESC_FHPIB "98625A/98625B fast HPIB"
#define DIO_ID_NHPIB 0x80
#define DIO_DESC_NHPIB "98624A HPIB"
#define DIO_ID_IHPIB 0x00
#define DIO_DESC_IHPIB "internal HPIB"
#define DIO_ID_SCSI0 0x07
#define DIO_DESC_SCSI0 "98625A SCSI0"
#define DIO_ID_SCSI1 0x27
#define DIO_DESC_SCSI1 "98625A SCSI1"
#define DIO_ID_SCSI2 0x47
#define DIO_DESC_SCSI2 "98625A SCSI2"
#define DIO_ID_SCSI3 0x67
#define DIO_DESC_SCSI3 "98625A SCSI3"
#define DIO_ID_FBUFFER 0x39
#define DIO_DESC_FBUFFER "bitmapped display"
#define DIO_ID_MISC0 0x03
#define DIO_DESC_MISC0 "98622A"
#define DIO_ID_MISC1 0x04
#define DIO_DESC_MISC1 "98623A"
#define DIO_ID_PARALLEL 0x06
#define DIO_DESC_PARALLEL "internal parallel"
#define DIO_ID_MISC2 0x09
#define DIO_DESC_MISC2 "98287A keyboard"
#define DIO_ID_MISC3 0x0a
#define DIO_DESC_MISC3 "HP98635A FP accelerator"
#define DIO_ID_MISC4 0x0b
#define DIO_DESC_MISC4 "timer"
#define DIO_ID_MISC5 0x12
#define DIO_DESC_MISC5 "98640A"
#define DIO_ID_MISC6 0x16
#define DIO_DESC_MISC6 "98659A"
#define DIO_ID_MISC7 0x19
#define DIO_DESC_MISC7 "237 display"
#define DIO_ID_MISC8 0x1a
#define DIO_DESC_MISC8 "quad-wide card"
#define DIO_ID_MISC9 0x1b
#define DIO_DESC_MISC9 "98253A"
#define DIO_ID_MISC10 0x1c
#define DIO_DESC_MISC10 "98253A"
#define DIO_ID_MISC11 0x1d
#define DIO_DESC_MISC11 "98633A"
#define DIO_ID_MISC12 0x1e
#define DIO_DESC_MISC12 "98259A"
#define DIO_ID_MISC13 0x1f
#define DIO_DESC_MISC13 "8741"
#define DIO_ID_VME 0x31
#define DIO_DESC_VME "98577A VME adapter"
#define DIO_ID_DCL 0x34
#define DIO_DESC_DCL "98628A DCL serial"
#define DIO_ID_DCLREM 0xb4
#define DIO_DESC_DCLREM "98628A DCLREM serial"
#define DIO_ID2_GATORBOX 0x01
#define DIO_DESC2_GATORBOX "98700/98710 \"gatorbox\" display"
#define DIO_ID2_TOPCAT 0x02
#define DIO_DESC2_TOPCAT "98544/98545/98547 \"topcat\" display"
#define DIO_ID2_RENAISSANCE 0x04
#define DIO_DESC2_RENAISSANCE "98720/98721 \"renaissance\" display"
#define DIO_ID2_LRCATSEYE 0x05
#define DIO_DESC2_LRCATSEYE "low-res catseye display"
#define DIO_ID2_HRCCATSEYE 0x06
#define DIO_DESC2_HRCCATSEYE "high-res color catseye display"
#define DIO_ID2_HRMCATSEYE 0x07
#define DIO_DESC2_HRMCATSEYE "high-res mono catseye display"
#define DIO_ID2_DAVINCI 0x08
#define DIO_DESC2_DAVINCI "98730/98731 \"davinci\" display"
#define DIO_ID2_XXXCATSEYE 0x09
#define DIO_DESC2_XXXCATSEYE "catseye display"
#define DIO_ID2_HYPERION 0x0e
#define DIO_DESC2_HYPERION "A1096A \"hyperion\" display"
#define DIO_ID2_XGENESIS 0x0b
#define DIO_DESC2_XGENESIS "\"x-genesis\" display"
#define DIO_ID2_TIGER 0x0c
#define DIO_DESC2_TIGER "\"tiger\" display"
#define DIO_ID2_YGENESIS 0x0d
#define DIO_DESC2_YGENESIS "\"y-genesis\" display"
extern void dio_init(void);
extern int dio_find(int deviceid);
extern void *dio_scodetoviraddr(int scode);
extern int dio_scodetoipl(int scode);
extern void dio_config_board(int scode);
extern void dio_unconfig_board(int scode);
#endif
#endif