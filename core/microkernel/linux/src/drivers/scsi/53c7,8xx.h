#ifndef NCR53c7x0_H
#define NCR53c7x0_H
#if !defined(LINUX_1_2) && !defined(LINUX_1_3)
#include <linux/version.h>
#if LINUX_VERSION_CODE > 65536 + 3 * 256
#define LINUX_1_3
#else
#define LINUX_1_2
#endif
#endif
#if defined(HOSTS_C) || defined(MODULE)
#include <scsi/scsicam.h>
extern int NCR53c7xx_abort(Scsi_Cmnd *);
extern int NCR53c7xx_detect(Scsi_Host_Template *tpnt);
extern int NCR53c7xx_queue_command(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
extern int NCR53c7xx_reset(Scsi_Cmnd *, unsigned int);
#ifdef MODULE
extern int NCR53c7xx_release(struct Scsi_Host *);
#else
#define NCR53c7xx_release NULL
#endif
#ifdef LINUX_1_2
#define NCR53c7xx {NULL, NULL, "NCR53c{7,8}xx (rel 17)", NCR53c7xx_detect,\
NULL, NULL, NULL, \
NCR53c7xx_queue_command, NCR53c7xx_abort, NCR53c7xx_reset, \
NULL , scsicam_bios_param, 24, \
7, 127 , 3, \
0, 0, DISABLE_CLUSTERING}
#else
#define NCR53c7xx {NULL, NULL, NULL, NULL, \
"NCR53c{7,8}xx (rel 17)", NCR53c7xx_detect,\
NULL, NULL, NULL, \
NCR53c7xx_queue_command, NCR53c7xx_abort, NCR53c7xx_reset, \
NULL , scsicam_bios_param, 24, \
7, 127 , 3, \
0, 0, DISABLE_CLUSTERING}
#endif
#endif
#ifndef HOSTS_C
#ifdef LINUX_1_2
extern inline unsigned long virt_to_phys(volatile void * address)
{
return (unsigned long) address;
}
extern inline void * phys_to_virt(unsigned long address)
{
return (void *) address;
}
#define virt_to_bus virt_to_phys
#define bus_to_virt phys_to_virt
#define readb(addr) (*(volatile unsigned char *) (addr))
#define readw(addr) (*(volatile unsigned short *) (addr))
#define readl(addr) (*(volatile unsigned int *) (addr))
#define writeb(b,addr) ((*(volatile unsigned char *) (addr)) = (b))
#define writew(b,addr) ((*(volatile unsigned short *) (addr)) = (b))
#define writel(b,addr) ((*(volatile unsigned int *) (addr)) = (b))
#define mb()
#endif
#define SCNTL0_REG 0x00
#define SCNTL0_ARB1 0x80
#define SCNTL0_ARB2 0x40
#define SCNTL0_STRT 0x20
#define SCNTL0_WATN 0x10
#define SCNTL0_EPC 0x08
#define SCNTL0_EPG_700 0x04
#define SCNTL0_AAP 0x02
#define SCNTL0_TRG 0x01
#define SCNTL1_REG 0x01
#define SCNTL1_EXC 0x80
#define SCNTL1_ADB 0x40
#define SCNTL1_ESR_700 0x20
#define SCNTL1_DHP_800 0x20
#define SCNTL1_CON 0x10
#define SCNTL1_RST 0x08
#define SCNTL1_AESP 0x04
#define SCNTL1_SND_700 0x02
#define SCNTL1_IARB_800 0x02
#define SCNTL1_RCV_700 0x01
#define SCNTL1_SST_800 0x01
#define SCNTL2_REG_800 0x02
#define SCNTL2_800_SDU 0x80
#define SCNTL3_REG_800 0x03
#define SCNTL3_800_SCF_SHIFT 4
#define SCNTL3_800_SCF_MASK 0x70
#define SCNTL3_800_SCF2 0x40
#define SCNTL3_800_SCF1 0x20
#define SCNTL3_800_SCF0 0x10
#define SCNTL3_800_CCF_SHIFT 0
#define SCNTL3_800_CCF_MASK 0x07
#define SCNTL3_800_CCF2 0x04
#define SCNTL3_800_CCF1 0x02
#define SCNTL3_800_CCF0 0x01
#define SDID_REG_700 0x02
#define SDID_REG_800 0x06
#define GP_REG_800 0x07
#define GP_800_IO1 0x02
#define GP_800_IO2 0x01
#define SIEN_REG_700 0x03
#define SIEN0_REG_800 0x40
#define SIEN_MA 0x80
#define SIEN_FC 0x40
#define SIEN_700_STO 0x20
#define SIEN_800_SEL 0x20
#define SIEN_700_SEL 0x10
#define SIEN_800_RESEL 0x10
#define SIEN_SGE 0x08
#define SIEN_UDC 0x04
#define SIEN_RST 0x02
#define SIEN_PAR 0x01
#define SCID_REG 0x04
#define SCID_800_RRE 0x40
#define SCID_800_SRE 0x20
#define SCID_800_ENC_MASK 0x07
#define SXFER_REG 0x05
#define SXFER_DHP 0x80
#define SXFER_TP2 0x40
#define SXFER_TP1 0x20
#define SXFER_TP0 0x10
#define SXFER_TP_MASK 0x70
#define SXFER_TP_SHIFT 5
#define SXFER_TP_4 0x00
#define SXFER_TP_5 0x10<<1
#define SXFER_TP_6 0x20<<1
#define SXFER_TP_7 0x30<<1
#define SXFER_TP_8 0x40<<1
#define SXFER_TP_9 0x50<<1
#define SXFER_TP_10 0x60<<1
#define SXFER_TP_11 0x70<<1
#define SXFER_MO3 0x08
#define SXFER_MO2 0x04
#define SXFER_MO1 0x02
#define SXFER_MO0 0x01
#define SXFER_MO_MASK 0x0f
#define SXFER_MO_SHIFT 0
#define SODL_REG_700 0x06
#define SODL_REG_800 0x54
#define SBCL_REG 0x0b
#define SBCL_REQ 0x80
#define SBCL_ACK 0x40
#define SBCL_BSY 0x20
#define SBCL_SEL 0x10
#define SBCL_ATN 0x08
#define SBCL_MSG 0x04
#define SBCL_CD 0x02
#define SBCL_IO 0x01
#define SBCL_PHASE_CMDOUT SBCL_CD
#define SBCL_PHASE_DATAIN SBCL_IO
#define SBCL_PHASE_DATAOUT 0
#define SBCL_PHASE_MSGIN (SBCL_CD|SBCL_IO|SBCL_MSG)
#define SBCL_PHASE_MSGOUT (SBCL_CD|SBCL_MSG)
#define SBCL_PHASE_STATIN (SBCL_CD|SBCL_IO)
#define SBCL_PHASE_MASK (SBCL_CD|SBCL_IO|SBCL_MSG)
#define SFBR_REG 0x08
#define SIDL_REG_700 0x09
#define SIDL_REG_800 0x50
#define SBDL_REG_700 0x0a
#define SBDL_REG_800 0x58
#define SSID_REG_800 0x0a
#define SSID_800_VAL 0x80
#define SSID_800_ENCID_MASK 0x07
#define SOCL_REG 0x0b
#define SOCL_REQ 0x80
#define SOCL_ACK 0x40
#define SOCL_BSY 0x20
#define SOCL_SEL 0x10
#define SOCL_ATN 0x08
#define SOCL_MSG 0x04
#define SOCL_CD 0x02
#define SOCL_IO 0x01
#define SBCL_SSCF1 0x02
#define SBCL_SSCF0 0x01
#define SBCL_SSCF_MASK 0x03
#define DSTAT_REG 0x0c
#define DSTAT_DFE 0x80
#define DSTAT_800_MDPE 0x40
#define DSTAT_800_BF 0x20
#define DSTAT_ABRT 0x10
#define DSTAT_SSI 0x08
#define DSTAT_SIR 0x04
#define DSTAT_WTD 0x02
#define DSTAT_OPC 0x01
#define DSTAT_800_IID 0x01
#define SSTAT0_REG 0x0d
#define SIST0_REG_800 0x42
#define SSTAT0_MA 0x80
#define SSTAT0_CMP 0x40
#define SSTAT0_700_STO 0x20
#define SIST0_800_SEL 0x20
#define SSTAT0_700_SEL 0x10
#define SIST0_800_RSL 0x10
#define SSTAT0_SGE 0x08
#define SSTAT0_UDC 0x04
#define SSTAT0_RST 0x02
#define SSTAT0_PAR 0x01
#define SSTAT1_REG 0x0e
#define SSTAT1_ILF 0x80
#define SSTAT1_ORF 0x40
#define SSTAT1_OLF 0x20
#define SSTAT1_AIP 0x10
#define SSTAT1_LOA 0x08
#define SSTAT1_WOA 0x04
#define SSTAT1_RST 0x02
#define SSTAT1_SDP 0x01
#define SSTAT2_REG 0x0f
#define SSTAT2_FF3 0x80
#define SSTAT2_FF2 0x40
#define SSTAT2_FF1 0x20
#define SSTAT2_FF0 0x10
#define SSTAT2_FF_MASK 0xf0
#define SSTAT2_FF_SHIFT 4
#define SSTAT2_SDP 0x08
#define SSTAT2_MSG 0x04
#define SSTAT2_CD 0x02
#define SSTAT2_IO 0x01
#define SSTAT2_PHASE_CMDOUT SSTAT2_CD
#define SSTAT2_PHASE_DATAIN SSTAT2_IO
#define SSTAT2_PHASE_DATAOUT 0
#define SSTAT2_PHASE_MSGIN (SSTAT2_CD|SSTAT2_IO|SSTAT2_MSG)
#define SSTAT2_PHASE_MSGOUT (SSTAT2_CD|SSTAT2_MSG)
#define SSTAT2_PHASE_STATIN (SSTAT2_CD|SSTAT2_IO)
#define SSTAT2_PHASE_MASK (SSTAT2_CD|SSTAT2_IO|SSTAT2_MSG)
#define SCRATCHA_REG_00 0x10
#define DSA_REG 0x10
#define CTEST0_REG_700 0x14
#define CTEST0_REG_800 0x18
#define CTEST0_700_RTRG 0x02
#define CTEST0_700_DDIR 0x01
#define CTEST1_REG_700 0x15
#define CTEST1_REG_800 0x19
#define CTEST1_FMT3 0x80
#define CTEST1_FMT2 0x40
#define CTEST1_FMT1 0x20
#define CTEST1_FMT0 0x10
#define CTEST1_FFL3 0x08
#define CTEST1_FFL2 0x04
#define CTEST1_FFL1 0x02
#define CTEST1_FFL0 0x01
#define CTEST2_REG_700 0x16
#define CTEST2_REG_800 0x1a
#define CTEST2_800_DDIR 0x80
#define CTEST2_800_SIGP 0x40
#define CTEST2_800_CIO 0x20 .
#define CTEST2_800_CM 0x10
#define CTEST2_700_SOFF 0x20
#define CTEST2_700_SFP 0x10
#define CTEST2_700_DFP 0x08
#define CTEST2_TEOP 0x04
#define CTEST2_DREQ 0x02
#define CTEST2_800_DACK 0x01
#define CTEST3_REG_700 0x17
#define CTEST3_REG_800 0x1b
#define CTEST3_800_V3 0x80
#define CTEST3_800_V2 0x40
#define CTEST3_800_V1 0x20
#define CTEST3_800_V0 0x10
#define CTEST3_800_FLF 0x08
#define CTEST3_800_CLF 0x04
#define CTEST3_800_FM 0x02
#define CTEST4_REG_700 0x18
#define CTEST4_REG_800 0x21
#define CTEST4_800_BDIS 0x80
#define CTEST4_ZMOD 0x40
#define CTEST4_SZM 0x20
#define CTEST4_700_SLBE 0x10
#define CTEST4_800_SRTM 0x10
#define CTEST4_700_SFWR 0x08
#define CTEST4_800_MPEE 0x08
#define CTEST4_FBL2 0x04
#define CTEST4_FBL1 0x02
#define CTEST4_FBL0 0x01
#define CTEST4_FBL_MASK 0x07
#define CTEST4_FBL_0 0x04
#define CTEST4_FBL_1 0x05
#define CTEST4_FBL_2 0x06
#define CTEST4_FBL_3 0x07
#define CTEST4_800_SAVE (CTEST4_800_BDIS)
#define CTEST5_REG_700 0x19
#define CTEST5_REG_800 0x22
#define CTEST5_ADCK 0x80
#define CTEST5_BBCK 0x40
#define CTEST5_700_ROFF 0x20
#define CTEST5_MASR 0x10
#define CTEST5_DDIR 0x08
#define CTEST5_700_EOP 0x04
#define CTEST5_700_DREQ 0x02
#define CTEST5_700_DACK 0x01
#define CTEST6_REG_700 0x1a
#define CTEST6_REG_800 0x23
#define CTEST7_REG 0x1b
#define CTEST7_10_CDIS 0x80
#define CTEST7_10_SC1 0x40
#define CTEST7_10_SC0 0x20
#define CTEST7_10_SC_MASK 0x60
#define CTEST7_0060_FM 0x20
#define CTEST7_STD 0x10
#define CTEST7_DFP 0x08
#define CTEST7_EVP 0x04
#define CTEST7_10_TT1 0x02
#define CTEST7_00_DC 0x02
#define CTEST7_DIFF 0x01
#define CTEST7_SAVE ( CTEST7_EVP | CTEST7_DIFF )
#define TEMP_REG 0x1c
#define DFIFO_REG 0x20
#define DFIFO_00_FLF 0x80
#define DFIFO_00_CLF 0x40
#define DFIFO_BO6 0x40
#define DFIFO_BO5 0x20
#define DFIFO_BO4 0x10
#define DFIFO_BO3 0x08
#define DFIFO_BO2 0x04
#define DFIFO_BO1 0x02
#define DFIFO_BO0 0x01
#define DFIFO_10_BO_MASK 0x7f
#define DFIFO_00_BO_MASK 0x3f
#define ISTAT_REG_700 0x21
#define ISTAT_REG_800 0x14
#define ISTAT_ABRT 0x80
#define ISTAT_10_SRST 0x40
#define ISTAT_10_SIGP 0x20
#define ISTAT_800_SEM 0x10
#define ISTAT_CON 0x08
#define ISTAT_800_INTF 0x04
#define ISTAT_700_PRE 0x04
#define ISTAT_SIP 0x02
#define ISTAT_DIP 0x01
#define CTEST8_REG 0x22
#define CTEST8_0066_EAS 0x80
#define CTEST8_0066_EFM 0x40
#define CTEST8_0066_GRP 0x20
#define CTEST8_0066_TE 0x10
#define CTEST8_0066_HSC 0x08
#define CTEST8_0066_SRA 0x04
#define CTEST8_0066_DAS 0x02
#define CTEST8_0066_LDE 0x01
#define CTEST8_10_V3 0x80
#define CTEST8_10_V2 0x40
#define CTEST8_10_V1 0x20
#define CTEST8_10_V0 0x10
#define CTEST8_10_V_MASK 0xf0
#define CTEST8_10_FLF 0x08
#define CTEST8_10_CLF 0x04
#define CTEST8_10_FM 0x02
#define CTEST8_10_SM 0x01
#define CTEST9_REG_00 0x23
#define LCRC_REG_10 0x23
#define DBC_REG 0x24
#define DBC_TCI_TRUE (1 << 19)
#define DBC_TCI_COMPARE_DATA (1 << 18)
#define DBC_TCI_COMPARE_PHASE (1 << 17)
#define DBC_TCI_WAIT_FOR_VALID (1 << 16)
#define DBC_TCI_MASK_MASK 0xff00
#define DBC_TCI_MASK_SHIFT 8
#define DBC_TCI_DATA_MASK 0xff
#define DBC_TCI_DATA_SHIFT 0
#define DBC_RWRI_IMMEDIATE_MASK 0xff00
#define DBC_RWRI_IMMEDIATE_SHIFT 8
#define DBC_RWRI_ADDRESS_MASK 0x3f0000
#define DBC_RWRI_ADDRESS_SHIFT 16
#define DCMD_REG 0x27
#define DCMD_TYPE_MASK 0xc0
#define DCMD_TYPE_BMI 0x00
#define DCMD_BMI_IO 0x01
#define DCMD_BMI_CD 0x02
#define DCMD_BMI_MSG 0x04
#define DCMD_BMI_OP_MASK 0x18
#define DCMD_BMI_OP_MOVE_T 0x00
#define DCMD_BMI_OP_MOVE_I 0x08
#define DCMD_BMI_INDIRECT 0x20
#define DCMD_TYPE_TCI 0x80
#define DCMD_TCI_IO 0x01
#define DCMD_TCI_CD 0x02
#define DCMD_TCI_MSG 0x04
#define DCMD_TCI_OP_MASK 0x38
#define DCMD_TCI_OP_JUMP 0x00
#define DCMD_TCI_OP_CALL 0x08
#define DCMD_TCI_OP_RETURN 0x10
#define DCMD_TCI_OP_INT 0x18
#define DCMD_TYPE_RWRI 0x40
#define DCMD_RWRI_OPC_MASK 0x38
#define DCMD_RWRI_OPC_WRITE 0x28
#define DCMD_RWRI_OPC_READ 0x30
#define DCMD_RWRI_OPC_MODIFY 0x38
#define DCMD_RWRI_OP_MASK 0x07
#define DCMD_RWRI_OP_MOVE 0x00
#define DCMD_RWRI_OP_SHL 0x01
#define DCMD_RWRI_OP_OR 0x02
#define DCMD_RWRI_OP_XOR 0x03
#define DCMD_RWRI_OP_AND 0x04
#define DCMD_RWRI_OP_SHR 0x05
#define DCMD_RWRI_OP_ADD 0x06
#define DCMD_RWRI_OP_ADDC 0x07
#define DCMD_TYPE_MMI 0xc0
#define DNAD_REG 0x28
#define DSP_REG 0x2c
#define DSPS_REG 0x30
#define DMODE_REG_00 0x34
#define DMODE_00_BL1 0x80
#define DMODE_00_BL0 0x40
#define DMODE_BL_MASK 0xc0
#define DMODE_BL_2 0x00
#define DMODE_BL_4 0x40
#define DMODE_BL_8 0x80
#define DMODE_BL_16 0xc0
#define DMODE_700_BW16 0x20
#define DMODE_700_286 0x10
#define DMODE_700_IOM 0x08
#define DMODE_700_FAM 0x04
#define DMODE_700_PIPE 0x02
#define DMODE_MAN 0x01
#define DMODE_700_SAVE ( DMODE_00_BL_MASK | DMODE_00_BW16 | DMODE_00_286 )
#define SCRATCHA_REG_800 0x34
#define SCRATCB_REG_10 0x34
#define DMODE_REG_10 0x38
#define DMODE_800_SIOM 0x20
#define DMODE_800_DIOM 0x10
#define DMODE_800_ERL 0x08
#define DIEN_REG 0x39
#define DIEN_800_MDPE 0x40
#define DIEN_800_BF 0x20
#define DIEN_ABRT 0x10
#define DIEN_SSI 0x08
#define DIEN_SIR 0x04
#define DIEN_700_WTD 0x02
#define DIEN_700_OPC 0x01
#define DIEN_800_IID 0x01
#define DWT_REG 0x3a
#define DCNTL_REG 0x3b
#define DCNTL_700_CF1 0x80
#define DCNTL_700_CF0 0x40
#define DCNTL_700_CF_MASK 0xc0
#define DCNTL_700_CF_2 0x00
#define DCNTL_700_CF_1_5 0x40
#define DCNTL_700_CF_1 0x80
#define DCNTL_700_CF_3 0xc0
#define DCNTL_700_S16 0x20
#define DCNTL_SSM 0x10
#define DCNTL_700_LLM 0x08
#define DCNTL_800_IRQM 0x08
#define DCNTL_STD 0x04
#define DCNTL_00_RST 0x01
#define DCNTL_10_COM 0x01
#define DCNTL_700_SAVE ( DCNTL_CF_MASK | DCNTL_S16)
#define SCRATCHB_REG_00 0x3c
#define SCRATCHB_REG_800 0x5c
#define ADDER_REG_10 0x3c
#define SIEN1_REG_800 0x41
#define SIEN1_800_STO 0x04
#define SIEN1_800_GEN 0x02
#define SIEN1_800_HTH 0x01
#define SIST1_REG_800 0x43
#define SIST1_800_STO 0x04
#define SIST1_800_GEN 0x02
#define SIST1_800_HTH 0x01
#define SLPAR_REG_800 0x44
#define MACNTL_REG_800 0x46
#define MACNTL_800_TYP3 0x80
#define MACNTL_800_TYP2 0x40
#define MACNTL_800_TYP1 0x20
#define MACNTL_800_TYP0 0x10
#define MACNTL_800_DWR 0x08
#define MACNTL_800_DRD 0x04
#define MACNTL_800_PSCPT 0x02
#define MACNTL_800_SCPTS 0x01
#define GPCNTL_REG_800 0x47
#define STIME0_REG_800 0x48
#define STIME0_800_HTH_MASK 0xf0
#define STIME0_800_HTH_SHIFT 4
#define STIME0_800_SEL_MASK 0x0f
#define STIME0_800_SEL_SHIFT 0
#define STIME1_REG_800 0x49
#define STIME1_800_GEN_MASK 0x0f
#define RESPID_REG_800 0x4a
#define STEST0_REG_800 0x4c
#define STEST0_800_SLT 0x08
#define STEST0_800_ART 0x04
#define STEST0_800_SOZ 0x02
#define STEST0_800_SOM 0x01
#define STEST1_REG_800 0x4d
#define STEST1_800_SCLK 0x80
#define STEST2_REG_800 0x4e
#define STEST2_800_SCE 0x80
#define STEST2_800_ROF 0x40
#define STEST2_800_SLB 0x10
#define STEST2_800_SZM 0x08
#define STEST2_800_EXT 0x02
#define STEST2_800_LOW 0x01
#define STEST3_REG_800 0x4f
#define STEST3_800_TE 0x80
#define STEST3_800_STR 0x40
#define STEST3_800_HSC 0x20
#define STEST3_800_DSI 0x10
#define STEST3_800_TTM 0x04
#define STEST3_800_CSF 0x02
#define STEST3_800_STW 0x01
#define OPTION_PARITY 0x1
#define OPTION_TAGGED_QUEUE 0x2
#define OPTION_700 0x8
#define OPTION_INTFLY 0x10
#define OPTION_DEBUG_INTR 0x20
#define OPTION_DEBUG_INIT_ONLY 0x40
#define OPTION_DEBUG_READ_ONLY 0x80
#define OPTION_DEBUG_TRACE 0x100
#define OPTION_DEBUG_SINGLE 0x200
#define OPTION_SYNCHRONOUS 0x400
#define OPTION_MEMORY_MAPPED 0x800
#define OPTION_IO_MAPPED 0x1000
#define OPTION_DEBUG_PROBE_ONLY 0x2000
#define OPTION_DEBUG_TESTS_ONLY 0x4000
#define OPTION_DEBUG_TEST0 0x08000
#define OPTION_DEBUG_TEST1 0x10000
#define OPTION_DEBUG_TEST2 0x20000
#define OPTION_DEBUG_DUMP 0x40000
#define OPTION_DEBUG_TARGET_LIMIT 0x80000
#define OPTION_DEBUG_NCOMMANDS_LIMIT 0x100000
#define OPTION_DEBUG_SCRIPT 0x200000
#define OPTION_DEBUG_FIXUP 0x400000
#define OPTION_DEBUG_DSA 0x800000
#define OPTION_DEBUG_CORRUPTION 0x1000000
#define OPTION_DEBUG_SDTR 0x2000000
#define OPTION_DEBUG_MISMATCH 0x4000000
#define OPTION_DISCONNECT 0x8000000
#define OPTION_DEBUG_DISCONNECT 0x10000000
#define OPTION_ALWAYS_SYNCHRONOUS 0x20000000
#define OPTION_DEBUG_QUEUES 0x80000000
#define OPTION_DEBUG_ALLOCATION 0x100000000LL
#define OPTION_DEBUG_SYNCHRONOUS 0x200000000LL
#define OPTION_NO_ASYNC 0x400000000LL
#define OPTION_NO_PRINT_RACE 0x800000000LL
#if !defined(PERM_OPTIONS)
#define PERM_OPTIONS 0
#endif
struct NCR53c7x0_synchronous {
u32 select_indirect;
u32 script[8];
unsigned char synchronous_want[5];
unsigned char sxfer_sanity, scntl3_sanity;
};
#define CMD_FLAG_SDTR 1
#define CMD_FLAG_WDTR 2
#define CMD_FLAG_DID_SDTR 4
#define CMD_FLAG_DID_WDTR 8
struct NCR53c7x0_table_indirect {
u32 count;
void *address;
};
enum ncr_event {
EVENT_NONE = 0,
EVENT_ISSUE_QUEUE = 0x5000000,
EVENT_START_QUEUE,
EVENT_SELECT,
EVENT_DISCONNECT,
EVENT_RESELECT,
EVENT_COMPLETE,
EVENT_IDLE,
EVENT_SELECT_FAILED,
EVENT_BEFORE_SELECT,
EVENT_RESELECT_FAILED
};
struct NCR53c7x0_event {
enum ncr_event event;
unsigned char target;
unsigned char lun;
struct timeval time;
u32 *dsa;
unsigned long pid;
unsigned char cmnd[12];
};
struct NCR53c7x0_cmd {
void *real;
void (* free)(void *, int);
Scsi_Cmnd *cmd;
int size;
int flags;
unsigned char select[11];
volatile struct NCR53c7x0_cmd *next;
u32 *data_transfer_start;
u32 *data_transfer_end;
u32 residual[6];
u32 saved_residual[6];
u32 saved_data_pointer;
u32 dsa_next_addr;
u32 dsa_addr;
u32 dsa[0];
};
struct NCR53c7x0_break {
u32 *address, old_instruction[2];
struct NCR53c7x0_break *next;
unsigned char old_size;
};
#define STATE_HALTED 0
#define STATE_WAITING 1
#define STATE_RUNNING 2
#define STATE_ABORTING 3
#define STATE_ABORTED 4
#define STATE_DISABLED 5
#define SPECIFIC_INT_NOTHING 0
#define SPECIFIC_INT_RESTART 1
#define SPECIFIC_INT_ABORT 2
#define SPECIFIC_INT_PANIC 3
#define SPECIFIC_INT_DONE 4
#define SPECIFIC_INT_BREAK 5
struct NCR53c7x0_hostdata {
int size;
int board;
int chip;
unsigned char pci_bus, pci_device_fn;
unsigned pci_valid:1;
u32 *dsp;
unsigned dsp_changed:1;
unsigned char dstat;
unsigned dstat_valid:1;
unsigned expecting_iid:1;
unsigned expecting_sto:1;
void (* init_fixup)(struct Scsi_Host *host);
void (* init_save_regs)(struct Scsi_Host *host);
void (* dsa_fixup)(struct NCR53c7x0_cmd *cmd);
void (* soft_reset)(struct Scsi_Host *host);
int (* run_tests)(struct Scsi_Host *host);
int (* dstat_sir_intr)(struct Scsi_Host *host, struct NCR53c7x0_cmd *cmd);
int dsa_len;
s32 dsa_start;
s32 dsa_end;
s32 dsa_next;
s32 dsa_prev;
s32 dsa_cmnd;
s32 dsa_select;
s32 dsa_msgout;
s32 dsa_cmdout;
s32 dsa_dataout;
s32 dsa_datain;
s32 dsa_msgin;
s32 dsa_msgout_other;
s32 dsa_write_sync;
s32 dsa_write_resume;
s32 dsa_check_reselect;
s32 dsa_status;
s32 dsa_saved_pointer;
s32 dsa_jump_dest;
s32 E_accept_message;
s32 E_command_complete;
s32 E_data_transfer;
s32 E_dsa_code_template;
s32 E_dsa_code_template_end;
s32 E_end_data_transfer;
s32 E_msg_in;
s32 E_initiator_abort;
s32 E_other_transfer;
s32 E_other_in;
s32 E_other_out;
s32 E_target_abort;
s32 E_debug_break;
s32 E_reject_message;
s32 E_respond_message;
s32 E_select;
s32 E_select_msgout;
s32 E_test_0;
s32 E_test_1;
s32 E_test_2;
s32 E_test_3;
s32 E_dsa_zero;
s32 E_cmdout_cmdout;
s32 E_wait_reselect;
s32 E_dsa_code_begin;
long long options;
volatile u32 test_completed;
int test_running;
s32 test_source;
volatile s32 test_dest;
volatile int state;
unsigned char dmode;
unsigned char istat;
int scsi_clock;
volatile int intrs;
volatile int resets;
unsigned char saved_dmode;
unsigned char saved_ctest4;
unsigned char saved_ctest7;
unsigned char saved_dcntl;
unsigned char saved_scntl3;
unsigned char this_id_mask;
struct NCR53c7x0_break *breakpoints,
*breakpoint_current;
#ifdef NCR_DEBUG
int debug_size;
volatile int debug_count;
volatile char *debug_buf;
volatile char *debug_write;
volatile char *debug_read;
#endif
int debug_print_limit;
unsigned char debug_lun_limit[16];
int debug_count_limit;
volatile unsigned idle:1;
volatile struct NCR53c7x0_synchronous sync[16];
volatile Scsi_Cmnd *issue_queue;
volatile struct NCR53c7x0_cmd *running_list;
volatile struct NCR53c7x0_cmd *current;
volatile struct NCR53c7x0_cmd *spare;
volatile struct NCR53c7x0_cmd *free;
int max_cmd_size;
volatile int num_cmds;
volatile int extra_allocate;
volatile unsigned char cmd_allocated[16];
volatile unsigned char busy[16][8];
volatile struct NCR53c7x0_cmd *finished_queue;
volatile u32 *schedule;
volatile unsigned char msg_buf[16];
volatile unsigned char synchronous_want[16][5];
volatile unsigned char wide_want[16][4];
volatile u16 initiate_sdtr;
volatile u16 initiate_wdtr;
volatile u16 talked_to;
volatile unsigned char request_sense[16];
u32 addr_reconnect_dsa_head;
volatile u32 reconnect_dsa_head;
volatile unsigned char reselected_identify;
volatile unsigned char reselected_tag;
s32 NCR53c7xx_zero;
s32 NCR53c7xx_sink;
u32 NOP_insn;
char NCR53c7xx_msg_reject;
char NCR53c7xx_msg_abort;
char NCR53c7xx_msg_nop;
volatile int event_size, event_index;
volatile struct NCR53c7x0_event *events;
u32 *abort_script;
int script_count;
u32 script[0];
};
#define IRQ_NONE 255
#define DMA_NONE 255
#define IRQ_AUTO 254
#define DMA_AUTO 254
#define BOARD_GENERIC 0
#define NCR53c7x0_insn_size(insn) \
(((insn) & DCMD_TYPE_MASK) == DCMD_TYPE_MMI ? 3 : 2)
#define NCR53c7x0_local_declare() \
volatile unsigned char *NCR53c7x0_address_memory; \
unsigned int NCR53c7x0_address_io; \
int NCR53c7x0_memory_mapped
#define NCR53c7x0_local_setup(host) \
NCR53c7x0_address_memory = (void *) (host)->base; \
NCR53c7x0_address_io = (unsigned int) (host)->io_port; \
NCR53c7x0_memory_mapped = ((struct NCR53c7x0_hostdata *) \
host->hostdata)-> options & OPTION_MEMORY_MAPPED
#define NCR53c7x0_read8(address) \
(NCR53c7x0_memory_mapped ? \
(unsigned int)readb(NCR53c7x0_address_memory + (address)) : \
inb(NCR53c7x0_address_io + (address)))
#define NCR53c7x0_read16(address) \
(NCR53c7x0_memory_mapped ? \
(unsigned int)readw(NCR53c7x0_address_memory + (address)) : \
inw(NCR53c7x0_address_io + (address)))
#define NCR53c7x0_read32(address) \
(NCR53c7x0_memory_mapped ? \
(unsigned int) readl(NCR53c7x0_address_memory + (address)) : \
inl(NCR53c7x0_address_io + (address)))
#define NCR53c7x0_write8(address,value) \
(NCR53c7x0_memory_mapped ? \
({writeb((value), NCR53c7x0_address_memory + (address)); mb();}) : \
outb((value), NCR53c7x0_address_io + (address)))
#define NCR53c7x0_write16(address,value) \
(NCR53c7x0_memory_mapped ? \
({writew((value), NCR53c7x0_address_memory + (address)); mb();}) : \
outw((value), NCR53c7x0_address_io + (address)))
#define NCR53c7x0_write32(address,value) \
(NCR53c7x0_memory_mapped ? \
({writel((value), NCR53c7x0_address_memory + (address)); mb();}) : \
outl((value), NCR53c7x0_address_io + (address)))
#define patch_abs_32(script, offset, symbol, value) \
for (i = 0; i < (sizeof (A_##symbol##_used) / sizeof \
(u32)); ++i) { \
(script)[A_##symbol##_used[i] - (offset)] += (value); \
if (hostdata->options & OPTION_DEBUG_FIXUP) \
printk("scsi%d : %s reference %d at 0x%x in %s is now 0x%x\n",\
host->host_no, #symbol, i, A_##symbol##_used[i] - \
(int)(offset), #script, (script)[A_##symbol##_used[i] - \
(offset)]); \
}
#define patch_abs_rwri_data(script, offset, symbol, value) \
for (i = 0; i < (sizeof (A_##symbol##_used) / sizeof \
(u32)); ++i) \
(script)[A_##symbol##_used[i] - (offset)] = \
((script)[A_##symbol##_used[i] - (offset)] & \
~DBC_RWRI_IMMEDIATE_MASK) | \
(((value) << DBC_RWRI_IMMEDIATE_SHIFT) & \
DBC_RWRI_IMMEDIATE_MASK)
#define patch_abs_tci_data(script, offset, symbol, value) \
for (i = 0; i < (sizeof (A_##symbol##_used) / sizeof \
(u32)); ++i) \
(script)[A_##symbol##_used[i] - (offset)] = \
((script)[A_##symbol##_used[i] - (offset)] & \
~DBC_TCI_DATA_MASK) | \
(((value) << DBC_TCI_DATA_SHIFT) & \
DBC_TCI_DATA_MASK)
#define patch_dsa_32(dsa, symbol, word, value) \
{ \
(dsa)[(hostdata->symbol - hostdata->dsa_start) / sizeof(u32) \
+ (word)] = (value); \
if (hostdata->options & OPTION_DEBUG_DSA) \
printk("scsi : dsa %s symbol %s(%d) word %d now 0x%x\n", \
#dsa, #symbol, hostdata->symbol, \
(word), (u32) (value)); \
}
#define FATAL(host) shutdown((host));
#endif
#endif