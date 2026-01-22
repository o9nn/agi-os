#define ASC_VERSION "3.1E"
#define ASC_LINUX_VERSION(V, P, S)    (((V) * 65536) + ((P) * 256) + (S))
#ifndef LINUX_VERSION_CODE
#include <linux/version.h>
#endif
#include <linux/config.h>
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
#ifdef MODULE
#include <linux/module.h>
#endif
#endif
#include <linux/string.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/head.h>
#include <linux/types.h>
#include <linux/ioport.h>
#include <linux/delay.h>
#include <linux/malloc.h>
#include <linux/mm.h>
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
#include <linux/proc_fs.h>
#endif
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(2,1,23)
#include <linux/init.h>
#endif
#include <asm/io.h>
#include <asm/system.h>
#include <asm/dma.h>
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
#include "../block/blk.h"
#else
#include <linux/blk.h>
#include <linux/stat.h>
#endif
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(2,1,95)
#include <asm/spinlock.h>
#endif
#include "scsi.h"
#include "hosts.h"
#include "sd.h"
#include "advansys.h"
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(2,1,93)
#ifdef CONFIG_PCI
#include <linux/pci.h>
#endif
#else
#define ASC_CONFIG_PCI
#endif
#define DID_UNDERRUN 0
#define ADVANSYS_ASSERT
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
#undef ADVANSYS_STATS
#else
#define ADVANSYS_STATS
#endif
#ifdef ADVANSYS_DEBUG
#define STATIC
#else
#define STATIC static
#endif
#define ASC_LIB_VERSION_MAJOR  1
#define ASC_LIB_VERSION_MINOR  22
#define ASC_LIB_SERIAL_NUMBER  113
typedef unsigned char uchar;
#ifndef NULL
#define NULL     (0)
#endif
#ifndef TRUE
#define TRUE     (1)
#endif
#ifndef FALSE
#define FALSE    (0)
#endif
#define  REG     register
#define rchar    REG __s8
#define rshort   REG __s16
#define rint     REG __s32
#define rlong    REG __s32
#define ruchar   REG __u8
#define rushort  REG __u16
#define ruint    REG __u32
#define rulong   REG __u32
#define NULLPTR  (void *)0
#define FNULLPTR (void *)0UL
#define EOF      (-1)
#define EOS      '\0'
#define ERR      (-1)
#define UB_ERR   (uchar)(0xFF)
#define UW_ERR   (uint)(0xFFFF)
#define UL_ERR   (ulong)(0xFFFFFFFFUL)
#define iseven_word(val)  ((((uint)val) & (uint)0x0001) == 0)
#define isodd_word(val)   ((((uint)val) & (uint)0x0001) != 0)
#define toeven_word(val)  (((uint)val) & (uint)0xFFFE)
#define biton(val, bits)   (((uint)(val >> bits) & (uint)0x0001) != 0)
#define bitoff(val, bits)  (((uint)(val >> bits) & (uint)0x0001) == 0)
#define lbiton(val, bits)  (((ulong)(val >> bits) & (ulong)0x00000001UL) != 0)
#define lbitoff(val, bits) (((ulong)(val >> bits) & (ulong)0x00000001UL) == 0)
#define  absh(val)    ((val) < 0 ? -(val) : (val))
#define  swapbyte(ch)  ((((ch) << 4) | ((ch) >> 4)))
#ifndef GBYTE
#define GBYTE       (0x40000000UL)
#endif
#ifndef MBYTE
#define MBYTE       (0x100000UL)
#endif
#ifndef KBYTE
#define KBYTE       (0x400)
#endif
#define HI_BYTE(x) (*((__u8 *)(&x)+1))
#define LO_BYTE(x) (*((__u8 *)&x))
#define HI_WORD(x) (*((__u16 *)(&x)+1))
#define LO_WORD(x) (*((__u16 *)&x))
#ifndef MAKEWORD
#define MAKEWORD(lo, hi)    ((__u16) (((__u16) lo) | ((__u16) hi << 8)))
#endif
#ifndef MAKELONG
#define MAKELONG(lo, hi)    ((__u32) (((__u32) lo) | ((__u32) hi << 16)))
#endif
#define SwapWords(dWord)        ((__u32) ((dWord >> 16) | (dWord << 16)))
#define SwapBytes(word)         ((__u16) ((word >> 8) | (word << 8)))
#define BigToLittle(dWord) ((__u32) (SwapWords(MAKELONG(SwapBytes(LO_WORD(dWord)), SwapBytes(HI_WORD(dWord))))))
#define LittleToBig(dWord)      BigToLittle(dWord)
#define AscPCIConfigVendorIDRegister      0x0000
#define AscPCIConfigDeviceIDRegister      0x0002
#define AscPCIConfigCommandRegister       0x0004
#define AscPCIConfigStatusRegister        0x0006
#define AscPCIConfigRevisionIDRegister    0x0008
#define AscPCIConfigCacheSize             0x000C
#define AscPCIConfigLatencyTimer          0x000D
#define AscPCIIOBaseRegister              0x0010
#define AscPCICmdRegBits_IOMemBusMaster   0x0007
#define ASC_PCI_ID2BUS(id)    ((id) & 0xFF)
#define ASC_PCI_ID2DEV(id)    (((id) >> 11) & 0x1F)
#define ASC_PCI_ID2FUNC(id)   (((id) >> 8) & 0x7)
#define ASC_PCI_MKID(bus, dev, func) ((((dev) & 0x1F) << 11) | (((func) & 0x7) << 8) | ((bus) & 0xFF))
#define ASC_PCI_VENDORID                  0x10CD
#define ASC_PCI_DEVICEID_1200A            0x1100
#define ASC_PCI_DEVICEID_1200B            0x1200
#define ASC_PCI_DEVICEID_ULTRA            0x1300
#define ASC_PCI_REVISION_3150             0x02
#define ASC_PCI_REVISION_3050             0x03
#define  ASC_DVCLIB_CALL_DONE     (1)
#define  ASC_DVCLIB_CALL_FAILED   (0)
#define  ASC_DVCLIB_CALL_ERROR    (-1)
#define PortAddr            unsigned short
#define Ptr2Func            ulong
#define inp(port)           inb(port)
#define inpw(port)          inw(port)
#define inpl(port)          inl(port)
#define outp(port, byte)    outb((byte), (port))
#define outpw(port, word)   outw((word), (port))
#define outpl(port, long)   outl((long), (port))
#define ASC_MAX_SG_QUEUE    7
#define ASC_MAX_SG_LIST     SG_ALL
#define ASC_CS_TYPE  unsigned short
#ifndef asc_ptr_type
#define asc_ptr_type
#endif
#ifndef ASC_GET_PTR2FUNC
#define ASC_GET_PTR2FUNC(fun)  (Ptr2Func)(fun)
#endif
#define FLIP_BYTE_NIBBLE(x)    (((x<<4)& 0xFF) | (x>>4))
#define ASC_IS_ISA          (0x0001)
#define ASC_IS_ISAPNP       (0x0081)
#define ASC_IS_EISA         (0x0002)
#define ASC_IS_PCI          (0x0004)
#define ASC_IS_PCI_ULTRA    (0x0104)
#define ASC_IS_PCMCIA       (0x0008)
#define ASC_IS_MCA          (0x0020)
#define ASC_IS_VL           (0x0040)
#define ASC_ISA_PNP_PORT_ADDR  (0x279)
#define ASC_ISA_PNP_PORT_WRITE (ASC_ISA_PNP_PORT_ADDR+0x800)
#define ASC_IS_WIDESCSI_16  (0x0100)
#define ASC_IS_WIDESCSI_32  (0x0200)
#define ASC_IS_BIG_ENDIAN   (0x8000)
#define ASC_CHIP_MIN_VER_VL      (0x01)
#define ASC_CHIP_MAX_VER_VL      (0x07)
#define ASC_CHIP_MIN_VER_PCI     (0x09)
#define ASC_CHIP_MAX_VER_PCI     (0x0F)
#define ASC_CHIP_VER_PCI_BIT     (0x08)
#define ASC_CHIP_MIN_VER_ISA     (0x11)
#define ASC_CHIP_MIN_VER_ISA_PNP (0x21)
#define ASC_CHIP_MAX_VER_ISA     (0x27)
#define ASC_CHIP_VER_ISA_BIT     (0x30)
#define ASC_CHIP_VER_ISAPNP_BIT  (0x20)
#define ASC_CHIP_VER_ASYN_BUG    (0x21)
#define ASC_CHIP_VER_PCI             0x08
#define ASC_CHIP_VER_PCI_ULTRA_3150  (ASC_CHIP_VER_PCI | 0x02)
#define ASC_CHIP_VER_PCI_ULTRA_3050  (ASC_CHIP_VER_PCI | 0x03)
#define ASC_CHIP_MIN_VER_EISA (0x41)
#define ASC_CHIP_MAX_VER_EISA (0x47)
#define ASC_CHIP_VER_EISA_BIT (0x40)
#define ASC_CHIP_LATEST_VER_EISA   ((ASC_CHIP_MIN_VER_EISA - 1) + 3)
#define ASC_MAX_LIB_SUPPORTED_ISA_CHIP_VER   0x21
#define ASC_MAX_LIB_SUPPORTED_PCI_CHIP_VER   0x0A
#define ASC_MAX_VL_DMA_ADDR     (0x07FFFFFFL)
#define ASC_MAX_VL_DMA_COUNT    (0x07FFFFFFL)
#define ASC_MAX_PCI_DMA_ADDR    (0xFFFFFFFFL)
#define ASC_MAX_PCI_DMA_COUNT   (0xFFFFFFFFL)
#define ASC_MAX_ISA_DMA_ADDR    (0x00FFFFFFL)
#define ASC_MAX_ISA_DMA_COUNT   (0x00FFFFFFL)
#define ASC_MAX_EISA_DMA_ADDR   (0x07FFFFFFL)
#define ASC_MAX_EISA_DMA_COUNT  (0x07FFFFFFL)
#ifndef inpw_noswap
#define inpw_noswap(port)         inpw(port)
#endif
#ifndef outpw_noswap
#define outpw_noswap(port, data)  outpw(port, data)
#endif
#define ASC_SCSI_ID_BITS  3
#define ASC_SCSI_TIX_TYPE     uchar
#define ASC_ALL_DEVICE_BIT_SET  0xFF
#ifdef ASC_WIDESCSI_16
#undef  ASC_SCSI_ID_BITS
#define ASC_SCSI_ID_BITS  4
#define ASC_ALL_DEVICE_BIT_SET  0xFFFF
#endif
#ifdef ASC_WIDESCSI_32
#undef  ASC_SCSI_ID_BITS
#define ASC_SCSI_ID_BITS  5
#define ASC_ALL_DEVICE_BIT_SET  0xFFFFFFFFL
#endif
#if ASC_SCSI_ID_BITS == 3
#define ASC_SCSI_BIT_ID_TYPE  uchar
#define ASC_MAX_TID       7
#define ASC_MAX_LUN       7
#define ASC_SCSI_WIDTH_BIT_SET  0xFF
#elif ASC_SCSI_ID_BITS == 4
#define ASC_SCSI_BIT_ID_TYPE   ushort
#define ASC_MAX_TID         15
#define ASC_MAX_LUN         7
#define ASC_SCSI_WIDTH_BIT_SET  0xFFFF
#elif ASC_SCSI_ID_BITS == 5
#define ASC_SCSI_BIT_ID_TYPE    ulong
#define ASC_MAX_TID         31
#define ASC_MAX_LUN         7
#define ASC_SCSI_WIDTH_BIT_SET  0xFFFFFFFF
#else
#error  ASC_SCSI_ID_BITS definition is wrong
#endif
#define ASC_MAX_SENSE_LEN   32
#define ASC_MIN_SENSE_LEN   14
#define ASC_MAX_CDB_LEN     12
#define ASC_SCSI_RESET_HOLD_TIME_US  60
#define SCSICMD_TestUnitReady     0x00
#define SCSICMD_Rewind            0x01
#define SCSICMD_Rezero            0x01
#define SCSICMD_RequestSense      0x03
#define SCSICMD_Format            0x04
#define SCSICMD_FormatUnit        0x04
#define SCSICMD_Read6             0x08
#define SCSICMD_Write6            0x0A
#define SCSICMD_Seek6             0x0B
#define SCSICMD_Inquiry           0x12
#define SCSICMD_Verify6           0x13
#define SCSICMD_ModeSelect6       0x15
#define SCSICMD_ModeSense6        0x1A
#define SCSICMD_StartStopUnit     0x1B
#define SCSICMD_LoadUnloadTape    0x1B
#define SCSICMD_ReadCapacity      0x25
#define SCSICMD_Read10            0x28
#define SCSICMD_Write10           0x2A
#define SCSICMD_Seek10            0x2B
#define SCSICMD_Erase10           0x2C
#define SCSICMD_WriteAndVerify10  0x2E
#define SCSICMD_Verify10          0x2F
#define SCSICMD_WriteBuffer       0x3B
#define SCSICMD_ReadBuffer        0x3C
#define SCSICMD_ReadLong          0x3E
#define SCSICMD_WriteLong         0x3F
#define SCSICMD_ReadTOC           0x43
#define SCSICMD_ReadHeader        0x44
#define SCSICMD_ModeSelect10      0x55
#define SCSICMD_ModeSense10       0x5A
#define SCSI_TYPE_DASD     0x00
#define SCSI_TYPE_SASD     0x01
#define SCSI_TYPE_PRN      0x02
#define SCSI_TYPE_PROC     0x03
#define SCSI_TYPE_WORM     0x04
#define SCSI_TYPE_CDROM    0x05
#define SCSI_TYPE_SCANNER  0x06
#define SCSI_TYPE_OPTMEM   0x07
#define SCSI_TYPE_MED_CHG  0x08
#define SCSI_TYPE_COMM     0x09
#define SCSI_TYPE_UNKNOWN  0x1F
#define SCSI_TYPE_NO_DVC   0xFF
#define ASC_SCSIDIR_NOCHK    0x00
#define ASC_SCSIDIR_T2H      0x08
#define ASC_SCSIDIR_H2T      0x10
#define ASC_SCSIDIR_NODATA   0x18
#define SCSI_SENKEY_NO_SENSE      0x00
#define SCSI_SENKEY_UNDEFINED     0x01
#define SCSI_SENKEY_NOT_READY     0x02
#define SCSI_SENKEY_MEDIUM_ERR    0x03
#define SCSI_SENKEY_HW_ERR        0x04
#define SCSI_SENKEY_ILLEGAL       0x05
#define SCSI_SENKEY_ATTENTION     0x06
#define SCSI_SENKEY_PROTECTED     0x07
#define SCSI_SENKEY_BLANK         0x08
#define SCSI_SENKEY_V_UNIQUE      0x09
#define SCSI_SENKEY_CPY_ABORT     0x0A
#define SCSI_SENKEY_ABORT         0x0B
#define SCSI_SENKEY_EQUAL         0x0C
#define SCSI_SENKEY_VOL_OVERFLOW  0x0D
#define SCSI_SENKEY_MISCOMP       0x0E
#define SCSI_SENKEY_RESERVED      0x0F
#define SCSI_ASC_NOMEDIA          0x3A
#define ASC_SRB_HOST(x)  ((uchar)((uchar)(x) >> 4))
#define ASC_SRB_TID(x)   ((uchar)((uchar)(x) & (uchar)0x0F))
#define ASC_SRB_LUN(x)   ((uchar)((uint)(x) >> 13))
#define PUT_CDB1(x)   ((uchar)((uint)(x) >> 8))
#define SS_GOOD              0x00
#define SS_CHK_CONDITION     0x02
#define SS_CONDITION_MET     0x04
#define SS_TARGET_BUSY       0x08
#define SS_INTERMID          0x10
#define SS_INTERMID_COND_MET 0x14
#define SS_RSERV_CONFLICT    0x18
#define SS_CMD_TERMINATED    0x22
#define SS_QUEUE_FULL        0x28
#define MS_CMD_DONE    0x00
#define MS_EXTEND      0x01
#define MS_SDTR_LEN    0x03
#define MS_SDTR_CODE   0x01
#define MS_WDTR_LEN    0x02
#define MS_WDTR_CODE   0x03
#define MS_MDP_LEN    0x05
#define MS_MDP_CODE   0x00
#define M1_SAVE_DATA_PTR        0x02
#define M1_RESTORE_PTRS         0x03
#define M1_DISCONNECT           0x04
#define M1_INIT_DETECTED_ERR    0x05
#define M1_ABORT                0x06
#define M1_MSG_REJECT           0x07
#define M1_NO_OP                0x08
#define M1_MSG_PARITY_ERR       0x09
#define M1_LINK_CMD_DONE        0x0A
#define M1_LINK_CMD_DONE_WFLAG  0x0B
#define M1_BUS_DVC_RESET        0x0C
#define M1_ABORT_TAG            0x0D
#define M1_CLR_QUEUE            0x0E
#define M1_INIT_RECOVERY        0x0F
#define M1_RELEASE_RECOVERY     0x10
#define M1_KILL_IO_PROC         0x11
#define M2_QTAG_MSG_SIMPLE      0x20
#define M2_QTAG_MSG_HEAD        0x21
#define M2_QTAG_MSG_ORDERED     0x22
#define M2_IGNORE_WIDE_RESIDUE  0x23
typedef struct {
uchar               peri_dvc_type:5;
uchar               peri_qualifier:3;
} ASC_SCSI_INQ0;
typedef struct {
uchar               dvc_type_modifier:7;
uchar               rmb:1;
} ASC_SCSI_INQ1;
typedef struct {
uchar               ansi_apr_ver:3;
uchar               ecma_ver:3;
uchar               iso_ver:2;
} ASC_SCSI_INQ2;
typedef struct {
uchar               rsp_data_fmt:4;
uchar               res:2;
uchar               TemIOP:1;
uchar               aenc:1;
} ASC_SCSI_INQ3;
typedef struct {
uchar               StfRe:1;
uchar               CmdQue:1;
uchar               Reserved:1;
uchar               Linked:1;
uchar               Sync:1;
uchar               WBus16:1;
uchar               WBus32:1;
uchar               RelAdr:1;
} ASC_SCSI_INQ7;
typedef struct {
ASC_SCSI_INQ0       byte0;
ASC_SCSI_INQ1       byte1;
ASC_SCSI_INQ2       byte2;
ASC_SCSI_INQ3       byte3;
uchar               add_len;
uchar               res1;
uchar               res2;
ASC_SCSI_INQ7       byte7;
uchar               vendor_id[8];
uchar               product_id[16];
uchar               product_rev_level[4];
} ASC_SCSI_INQUIRY;
typedef struct asc_req_sense {
uchar               err_code:7;
uchar               info_valid:1;
uchar               segment_no;
uchar               sense_key:4;
uchar               reserved_bit:1;
uchar               sense_ILI:1;
uchar               sense_EOM:1;
uchar               file_mark:1;
uchar               info1[4];
uchar               add_sense_len;
uchar               cmd_sp_info[4];
uchar               asc;
uchar               ascq;
uchar               fruc;
uchar               sks_byte0:7;
uchar               sks_valid:1;
uchar               sks_bytes[2];
uchar               notused[2];
uchar               ex_sense_code;
uchar               info2[4];
} ASC_REQ_SENSE;
#define ASC_SG_LIST_PER_Q   7
#define QS_FREE        0x00
#define QS_READY       0x01
#define QS_DISC1       0x02
#define QS_DISC2       0x04
#define QS_BUSY        0x08
#define QS_ABORTED     0x40
#define QS_DONE        0x80
#define QC_NO_CALLBACK   0x01
#define QC_SG_SWAP_QUEUE 0x02
#define QC_SG_HEAD       0x04
#define QC_DATA_IN       0x08
#define QC_DATA_OUT      0x10
#define QC_URGENT        0x20
#define QC_MSG_OUT       0x40
#define QC_REQ_SENSE     0x80
#define QCSG_SG_XFER_LIST  0x02
#define QCSG_SG_XFER_MORE  0x04
#define QCSG_SG_XFER_END   0x08
#define QD_IN_PROGRESS       0x00
#define QD_NO_ERROR          0x01
#define QD_ABORTED_BY_HOST   0x02
#define QD_WITH_ERROR        0x04
#define QD_INVALID_REQUEST   0x80
#define QD_INVALID_HOST_NUM  0x81
#define QD_INVALID_DEVICE    0x82
#define QD_ERR_INTERNAL      0xFF
#define QHSTA_NO_ERROR               0x00
#define QHSTA_M_SEL_TIMEOUT          0x11
#define QHSTA_M_DATA_OVER_RUN        0x12
#define QHSTA_M_DATA_UNDER_RUN       0x12
#define QHSTA_M_UNEXPECTED_BUS_FREE  0x13
#define QHSTA_M_BAD_BUS_PHASE_SEQ    0x14
#define QHSTA_D_QDONE_SG_LIST_CORRUPTED 0x21
#define QHSTA_D_ASC_DVC_ERROR_CODE_SET  0x22
#define QHSTA_D_HOST_ABORT_FAILED       0x23
#define QHSTA_D_EXE_SCSI_Q_FAILED       0x24
#define QHSTA_D_EXE_SCSI_Q_BUSY_TIMEOUT 0x25
#define QHSTA_D_ASPI_NO_BUF_POOL        0x26
#define QHSTA_M_WTM_TIMEOUT         0x41
#define QHSTA_M_BAD_CMPL_STATUS_IN  0x42
#define QHSTA_M_NO_AUTO_REQ_SENSE   0x43
#define QHSTA_M_AUTO_REQ_SENSE_FAIL 0x44
#define QHSTA_M_TARGET_STATUS_BUSY  0x45
#define QHSTA_M_BAD_TAG_CODE        0x46
#define QHSTA_M_BAD_QUEUE_FULL_OR_BUSY  0x47
#define QHSTA_M_HUNG_REQ_SCSI_BUS_RESET 0x48
#define QHSTA_D_LRAM_CMP_ERROR        0x81
#define QHSTA_M_MICRO_CODE_ERROR_HALT 0xA1
#define ASC_FLAG_SCSIQ_REQ        0x01
#define ASC_FLAG_BIOS_SCSIQ_REQ   0x02
#define ASC_FLAG_BIOS_ASYNC_IO    0x04
#define ASC_FLAG_SRB_LINEAR_ADDR  0x08
#define ASC_FLAG_WIN16            0x10
#define ASC_FLAG_WIN32            0x20
#define ASC_FLAG_ISA_OVER_16MB    0x40
#define ASC_FLAG_DOS_VM_CALLBACK  0x80
#define ASC_TAG_FLAG_EXTRA_BYTES               0x10
#define ASC_TAG_FLAG_DISABLE_DISCONNECT        0x04
#define ASC_TAG_FLAG_DISABLE_ASYN_USE_SYN_FIX  0x08
#define ASC_TAG_FLAG_DISABLE_CHK_COND_INT_HOST 0x40
#define ASC_SCSIQ_CPY_BEG              4
#define ASC_SCSIQ_SGHD_CPY_BEG         2
#define ASC_SCSIQ_B_FWD                0
#define ASC_SCSIQ_B_BWD                1
#define ASC_SCSIQ_B_STATUS             2
#define ASC_SCSIQ_B_QNO                3
#define ASC_SCSIQ_B_CNTL               4
#define ASC_SCSIQ_B_SG_QUEUE_CNT       5
#define ASC_SCSIQ_D_DATA_ADDR          8
#define ASC_SCSIQ_D_DATA_CNT          12
#define ASC_SCSIQ_B_SENSE_LEN         20
#define ASC_SCSIQ_DONE_INFO_BEG       22
#define ASC_SCSIQ_D_SRBPTR            22
#define ASC_SCSIQ_B_TARGET_IX         26
#define ASC_SCSIQ_B_CDB_LEN           28
#define ASC_SCSIQ_B_TAG_CODE          29
#define ASC_SCSIQ_W_VM_ID             30
#define ASC_SCSIQ_DONE_STATUS         32
#define ASC_SCSIQ_HOST_STATUS         33
#define ASC_SCSIQ_SCSI_STATUS         34
#define ASC_SCSIQ_CDB_BEG             36
#define ASC_SCSIQ_DW_REMAIN_XFER_ADDR 56
#define ASC_SCSIQ_DW_REMAIN_XFER_CNT  60
#define ASC_SCSIQ_B_SG_WK_QP          49
#define ASC_SCSIQ_B_SG_WK_IX          50
#define ASC_SCSIQ_W_REQ_COUNT         52
#define ASC_SCSIQ_B_LIST_CNT          6
#define ASC_SCSIQ_B_CUR_LIST_CNT      7
#define ASC_SGQ_B_SG_CNTL             4
#define ASC_SGQ_B_SG_HEAD_QP          5
#define ASC_SGQ_B_SG_LIST_CNT         6
#define ASC_SGQ_B_SG_CUR_LIST_CNT     7
#define ASC_SGQ_LIST_BEG              8
#define ASC_DEF_SCSI1_QNG    4
#define ASC_MAX_SCSI1_QNG    4
#define ASC_DEF_SCSI2_QNG    16
#define ASC_MAX_SCSI2_QNG    32
#define ASC_TAG_CODE_MASK    0x23
#define ASC_STOP_REQ_RISC_STOP      0x01
#define ASC_STOP_ACK_RISC_STOP      0x03
#define ASC_STOP_CLEAN_UP_BUSY_Q    0x10
#define ASC_STOP_CLEAN_UP_DISC_Q    0x20
#define ASC_STOP_HOST_REQ_RISC_HALT 0x40
#define ASC_TIDLUN_TO_IX(tid, lun)  (ASC_SCSI_TIX_TYPE)((tid) + ((lun)<<ASC_SCSI_ID_BITS))
#define ASC_TID_TO_TARGET_ID(tid)   (ASC_SCSI_BIT_ID_TYPE)(0x01 << (tid))
#define ASC_TIX_TO_TARGET_ID(tix)   (0x01 << ((tix) & ASC_MAX_TID))
#define ASC_TIX_TO_TID(tix)         ((tix) & ASC_MAX_TID)
#define ASC_TID_TO_TIX(tid)         ((tid) & ASC_MAX_TID)
#define ASC_TIX_TO_LUN(tix)         (((tix) >> ASC_SCSI_ID_BITS) & ASC_MAX_LUN)
#define ASC_QNO_TO_QADDR(q_no)      ((ASC_QADR_BEG)+((int)(q_no) << 6))
typedef struct asc_scisq_1 {
uchar               status;
uchar               q_no;
uchar               cntl;
uchar               sg_queue_cnt;
uchar               target_id;
uchar               target_lun;
ulong               data_addr;
ulong               data_cnt;
ulong               sense_addr;
uchar               sense_len;
uchar               extra_bytes;
} ASC_SCSIQ_1;
typedef struct asc_scisq_2 {
ulong               srb_ptr;
uchar               target_ix;
uchar               flag;
uchar               cdb_len;
uchar               tag_code;
ushort              vm_id;
} ASC_SCSIQ_2;
typedef struct asc_scsiq_3 {
uchar               done_stat;
uchar               host_stat;
uchar               scsi_stat;
uchar               scsi_msg;
} ASC_SCSIQ_3;
typedef struct asc_scsiq_4 {
uchar               cdb[ASC_MAX_CDB_LEN];
uchar               y_first_sg_list_qp;
uchar               y_working_sg_qp;
uchar               y_working_sg_ix;
uchar               y_res;
ushort              x_req_count;
ushort              x_reconnect_rtn;
ulong               x_saved_data_addr;
ulong               x_saved_data_cnt;
} ASC_SCSIQ_4;
typedef struct asc_q_done_info {
ASC_SCSIQ_2         d2;
ASC_SCSIQ_3         d3;
uchar               q_status;
uchar               q_no;
uchar               cntl;
uchar               sense_len;
uchar               extra_bytes;
uchar               res;
ulong               remain_bytes;
} ASC_QDONE_INFO;
typedef struct asc_sg_list {
ulong               addr;
ulong               bytes;
} ASC_SG_LIST;
typedef struct asc_sg_head {
ushort              entry_cnt;
ushort              queue_cnt;
ushort              entry_to_copy;
ushort              res;
ASC_SG_LIST         sg_list[ASC_MAX_SG_LIST];
} ASC_SG_HEAD;
#define ASC_MIN_SG_LIST   2
typedef struct asc_min_sg_head {
ushort              entry_cnt;
ushort              queue_cnt;
ushort              entry_to_copy;
ushort              res;
ASC_SG_LIST         sg_list[ASC_MIN_SG_LIST];
} ASC_MIN_SG_HEAD;
#define QCX_SORT        (0x0001)
#define QCX_COALEASE    (0x0002)
typedef struct asc_scsi_q {
ASC_SCSIQ_1         q1;
ASC_SCSIQ_2         q2;
uchar               *cdbptr;
ASC_SG_HEAD         *sg_head;
} ASC_SCSI_Q;
typedef struct asc_scsi_req_q {
ASC_SCSIQ_1         r1;
ASC_SCSIQ_2         r2;
uchar       *cdbptr;
ASC_SG_HEAD *sg_head;
uchar               *sense_ptr;
ASC_SCSIQ_3         r3;
uchar               cdb[ASC_MAX_CDB_LEN];
uchar               sense[ASC_MIN_SENSE_LEN];
} ASC_SCSI_REQ_Q;
typedef struct asc_scsi_bios_req_q {
ASC_SCSIQ_1         r1;
ASC_SCSIQ_2         r2;
uchar               *cdbptr;
ASC_SG_HEAD         *sg_head;
uchar               *sense_ptr;
ASC_SCSIQ_3         r3;
uchar               cdb[ASC_MAX_CDB_LEN];
uchar               sense[ASC_MIN_SENSE_LEN];
} ASC_SCSI_BIOS_REQ_Q;
typedef struct asc_risc_q {
uchar               fwd;
uchar               bwd;
ASC_SCSIQ_1         i1;
ASC_SCSIQ_2         i2;
ASC_SCSIQ_3         i3;
ASC_SCSIQ_4         i4;
} ASC_RISC_Q;
typedef struct asc_sg_list_q {
uchar               seq_no;
uchar               q_no;
uchar               cntl;
uchar               sg_head_qp;
uchar               sg_list_cnt;
uchar               sg_cur_list_cnt;
} ASC_SG_LIST_Q;
typedef struct asc_risc_sg_list_q {
uchar               fwd;
uchar               bwd;
ASC_SG_LIST_Q       sg;
ASC_SG_LIST         sg_list[7];
} ASC_RISC_SG_LIST_Q;
#define ASC_EXE_SCSI_IO_MAX_IDLE_LOOP  0x1000000UL
#define ASC_EXE_SCSI_IO_MAX_WAIT_LOOP  1024
#define ASCQ_ERR_NO_ERROR             0
#define ASCQ_ERR_IO_NOT_FOUND         1
#define ASCQ_ERR_LOCAL_MEM            2
#define ASCQ_ERR_CHKSUM               3
#define ASCQ_ERR_START_CHIP           4
#define ASCQ_ERR_INT_TARGET_ID        5
#define ASCQ_ERR_INT_LOCAL_MEM        6
#define ASCQ_ERR_HALT_RISC            7
#define ASCQ_ERR_GET_ASPI_ENTRY       8
#define ASCQ_ERR_CLOSE_ASPI           9
#define ASCQ_ERR_HOST_INQUIRY         0x0A
#define ASCQ_ERR_SAVED_SRB_BAD        0x0B
#define ASCQ_ERR_QCNTL_SG_LIST        0x0C
#define ASCQ_ERR_Q_STATUS             0x0D
#define ASCQ_ERR_WR_SCSIQ             0x0E
#define ASCQ_ERR_PC_ADDR              0x0F
#define ASCQ_ERR_SYN_OFFSET           0x10
#define ASCQ_ERR_SYN_XFER_TIME        0x11
#define ASCQ_ERR_LOCK_DMA             0x12
#define ASCQ_ERR_UNLOCK_DMA           0x13
#define ASCQ_ERR_VDS_CHK_INSTALL      0x14
#define ASCQ_ERR_MICRO_CODE_HALT      0x15
#define ASCQ_ERR_SET_LRAM_ADDR        0x16
#define ASCQ_ERR_CUR_QNG              0x17
#define ASCQ_ERR_SG_Q_LINKS           0x18
#define ASCQ_ERR_SCSIQ_PTR            0x19
#define ASCQ_ERR_ISR_RE_ENTRY         0x1A
#define ASCQ_ERR_CRITICAL_RE_ENTRY    0x1B
#define ASCQ_ERR_ISR_ON_CRITICAL      0x1C
#define ASCQ_ERR_SG_LIST_ODD_ADDRESS  0x1D
#define ASCQ_ERR_XFER_ADDRESS_TOO_BIG 0x1E
#define ASCQ_ERR_SCSIQ_NULL_PTR       0x1F
#define ASCQ_ERR_SCSIQ_BAD_NEXT_PTR   0x20
#define ASCQ_ERR_GET_NUM_OF_FREE_Q    0x21
#define ASCQ_ERR_SEND_SCSI_Q          0x22
#define ASCQ_ERR_HOST_REQ_RISC_HALT   0x23
#define ASCQ_ERR_RESET_SDTR           0x24
#define ASC_WARN_NO_ERROR             0x0000
#define ASC_WARN_IO_PORT_ROTATE       0x0001
#define ASC_WARN_EEPROM_CHKSUM        0x0002
#define ASC_WARN_IRQ_MODIFIED         0x0004
#define ASC_WARN_AUTO_CONFIG          0x0008
#define ASC_WARN_CMD_QNG_CONFLICT     0x0010
#define ASC_WARN_EEPROM_RECOVER       0x0020
#define ASC_WARN_CFG_MSW_RECOVER      0x0040
#define ASC_WARN_SET_PCI_CONFIG_SPACE 0x0080
#define ASC_IERR_WRITE_EEPROM         0x0001
#define ASC_IERR_MCODE_CHKSUM         0x0002
#define ASC_IERR_SET_PC_ADDR          0x0004
#define ASC_IERR_START_STOP_CHIP      0x0008
#define ASC_IERR_IRQ_NO               0x0010
#define ASC_IERR_SET_IRQ_NO           0x0020
#define ASC_IERR_CHIP_VERSION         0x0040
#define ASC_IERR_SET_SCSI_ID          0x0080
#define ASC_IERR_GET_PHY_ADDR         0x0100
#define ASC_IERR_BAD_SIGNATURE        0x0200
#define ASC_IERR_NO_BUS_TYPE          0x0400
#define ASC_IERR_SCAM                 0x0800
#define ASC_IERR_SET_SDTR             0x1000
#define ASC_IERR_RW_LRAM              0x8000
#define ASC_DEF_IRQ_NO  10
#define ASC_MAX_IRQ_NO  15
#define ASC_MIN_IRQ_NO  10
#define ASC_MIN_REMAIN_Q        (0x02)
#define ASC_DEF_MAX_TOTAL_QNG   (0xF0)
#define ASC_MIN_TAG_Q_PER_DVC   (0x04)
#define ASC_DEF_TAG_Q_PER_DVC   (0x04)
#define ASC_MIN_FREE_Q        ASC_MIN_REMAIN_Q
#define ASC_MIN_TOTAL_QNG     ((ASC_MAX_SG_QUEUE)+(ASC_MIN_FREE_Q))
#define ASC_MAX_TOTAL_QNG 240
#define ASC_MAX_PCI_ULTRA_INRAM_TOTAL_QNG 16
#define ASC_MAX_PCI_ULTRA_INRAM_TAG_QNG   8
#define ASC_MAX_PCI_INRAM_TOTAL_QNG  20
#define ASC_MAX_INRAM_TAG_QNG   16
#define ASC_IOADR_TABLE_MAX_IX  11
#define ASC_IOADR_GAP   0x10
#define ASC_SEARCH_IOP_GAP 0x10
#define ASC_MIN_IOP_ADDR   (PortAddr)0x0100
#define ASC_MAX_IOP_ADDR   (PortAddr)0x3F0
#define ASC_IOADR_1     (PortAddr)0x0110
#define ASC_IOADR_2     (PortAddr)0x0130
#define ASC_IOADR_3     (PortAddr)0x0150
#define ASC_IOADR_4     (PortAddr)0x0190
#define ASC_IOADR_5     (PortAddr)0x0210
#define ASC_IOADR_6     (PortAddr)0x0230
#define ASC_IOADR_7     (PortAddr)0x0250
#define ASC_IOADR_8     (PortAddr)0x0330
#define ASC_IOADR_DEF   ASC_IOADR_8
#define ASC_LIB_SCSIQ_WK_SP        256
#define ASC_MAX_SYN_XFER_NO        16
#define ASC_SYN_MAX_OFFSET         0x0F
#define ASC_DEF_SDTR_OFFSET        0x0F
#define ASC_DEF_SDTR_INDEX         0x00
#define ASC_SDTR_ULTRA_PCI_10MB_INDEX  0x02
#define SYN_XFER_NS_0  25
#define SYN_XFER_NS_1  30
#define SYN_XFER_NS_2  35
#define SYN_XFER_NS_3  40
#define SYN_XFER_NS_4  50
#define SYN_XFER_NS_5  60
#define SYN_XFER_NS_6  70
#define SYN_XFER_NS_7  85
#define SYN_ULTRA_XFER_NS_0    12
#define SYN_ULTRA_XFER_NS_1    19
#define SYN_ULTRA_XFER_NS_2    25
#define SYN_ULTRA_XFER_NS_3    32
#define SYN_ULTRA_XFER_NS_4    38
#define SYN_ULTRA_XFER_NS_5    44
#define SYN_ULTRA_XFER_NS_6    50
#define SYN_ULTRA_XFER_NS_7    57
#define SYN_ULTRA_XFER_NS_8    63
#define SYN_ULTRA_XFER_NS_9    69
#define SYN_ULTRA_XFER_NS_10   75
#define SYN_ULTRA_XFER_NS_11   82
#define SYN_ULTRA_XFER_NS_12   88
#define SYN_ULTRA_XFER_NS_13   94
#define SYN_ULTRA_XFER_NS_14  100
#define SYN_ULTRA_XFER_NS_15  107
typedef struct ext_msg {
uchar               msg_type;
uchar               msg_len;
uchar               msg_req;
union {
struct {
uchar               sdtr_xfer_period;
uchar               sdtr_req_ack_offset;
} sdtr;
struct {
uchar               wdtr_width;
} wdtr;
struct {
uchar               mdp_b3;
uchar               mdp_b2;
uchar               mdp_b1;
uchar               mdp_b0;
} mdp;
} u_ext_msg;
uchar               res;
} EXT_MSG;
#define xfer_period     u_ext_msg.sdtr.sdtr_xfer_period
#define req_ack_offset  u_ext_msg.sdtr.sdtr_req_ack_offset
#define wdtr_width      u_ext_msg.wdtr.wdtr_width
#define mdp_b3          u_ext_msg.mdp_b3
#define mdp_b2          u_ext_msg.mdp_b2
#define mdp_b1          u_ext_msg.mdp_b1
#define mdp_b0          u_ext_msg.mdp_b0
typedef struct asc_dvc_cfg {
ASC_SCSI_BIT_ID_TYPE can_tagged_qng;
ASC_SCSI_BIT_ID_TYPE cmd_qng_enabled;
ASC_SCSI_BIT_ID_TYPE disc_enable;
ASC_SCSI_BIT_ID_TYPE sdtr_enable;
uchar               chip_scsi_id:4;
uchar               isa_dma_speed:4;
uchar               isa_dma_channel;
uchar               chip_version;
ushort              pci_device_id;
ushort              lib_serial_no;
ushort              lib_version;
ushort              mcode_date;
ushort              mcode_version;
uchar               max_tag_qng[ASC_MAX_TID + 1];
uchar               *overrun_buf;
uchar               sdtr_period_offset[ASC_MAX_TID + 1];
ushort              pci_slot_info;
uchar               adapter_info[6];
} ASC_DVC_CFG;
#define ASC_DEF_DVC_CNTL       0xFFFF
#define ASC_DEF_CHIP_SCSI_ID   7
#define ASC_DEF_ISA_DMA_SPEED  4
#define ASC_INIT_STATE_NULL          0x0000
#define ASC_INIT_STATE_BEG_GET_CFG   0x0001
#define ASC_INIT_STATE_END_GET_CFG   0x0002
#define ASC_INIT_STATE_BEG_SET_CFG   0x0004
#define ASC_INIT_STATE_END_SET_CFG   0x0008
#define ASC_INIT_STATE_BEG_LOAD_MC   0x0010
#define ASC_INIT_STATE_END_LOAD_MC   0x0020
#define ASC_INIT_STATE_BEG_INQUIRY   0x0040
#define ASC_INIT_STATE_END_INQUIRY   0x0080
#define ASC_INIT_RESET_SCSI_DONE     0x0100
#define ASC_INIT_STATE_WITHOUT_EEP   0x8000
#define ASC_PCI_DEVICE_ID_REV_A      0x1100
#define ASC_PCI_DEVICE_ID_REV_B      0x1200
#define ASC_BUG_FIX_IF_NOT_DWB       0x0001
#define ASC_BUG_FIX_ASYN_USE_SYN     0x0002
#define ASYN_SDTR_DATA_FIX_PCI_REV_AB 0x41
#define ASC_MIN_TAGGED_CMD  7
#define ASC_MAX_SCSI_RESET_WAIT      30
typedef struct asc_dvc_var {
PortAddr            iop_base;
ushort              err_code;
ushort              dvc_cntl;
ushort              bug_fix_cntl;
ushort              bus_type;
Ptr2Func            isr_callback;
Ptr2Func            exe_callback;
ASC_SCSI_BIT_ID_TYPE init_sdtr;
ASC_SCSI_BIT_ID_TYPE sdtr_done;
ASC_SCSI_BIT_ID_TYPE use_tagged_qng;
ASC_SCSI_BIT_ID_TYPE unit_not_ready;
ASC_SCSI_BIT_ID_TYPE queue_full_or_busy;
ASC_SCSI_BIT_ID_TYPE start_motor;
uchar               scsi_reset_wait;
uchar               chip_no;
char                is_in_int;
uchar               max_total_qng;
uchar               cur_total_qng;
uchar               in_critical_cnt;
uchar               irq_no;
uchar               last_q_shortage;
ushort              init_state;
uchar               cur_dvc_qng[ASC_MAX_TID + 1];
uchar               max_dvc_qng[ASC_MAX_TID + 1];
ASC_SCSI_Q  *scsiq_busy_head[ASC_MAX_TID + 1];
ASC_SCSI_Q  *scsiq_busy_tail[ASC_MAX_TID + 1];
uchar               sdtr_period_tbl[ASC_MAX_SYN_XFER_NO];
ASC_DVC_CFG *cfg;
Ptr2Func            saved_ptr2func;
ASC_SCSI_BIT_ID_TYPE pci_fix_asyn_xfer_always;
char                redo_scam;
ushort              res2;
uchar               dos_int13_table[ASC_MAX_TID + 1];
ulong               max_dma_count;
ASC_SCSI_BIT_ID_TYPE no_scam;
ASC_SCSI_BIT_ID_TYPE pci_fix_asyn_xfer;
uchar               max_sdtr_index;
uchar               host_init_sdtr_index;
ulong               drv_ptr;
ulong               uc_break;
ulong               res7;
ulong               res8;
} ASC_DVC_VAR;
typedef int (* ASC_ISR_CALLBACK) (ASC_DVC_VAR asc_ptr_type *, ASC_QDONE_INFO *);
typedef int (* ASC_EXE_CALLBACK) (ASC_DVC_VAR asc_ptr_type *, ASC_SCSI_Q *);
typedef struct asc_dvc_inq_info {
uchar               type[ASC_MAX_TID + 1][ASC_MAX_LUN + 1];
} ASC_DVC_INQ_INFO;
typedef struct asc_cap_info {
ulong               lba;
ulong               blk_size;
} ASC_CAP_INFO;
typedef struct asc_cap_info_array {
ASC_CAP_INFO        cap_info[ASC_MAX_TID + 1][ASC_MAX_LUN + 1];
} ASC_CAP_INFO_ARRAY;
#define ASC_MCNTL_NO_SEL_TIMEOUT  (ushort)0x0001
#define ASC_MCNTL_NULL_TARGET     (ushort)0x0002
#define ASC_CNTL_INITIATOR         (ushort)0x0001
#define ASC_CNTL_BIOS_GT_1GB       (ushort)0x0002
#define ASC_CNTL_BIOS_GT_2_DISK    (ushort)0x0004
#define ASC_CNTL_BIOS_REMOVABLE    (ushort)0x0008
#define ASC_CNTL_NO_SCAM           (ushort)0x0010
#define ASC_CNTL_INT_MULTI_Q       (ushort)0x0080
#define ASC_CNTL_NO_LUN_SUPPORT    (ushort)0x0040
#define ASC_CNTL_NO_VERIFY_COPY    (ushort)0x0100
#define ASC_CNTL_RESET_SCSI        (ushort)0x0200
#define ASC_CNTL_INIT_INQUIRY      (ushort)0x0400
#define ASC_CNTL_INIT_VERBOSE      (ushort)0x0800
#define ASC_CNTL_SCSI_PARITY       (ushort)0x1000
#define ASC_CNTL_BURST_MODE        (ushort)0x2000
#define ASC_CNTL_SDTR_ENABLE_ULTRA (ushort)0x4000
#define ASC_EEP_DVC_CFG_BEG_VL    2
#define ASC_EEP_MAX_DVC_ADDR_VL   15
#define ASC_EEP_DVC_CFG_BEG      32
#define ASC_EEP_MAX_DVC_ADDR     45
#define ASC_EEP_DEFINED_WORDS    10
#define ASC_EEP_MAX_ADDR         63
#define ASC_EEP_RES_WORDS         0
#define ASC_EEP_MAX_RETRY        20
#define ASC_MAX_INIT_BUSY_RETRY   8
#define ASC_EEP_ISA_PNP_WSIZE    16
typedef struct asceep_config {
ushort              cfg_lsw;
ushort              cfg_msw;
uchar               init_sdtr;
uchar               disc_enable;
uchar               use_cmd_qng;
uchar               start_motor;
uchar               max_total_qng;
uchar               max_tag_qng;
uchar               bios_scan;
uchar               power_up_wait;
uchar               no_scam;
uchar               chip_scsi_id:4;
uchar               isa_dma_speed:4;
uchar               dos_int13_table[ASC_MAX_TID + 1];
uchar               adapter_info[6];
ushort              cntl;
ushort              chksum;
} ASCEEP_CONFIG;
#define ASC_PCI_CFG_LSW_SCSI_PARITY  0x0800
#define ASC_PCI_CFG_LSW_BURST_MODE   0x0080
#define ASC_PCI_CFG_LSW_INTR_ABLE    0x0020
#define ASC_EEP_CMD_READ          0x80
#define ASC_EEP_CMD_WRITE         0x40
#define ASC_EEP_CMD_WRITE_ABLE    0x30
#define ASC_EEP_CMD_WRITE_DISABLE 0x00
#define ASC_OVERRUN_BSIZE  0x00000048UL
#define ASC_CTRL_BREAK_ONCE        0x0001
#define ASC_CTRL_BREAK_STAY_IDLE   0x0002
#define ASCV_MSGOUT_BEG         0x0000
#define ASCV_MSGOUT_SDTR_PERIOD (ASCV_MSGOUT_BEG+3)
#define ASCV_MSGOUT_SDTR_OFFSET (ASCV_MSGOUT_BEG+4)
#define ASCV_BREAK_SAVED_CODE   (ushort)0x0006
#define ASCV_MSGIN_BEG          (ASCV_MSGOUT_BEG+8)
#define ASCV_MSGIN_SDTR_PERIOD  (ASCV_MSGIN_BEG+3)
#define ASCV_MSGIN_SDTR_OFFSET  (ASCV_MSGIN_BEG+4)
#define ASCV_SDTR_DATA_BEG      (ASCV_MSGIN_BEG+8)
#define ASCV_SDTR_DONE_BEG      (ASCV_SDTR_DATA_BEG+8)
#define ASCV_MAX_DVC_QNG_BEG    (ushort)0x0020
#define ASCV_BREAK_ADDR           (ushort)0x0028
#define ASCV_BREAK_NOTIFY_COUNT   (ushort)0x002A
#define ASCV_BREAK_CONTROL        (ushort)0x002C
#define ASCV_BREAK_HIT_COUNT      (ushort)0x002E
#define ASCV_ASCDVC_ERR_CODE_W  (ushort)0x0030
#define ASCV_MCODE_CHKSUM_W   (ushort)0x0032
#define ASCV_MCODE_SIZE_W     (ushort)0x0034
#define ASCV_STOP_CODE_B      (ushort)0x0036
#define ASCV_DVC_ERR_CODE_B   (ushort)0x0037
#define ASCV_OVERRUN_PADDR_D  (ushort)0x0038
#define ASCV_OVERRUN_BSIZE_D  (ushort)0x003C
#define ASCV_HALTCODE_W       (ushort)0x0040
#define ASCV_CHKSUM_W         (ushort)0x0042
#define ASCV_MC_DATE_W        (ushort)0x0044
#define ASCV_MC_VER_W         (ushort)0x0046
#define ASCV_NEXTRDY_B        (ushort)0x0048
#define ASCV_DONENEXT_B       (ushort)0x0049
#define ASCV_USE_TAGGED_QNG_B (ushort)0x004A
#define ASCV_SCSIBUSY_B       (ushort)0x004B
#define ASCV_Q_DONE_IN_PROGRESS_B  (ushort)0x004C
#define ASCV_CURCDB_B         (ushort)0x004D
#define ASCV_RCLUN_B          (ushort)0x004E
#define ASCV_BUSY_QHEAD_B     (ushort)0x004F
#define ASCV_DISC1_QHEAD_B    (ushort)0x0050
#define ASCV_DISC_ENABLE_B    (ushort)0x0052
#define ASCV_CAN_TAGGED_QNG_B (ushort)0x0053
#define ASCV_HOSTSCSI_ID_B    (ushort)0x0055
#define ASCV_MCODE_CNTL_B     (ushort)0x0056
#define ASCV_NULL_TARGET_B    (ushort)0x0057
#define ASCV_FREE_Q_HEAD_W    (ushort)0x0058
#define ASCV_DONE_Q_TAIL_W    (ushort)0x005A
#define ASCV_FREE_Q_HEAD_B    (ushort)(ASCV_FREE_Q_HEAD_W+1)
#define ASCV_DONE_Q_TAIL_B    (ushort)(ASCV_DONE_Q_TAIL_W+1)
#define ASCV_HOST_FLAG_B      (ushort)0x005D
#define ASCV_TOTAL_READY_Q_B  (ushort)0x0064
#define ASCV_VER_SERIAL_B     (ushort)0x0065
#define ASCV_HALTCODE_SAVED_W (ushort)0x0066
#define ASCV_WTM_FLAG_B       (ushort)0x0068
#define ASCV_RISC_FLAG_B      (ushort)0x006A
#define ASCV_REQ_SG_LIST_QP   (ushort)0x006B
#define ASC_HOST_FLAG_IN_ISR        0x01
#define ASC_HOST_FLAG_ACK_INT       0x02
#define ASC_RISC_FLAG_GEN_INT      0x01
#define ASC_RISC_FLAG_REQ_SG_LIST  0x02
#define IOP_CTRL         (0x0F)
#define IOP_STATUS       (0x0E)
#define IOP_INT_ACK      IOP_STATUS
#define IOP_REG_IFC      (0x0D)
#define IOP_SYN_OFFSET    (0x0B)
#define IOP_EXTRA_CONTROL (0x0D)
#define IOP_REG_PC        (0x0C)
#define IOP_RAM_ADDR      (0x0A)
#define IOP_RAM_DATA      (0x08)
#define IOP_EEP_DATA      (0x06)
#define IOP_EEP_CMD       (0x07)
#define IOP_VERSION       (0x03)
#define IOP_CONFIG_HIGH   (0x04)
#define IOP_CONFIG_LOW    (0x02)
#define IOP_SIG_BYTE      (0x01)
#define IOP_SIG_WORD      (0x00)
#define IOP_REG_DC1      (0x0E)
#define IOP_REG_DC0      (0x0C)
#define IOP_REG_SB       (0x0B)
#define IOP_REG_DA1      (0x0A)
#define IOP_REG_DA0      (0x08)
#define IOP_REG_SC       (0x09)
#define IOP_DMA_SPEED    (0x07)
#define IOP_REG_FLAG     (0x07)
#define IOP_FIFO_H       (0x06)
#define IOP_FIFO_L       (0x04)
#define IOP_REG_ID       (0x05)
#define IOP_REG_QP       (0x03)
#define IOP_REG_IH       (0x02)
#define IOP_REG_IX       (0x01)
#define IOP_REG_AX       (0x00)
#define IFC_REG_LOCK      (0x00)
#define IFC_REG_UNLOCK    (0x09)
#define IFC_WR_EN_FILTER  (0x10)
#define IFC_RD_NO_EEPROM  (0x10)
#define IFC_SLEW_RATE     (0x20)
#define IFC_ACT_NEG       (0x40)
#define IFC_INP_FILTER    (0x80)
#define IFC_INIT_DEFAULT  (IFC_ACT_NEG | IFC_REG_UNLOCK)
#define SC_SEL   (uchar)(0x80)
#define SC_BSY   (uchar)(0x40)
#define SC_ACK   (uchar)(0x20)
#define SC_REQ   (uchar)(0x10)
#define SC_ATN   (uchar)(0x08)
#define SC_IO    (uchar)(0x04)
#define SC_CD    (uchar)(0x02)
#define SC_MSG   (uchar)(0x01)
#define SEC_SCSI_CTL         (uchar)(0x80)
#define SEC_ACTIVE_NEGATE    (uchar)(0x40)
#define SEC_SLEW_RATE        (uchar)(0x20)
#define SEC_ENABLE_FILTER    (uchar)(0x10)
#define ASC_HALT_EXTMSG_IN     (ushort)0x8000
#define ASC_HALT_CHK_CONDITION (ushort)0x8100
#define ASC_HALT_SS_QUEUE_FULL (ushort)0x8200
#define ASC_HALT_DISABLE_ASYN_USE_SYN_FIX  (ushort)0x8300
#define ASC_HALT_ENABLE_ASYN_USE_SYN_FIX   (ushort)0x8400
#define ASC_HALT_SDTR_REJECTED (ushort)0x4000
#define ASC_MAX_QNO        0xF8
#define ASC_DATA_SEC_BEG   (ushort)0x0080
#define ASC_DATA_SEC_END   (ushort)0x0080
#define ASC_CODE_SEC_BEG   (ushort)0x0080
#define ASC_CODE_SEC_END   (ushort)0x0080
#define ASC_QADR_BEG       (0x4000)
#define ASC_QADR_USED      (ushort)(ASC_MAX_QNO * 64)
#define ASC_QADR_END       (ushort)0x7FFF
#define ASC_QLAST_ADR      (ushort)0x7FC0
#define ASC_QBLK_SIZE      0x40
#define ASC_BIOS_DATA_QBEG 0xF8
#define ASC_MIN_ACTIVE_QNO 0x01
#define ASC_QLINK_END      0xFF
#define ASC_EEPROM_WORDS   0x10
#define ASC_MAX_MGS_LEN    0x10
#define ASC_BIOS_ADDR_DEF  0xDC00
#define ASC_BIOS_SIZE      0x3800
#define ASC_BIOS_RAM_OFF   0x3800
#define ASC_BIOS_RAM_SIZE  0x800
#define ASC_BIOS_MIN_ADDR  0xC000
#define ASC_BIOS_MAX_ADDR  0xEC00
#define ASC_BIOS_BANK_SIZE 0x0400
#define ASC_MCODE_START_ADDR  0x0080
#define ASC_CFG0_HOST_INT_ON    0x0020
#define ASC_CFG0_BIOS_ON        0x0040
#define ASC_CFG0_VERA_BURST_ON  0x0080
#define ASC_CFG0_SCSI_PARITY_ON 0x0800
#define ASC_CFG1_SCSI_TARGET_ON 0x0080
#define ASC_CFG1_LRAM_8BITS_ON  0x0800
#define ASC_CFG_MSW_CLR_MASK    0x3080
#define CSW_TEST1             (ASC_CS_TYPE)0x8000
#define CSW_AUTO_CONFIG       (ASC_CS_TYPE)0x4000
#define CSW_RESERVED1         (ASC_CS_TYPE)0x2000
#define CSW_IRQ_WRITTEN       (ASC_CS_TYPE)0x1000
#define CSW_33MHZ_SELECTED    (ASC_CS_TYPE)0x0800
#define CSW_TEST2             (ASC_CS_TYPE)0x0400
#define CSW_TEST3             (ASC_CS_TYPE)0x0200
#define CSW_RESERVED2         (ASC_CS_TYPE)0x0100
#define CSW_DMA_DONE          (ASC_CS_TYPE)0x0080
#define CSW_FIFO_RDY          (ASC_CS_TYPE)0x0040
#define CSW_EEP_READ_DONE     (ASC_CS_TYPE)0x0020
#define CSW_HALTED            (ASC_CS_TYPE)0x0010
#define CSW_SCSI_RESET_ACTIVE (ASC_CS_TYPE)0x0008
#define CSW_PARITY_ERR        (ASC_CS_TYPE)0x0004
#define CSW_SCSI_RESET_LATCH  (ASC_CS_TYPE)0x0002
#define CSW_INT_PENDING       (ASC_CS_TYPE)0x0001
#define CIW_CLR_SCSI_RESET_INT (ASC_CS_TYPE)0x1000
#define CIW_INT_ACK      (ASC_CS_TYPE)0x0100
#define CIW_TEST1        (ASC_CS_TYPE)0x0200
#define CIW_TEST2        (ASC_CS_TYPE)0x0400
#define CIW_SEL_33MHZ    (ASC_CS_TYPE)0x0800
#define CIW_IRQ_ACT      (ASC_CS_TYPE)0x1000
#define CC_CHIP_RESET   (uchar)0x80
#define CC_SCSI_RESET   (uchar)0x40
#define CC_HALT         (uchar)0x20
#define CC_SINGLE_STEP  (uchar)0x10
#define CC_DMA_ABLE     (uchar)0x08
#define CC_TEST         (uchar)0x04
#define CC_BANK_ONE     (uchar)0x02
#define CC_DIAG         (uchar)0x01
#define ASC_1000_ID0W      0x04C1
#define ASC_1000_ID0W_FIX  0x00C1
#define ASC_1000_ID1B      0x25
#define ASC_EISA_BIG_IOP_GAP   (0x1C30-0x0C50)
#define ASC_EISA_SMALL_IOP_GAP (0x0020)
#define ASC_EISA_MIN_IOP_ADDR  (0x0C30)
#define ASC_EISA_MAX_IOP_ADDR  (0xFC50)
#define ASC_EISA_REV_IOP_MASK  (0x0C83)
#define ASC_EISA_PID_IOP_MASK  (0x0C80)
#define ASC_EISA_CFG_IOP_MASK  (0x0C86)
#define ASC_GET_EISA_SLOT(iop)  (PortAddr)((iop) & 0xF000)
#define ASC_EISA_ID_740    0x01745004UL
#define ASC_EISA_ID_750    0x01755004UL
#define INS_HALTINT        (ushort)0x6281
#define INS_HALT           (ushort)0x6280
#define INS_SINT           (ushort)0x6200
#define INS_RFLAG_WTM      (ushort)0x7380
#define ASC_MC_SAVE_CODE_WSIZE  0x500
#define ASC_MC_SAVE_DATA_WSIZE  0x40
typedef struct asc_mc_saved {
ushort              data[ASC_MC_SAVE_DATA_WSIZE];
ushort              code[ASC_MC_SAVE_CODE_WSIZE];
} ASC_MC_SAVED;
#define AscGetQDoneInProgress(port)         AscReadLramByte((port), ASCV_Q_DONE_IN_PROGRESS_B)
#define AscPutQDoneInProgress(port, val)    AscWriteLramByte((port), ASCV_Q_DONE_IN_PROGRESS_B, val)
#define AscGetVarFreeQHead(port)            AscReadLramWord((port), ASCV_FREE_Q_HEAD_W)
#define AscGetVarDoneQTail(port)            AscReadLramWord((port), ASCV_DONE_Q_TAIL_W)
#define AscPutVarFreeQHead(port, val)       AscWriteLramWord((port), ASCV_FREE_Q_HEAD_W, val)
#define AscPutVarDoneQTail(port, val)       AscWriteLramWord((port), ASCV_DONE_Q_TAIL_W, val)
#define AscGetRiscVarFreeQHead(port)        AscReadLramByte((port), ASCV_NEXTRDY_B)
#define AscGetRiscVarDoneQTail(port)        AscReadLramByte((port), ASCV_DONENEXT_B)
#define AscPutRiscVarFreeQHead(port, val)   AscWriteLramByte((port), ASCV_NEXTRDY_B, val)
#define AscPutRiscVarDoneQTail(port, val)   AscWriteLramByte((port), ASCV_DONENEXT_B, val)
#define AscPutMCodeSDTRDoneAtID(port, id, data)  AscWriteLramByte((port), (ushort)((ushort)ASCV_SDTR_DONE_BEG+(ushort)id), (data)) ;
#define AscGetMCodeSDTRDoneAtID(port, id)        AscReadLramByte((port), (ushort)((ushort)ASCV_SDTR_DONE_BEG+(ushort)id)) ;
#define AscPutMCodeInitSDTRAtID(port, id, data)  AscWriteLramByte((port), (ushort)((ushort)ASCV_SDTR_DATA_BEG+(ushort)id), data) ;
#define AscGetMCodeInitSDTRAtID(port, id)        AscReadLramByte((port), (ushort)((ushort)ASCV_SDTR_DATA_BEG+(ushort)id)) ;
#define AscSynIndexToPeriod(index)        (uchar)(asc_dvc->sdtr_period_tbl[ (index) ])
#define AscGetChipSignatureByte(port)     (uchar)inp((port)+IOP_SIG_BYTE)
#define AscGetChipSignatureWord(port)     (ushort)inpw((port)+IOP_SIG_WORD)
#define AscGetChipVerNo(port)             (uchar)inp((port)+IOP_VERSION)
#define AscGetChipCfgLsw(port)            (ushort)inpw((port)+IOP_CONFIG_LOW)
#define AscGetChipCfgMsw(port)            (ushort)inpw((port)+IOP_CONFIG_HIGH)
#define AscSetChipCfgLsw(port, data)      outpw((port)+IOP_CONFIG_LOW, data)
#define AscSetChipCfgMsw(port, data)      outpw((port)+IOP_CONFIG_HIGH, data)
#define AscGetChipEEPCmd(port)            (uchar)inp((port)+IOP_EEP_CMD)
#define AscSetChipEEPCmd(port, data)      outp((port)+IOP_EEP_CMD, data)
#define AscGetChipEEPData(port)           (ushort)inpw((port)+IOP_EEP_DATA)
#define AscSetChipEEPData(port, data)     outpw((port)+IOP_EEP_DATA, data)
#define AscGetChipLramAddr(port)          (ushort)inpw((PortAddr)((port)+IOP_RAM_ADDR))
#define AscSetChipLramAddr(port, addr)    outpw((PortAddr)((port)+IOP_RAM_ADDR), addr)
#define AscGetChipLramData(port)          (ushort)inpw((port)+IOP_RAM_DATA)
#define AscSetChipLramData(port, data)    outpw((port)+IOP_RAM_DATA, data)
#define AscGetChipLramDataNoSwap(port)         (ushort)inpw_noswap((port)+IOP_RAM_DATA)
#define AscSetChipLramDataNoSwap(port, data)   outpw_noswap((port)+IOP_RAM_DATA, data)
#define AscGetChipIFC(port)               (uchar)inp((port)+IOP_REG_IFC)
#define AscSetChipIFC(port, data)          outp((port)+IOP_REG_IFC, data)
#define AscGetChipStatus(port)            (ASC_CS_TYPE)inpw((port)+IOP_STATUS)
#define AscSetChipStatus(port, cs_val)    outpw((port)+IOP_STATUS, cs_val)
#define AscGetChipControl(port)           (uchar)inp((port)+IOP_CTRL)
#define AscSetChipControl(port, cc_val)   outp((port)+IOP_CTRL, cc_val)
#define AscGetChipSyn(port)               (uchar)inp((port)+IOP_SYN_OFFSET)
#define AscSetChipSyn(port, data)         outp((port)+IOP_SYN_OFFSET, data)
#define AscSetPCAddr(port, data)          outpw((port)+IOP_REG_PC, data)
#define AscGetPCAddr(port)                (ushort)inpw((port)+IOP_REG_PC)
#define AscIsIntPending(port)             (AscGetChipStatus(port) & (CSW_INT_PENDING | CSW_SCSI_RESET_LATCH))
#define AscGetChipScsiID(port)            ((AscGetChipCfgLsw(port) >> 8) & ASC_MAX_TID)
#define AscGetExtraControl(port)          (uchar)inp((port)+IOP_EXTRA_CONTROL)
#define AscSetExtraControl(port, data)    outp((port)+IOP_EXTRA_CONTROL, data)
#define AscReadChipAX(port)               (ushort)inpw((port)+IOP_REG_AX)
#define AscWriteChipAX(port, data)        outpw((port)+IOP_REG_AX, data)
#define AscReadChipIX(port)               (uchar)inp((port)+IOP_REG_IX)
#define AscWriteChipIX(port, data)        outp((port)+IOP_REG_IX, data)
#define AscReadChipIH(port)               (ushort)inpw((port)+IOP_REG_IH)
#define AscWriteChipIH(port, data)        outpw((port)+IOP_REG_IH, data)
#define AscReadChipQP(port)               (uchar)inp((port)+IOP_REG_QP)
#define AscWriteChipQP(port, data)        outp((port)+IOP_REG_QP, data)
#define AscReadChipFIFO_L(port)           (ushort)inpw((port)+IOP_REG_FIFO_L)
#define AscWriteChipFIFO_L(port, data)    outpw((port)+IOP_REG_FIFO_L, data)
#define AscReadChipFIFO_H(port)           (ushort)inpw((port)+IOP_REG_FIFO_H)
#define AscWriteChipFIFO_H(port, data)    outpw((port)+IOP_REG_FIFO_H, data)
#define AscReadChipDmaSpeed(port)         (uchar)inp((port)+IOP_DMA_SPEED)
#define AscWriteChipDmaSpeed(port, data)  outp((port)+IOP_DMA_SPEED, data)
#define AscReadChipDA0(port)              (ushort)inpw((port)+IOP_REG_DA0)
#define AscWriteChipDA0(port)             outpw((port)+IOP_REG_DA0, data)
#define AscReadChipDA1(port)              (ushort)inpw((port)+IOP_REG_DA1)
#define AscWriteChipDA1(port)             outpw((port)+IOP_REG_DA1, data)
#define AscReadChipDC0(port)              (ushort)inpw((port)+IOP_REG_DC0)
#define AscWriteChipDC0(port)             outpw((port)+IOP_REG_DC0, data)
#define AscReadChipDC1(port)              (ushort)inpw((port)+IOP_REG_DC1)
#define AscWriteChipDC1(port)             outpw((port)+IOP_REG_DC1, data)
#define AscReadChipDvcID(port)            (uchar)inp((port)+IOP_REG_ID)
#define AscWriteChipDvcID(port, data)     outp((port)+IOP_REG_ID, data)
STATIC int       AscWriteEEPCmdReg(PortAddr iop_base, uchar cmd_reg);
STATIC int       AscWriteEEPDataReg(PortAddr iop_base, ushort data_reg);
STATIC void      AscWaitEEPRead(void);
STATIC void      AscWaitEEPWrite(void);
STATIC ushort    AscReadEEPWord(PortAddr, uchar);
STATIC ushort    AscWriteEEPWord(PortAddr, uchar, ushort);
STATIC ushort    AscGetEEPConfig(PortAddr, ASCEEP_CONFIG *, ushort);
STATIC int       AscSetEEPConfigOnce(PortAddr, ASCEEP_CONFIG *, ushort);
STATIC int       AscSetEEPConfig(PortAddr, ASCEEP_CONFIG *, ushort);
STATIC int       AscStartChip(PortAddr);
STATIC int       AscStopChip(PortAddr);
STATIC void      AscSetChipIH(PortAddr, ushort);
STATIC int       AscIsChipHalted(PortAddr);
STATIC void      AscAckInterrupt(PortAddr);
STATIC void      AscDisableInterrupt(PortAddr);
STATIC void      AscEnableInterrupt(PortAddr);
STATIC void      AscSetBank(PortAddr, uchar);
STATIC int       AscResetChipAndScsiBus(ASC_DVC_VAR *);
STATIC ushort    AscGetIsaDmaChannel(PortAddr);
STATIC ushort    AscSetIsaDmaChannel(PortAddr, ushort);
STATIC uchar     AscSetIsaDmaSpeed(PortAddr, uchar);
STATIC uchar     AscGetIsaDmaSpeed(PortAddr);
STATIC uchar     AscReadLramByte(PortAddr, ushort);
STATIC ushort    AscReadLramWord(PortAddr, ushort);
STATIC ulong     AscReadLramDWord(PortAddr, ushort);
STATIC void      AscWriteLramWord(PortAddr, ushort, ushort);
STATIC void      AscWriteLramDWord(PortAddr, ushort, ulong);
STATIC void      AscWriteLramByte(PortAddr, ushort, uchar);
STATIC ulong     AscMemSumLramWord(PortAddr, ushort, rint);
STATIC void      AscMemWordSetLram(PortAddr, ushort, ushort, rint);
STATIC void      AscMemWordCopyToLram(PortAddr, ushort, ushort *, int);
STATIC void      AscMemDWordCopyToLram(PortAddr, ushort, ulong *, int);
STATIC void      AscMemWordCopyFromLram(PortAddr, ushort, ushort *, int);
STATIC ushort    AscInitAscDvcVar(ASC_DVC_VAR asc_ptr_type *);
STATIC ushort    AscInitFromEEP(ASC_DVC_VAR asc_ptr_type *);
STATIC ushort    AscInitFromAscDvcVar(ASC_DVC_VAR asc_ptr_type *);
STATIC ushort    AscInitMicroCodeVar(ASC_DVC_VAR asc_ptr_type * asc_dvc);
STATIC int       AscTestExternalLram(ASC_DVC_VAR asc_ptr_type *);
STATIC uchar     AscMsgOutSDTR(ASC_DVC_VAR asc_ptr_type *, uchar, uchar);
STATIC uchar     AscCalSDTRData(ASC_DVC_VAR asc_ptr_type *, uchar, uchar);
STATIC void      AscSetChipSDTR(PortAddr, uchar, uchar);
STATIC uchar     AscGetSynPeriodIndex(ASC_DVC_VAR asc_ptr_type *, ruchar);
STATIC uchar     AscAllocFreeQueue(PortAddr, uchar);
STATIC uchar     AscAllocMultipleFreeQueue(PortAddr, uchar, uchar);
STATIC int       AscRiscHaltedAbortSRB(ASC_DVC_VAR asc_ptr_type *, ulong);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC int       AscRiscHaltedAbortTIX(ASC_DVC_VAR asc_ptr_type *, uchar);
#endif
STATIC int       AscHostReqRiscHalt(PortAddr);
STATIC int       AscStopQueueExe(PortAddr);
STATIC int       AscStartQueueExe(PortAddr);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC int       AscCleanUpDiscQueue(PortAddr);
#endif
STATIC int       AscCleanUpBusyQueue(PortAddr);
STATIC int       AscWaitTixISRDone(ASC_DVC_VAR asc_ptr_type *, uchar);
STATIC int       AscWaitISRDone(ASC_DVC_VAR asc_ptr_type *);
STATIC ulong     AscGetOnePhyAddr(ASC_DVC_VAR asc_ptr_type *, uchar *,
ulong);
STATIC int       AscSendScsiQueue(ASC_DVC_VAR asc_ptr_type * asc_dvc,
ASC_SCSI_Q * scsiq,
uchar n_q_required);
STATIC int       AscPutReadyQueue(ASC_DVC_VAR asc_ptr_type *,
ASC_SCSI_Q *, uchar);
STATIC int       AscPutReadySgListQueue(ASC_DVC_VAR asc_ptr_type *,
ASC_SCSI_Q *, uchar);
STATIC int       AscSetChipSynRegAtID(PortAddr, uchar, uchar);
STATIC int       AscSetRunChipSynRegAtID(PortAddr, uchar, uchar);
STATIC ushort    AscInitLram(ASC_DVC_VAR asc_ptr_type *);
STATIC int       AscReInitLram(ASC_DVC_VAR asc_ptr_type *);
STATIC ushort    AscInitQLinkVar(ASC_DVC_VAR asc_ptr_type *);
STATIC int       AscSetLibErrorCode(ASC_DVC_VAR asc_ptr_type *, ushort);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC int       _AscWaitQDone(PortAddr, ASC_SCSI_Q *);
#endif
STATIC int       AscIsrChipHalted(ASC_DVC_VAR asc_ptr_type *);
STATIC uchar     _AscCopyLramScsiDoneQ(PortAddr, ushort,
ASC_QDONE_INFO *, ulong);
STATIC int       AscIsrQDone(ASC_DVC_VAR asc_ptr_type *);
STATIC int       AscCompareString(uchar *, uchar *, int);
STATIC ushort    AscGetEisaChipCfg(PortAddr);
STATIC ulong     AscGetEisaProductID(PortAddr);
STATIC PortAddr  AscSearchIOPortAddrEISA(PortAddr);
STATIC uchar     AscGetChipScsiCtrl(PortAddr);
STATIC uchar     AscSetChipScsiID(PortAddr, uchar);
STATIC uchar     AscGetChipVersion(PortAddr, ushort);
STATIC ushort    AscGetChipBusType(PortAddr);
STATIC ulong     AscLoadMicroCode(PortAddr, ushort, ushort *, ushort);
STATIC int       AscFindSignature(PortAddr);
STATIC PortAddr  AscSearchIOPortAddr11(PortAddr);
STATIC void      AscToggleIRQAct(PortAddr);
STATIC void      AscSetISAPNPWaitForKey(void);
STATIC uchar     AscGetChipIRQ(PortAddr, ushort);
STATIC uchar     AscSetChipIRQ(PortAddr, uchar, ushort);
STATIC ushort    AscGetChipBiosAddress(PortAddr, ushort);
STATIC long      DvcEnterCritical(void);
STATIC void      DvcLeaveCritical(long);
STATIC void      DvcInPortWords(PortAddr, ushort *, int);
STATIC void      DvcOutPortWords(PortAddr, ushort *, int);
STATIC void      DvcOutPortDWords(PortAddr, ulong *, int);
STATIC uchar     DvcReadPCIConfigByte(ASC_DVC_VAR asc_ptr_type *, ushort);
STATIC void      DvcWritePCIConfigByte(ASC_DVC_VAR asc_ptr_type *,
ushort, uchar);
STATIC ushort      AscGetChipBiosAddress(PortAddr, ushort);
STATIC void      DvcSleepMilliSecond(ulong);
STATIC void      DvcDelayNanoSecond(ASC_DVC_VAR asc_ptr_type *, ulong);
STATIC ulong     DvcGetSGList(ASC_DVC_VAR asc_ptr_type *, uchar *,
ulong, ASC_SG_HEAD *);
STATIC void      DvcPutScsiQ(PortAddr, ushort, ushort *, int);
STATIC void      DvcGetQinfo(PortAddr, ushort, ushort *, int);
STATIC PortAddr  AscSearchIOPortAddr(PortAddr, ushort);
STATIC ushort    AscInitGetConfig(ASC_DVC_VAR asc_ptr_type *);
STATIC ushort    AscInitSetConfig(ASC_DVC_VAR asc_ptr_type *);
STATIC ushort    AscInitAsc1000Driver(ASC_DVC_VAR asc_ptr_type *);
STATIC void      AscAsyncFix(ASC_DVC_VAR asc_ptr_type *, uchar,
ASC_SCSI_INQUIRY *);
STATIC int       AscTagQueuingSafe(ASC_SCSI_INQUIRY *);
STATIC void      AscInquiryHandling(ASC_DVC_VAR asc_ptr_type *,
uchar, ASC_SCSI_INQUIRY *);
STATIC int       AscExeScsiQueue(ASC_DVC_VAR asc_ptr_type *, ASC_SCSI_Q *);
STATIC int       AscISR(ASC_DVC_VAR asc_ptr_type *);
STATIC uint      AscGetNumOfFreeQueue(ASC_DVC_VAR asc_ptr_type *, uchar,
uchar);
STATIC int       AscSgListToQueue(int);
STATIC int       AscAbortSRB(ASC_DVC_VAR asc_ptr_type *, ulong);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC int       AscResetDevice(ASC_DVC_VAR asc_ptr_type *, uchar);
#endif
STATIC int       AscResetSB(ASC_DVC_VAR asc_ptr_type *);
STATIC void      AscEnableIsaDma(uchar);
STATIC ulong     AscGetMaxDmaCount(ushort);
#define ADV_LIB_VERSION_MAJOR  3
#define ADV_LIB_VERSION_MINOR  45
#define ADV_OS_LINUX
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
#define AdvPortAddr  unsigned short
#else
#define AdvPortAddr  unsigned long
#endif
#define ADV_MEM_READB(addr) readb(addr)
#define ADV_MEM_READW(addr) readw(addr)
#define ADV_MEM_WRITEB(addr, byte) writeb(byte, addr)
#define ADV_MEM_WRITEW(addr, word) writew(word, addr)
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,0)
#define ioremap vremap
#define iounmap vfree
#endif
#define ADV_TOT_SG_LIST         64
#define ADV_MAX_SG_LIST         64
#define ADV_NUM_SG_BLOCK \
((ADV_MAX_SG_LIST + (NO_OF_SG_PER_BLOCK - 1))/NO_OF_SG_PER_BLOCK)
#define ADV_SG_TOTAL_MEM_SIZE \
(sizeof(ADV_SG_BLOCK) *  ADV_NUM_SG_BLOCK)
#define ASC_PAGE_SIZE PAGE_SIZE
#define ADV_NUM_PAGE_CROSSING \
((ADV_SG_TOTAL_MEM_SIZE + (ASC_PAGE_SIZE - 1))/ASC_PAGE_SIZE)
#define ADV_ASSERT(a) ASC_ASSERT(a)
#define ADV_PCI_VENDOR_ID               0x10CD
#define ADV_PCI_DEVICE_ID_REV_A         0x2300
#define ASC_EEP_DVC_CFG_BEGIN           (0x00)
#define ASC_EEP_DVC_CFG_END             (0x15)
#define ASC_EEP_DVC_CTL_BEGIN           (0x16)
#define ASC_EEP_MAX_WORD_ADDR           (0x1E)
#define ASC_EEP_DELAY_MS                100
#define ADV_EEPROM_BIG_ENDIAN          0x8000
#define ADV_EEPROM_BIOS_ENABLE         0x4000
#define ADV_EEPROM_TERM_POL            0x2000
typedef struct adveep_config
{
ushort cfg_lsw;
ushort cfg_msw;
ushort disc_enable;
ushort wdtr_able;
ushort sdtr_able;
ushort start_motor;
ushort tagqng_able;
ushort bios_scan;
ushort scam_tolerant;
uchar  adapter_scsi_id;
uchar  bios_boot_delay;
uchar  scsi_reset_delay;
uchar  bios_id_lun;
uchar  termination;
uchar  reserved1;
ushort bios_ctrl;
ushort  ultra_able;
ushort  reserved2;
uchar   max_host_qng;
uchar   max_dvc_qng;
ushort  dvc_cntl;
ushort  bug_fix;
ushort  serial_number_word1;
ushort  serial_number_word2;
ushort  serial_number_word3;
ushort  check_sum;
uchar   oem_name[16];
ushort  dvc_err_code;
ushort  adv_err_code;
ushort  adv_err_addr;
ushort  saved_dvc_err_code;
ushort  saved_adv_err_code;
ushort  saved_adv_err_addr;
ushort  num_of_err;
} ADVEEP_CONFIG;
#define ASC_EEP_CMD_DONE             0x0200
#define ASC_EEP_CMD_DONE_ERR         0x0001
#define EEP_CFG_WORD_BIG_ENDIAN      0x8000
#define BIOS_CTRL_BIOS               0x0001
#define BIOS_CTRL_EXTENDED_XLAT      0x0002
#define BIOS_CTRL_GT_2_DISK          0x0004
#define BIOS_CTRL_BIOS_REMOVABLE     0x0008
#define BIOS_CTRL_BOOTABLE_CD        0x0010
#define BIOS_CTRL_MULTIPLE_LUN       0x0040
#define BIOS_CTRL_DISPLAY_MSG        0x0080
#define BIOS_CTRL_NO_SCAM            0x0100
#define BIOS_CTRL_RESET_SCSI_BUS     0x0200
#define BIOS_CTRL_INIT_VERBOSE       0x0800
#define BIOS_CTRL_SCSI_PARITY        0x1000
#define ADV_CONDOR_MEMSIZE   0x2000
#define ADV_CONDOR_IOLEN     0x40
#define IOPB_INTR_STATUS_REG    0x00
#define IOPB_CHIP_ID_1          0x01
#define IOPB_INTR_ENABLES       0x02
#define IOPB_CHIP_TYPE_REV      0x03
#define IOPB_RES_ADDR_4         0x04
#define IOPB_RES_ADDR_5         0x05
#define IOPB_RAM_DATA           0x06
#define IOPB_RES_ADDR_7         0x07
#define IOPB_FLAG_REG           0x08
#define IOPB_RES_ADDR_9         0x09
#define IOPB_RISC_CSR           0x0A
#define IOPB_RES_ADDR_B         0x0B
#define IOPB_RES_ADDR_C         0x0C
#define IOPB_RES_ADDR_D         0x0D
#define IOPB_RES_ADDR_E         0x0E
#define IOPB_RES_ADDR_F         0x0F
#define IOPB_MEM_CFG            0x10
#define IOPB_RES_ADDR_11        0x11
#define IOPB_RES_ADDR_12        0x12
#define IOPB_RES_ADDR_13        0x13
#define IOPB_FLASH_PAGE         0x14
#define IOPB_RES_ADDR_15        0x15
#define IOPB_RES_ADDR_16        0x16
#define IOPB_RES_ADDR_17        0x17
#define IOPB_FLASH_DATA         0x18
#define IOPB_RES_ADDR_19        0x19
#define IOPB_RES_ADDR_1A        0x1A
#define IOPB_RES_ADDR_1B        0x1B
#define IOPB_RES_ADDR_1C        0x1C
#define IOPB_RES_ADDR_1D        0x1D
#define IOPB_RES_ADDR_1E        0x1E
#define IOPB_RES_ADDR_1F        0x1F
#define IOPB_DMA_CFG0           0x20
#define IOPB_DMA_CFG1           0x21
#define IOPB_TICKLE             0x22
#define IOPB_DMA_REG_WR         0x23
#define IOPB_SDMA_STATUS        0x24
#define IOPB_SCSI_BYTE_CNT      0x25
#define IOPB_HOST_BYTE_CNT      0x26
#define IOPB_BYTE_LEFT_TO_XFER  0x27
#define IOPB_BYTE_TO_XFER_0     0x28
#define IOPB_BYTE_TO_XFER_1     0x29
#define IOPB_BYTE_TO_XFER_2     0x2A
#define IOPB_BYTE_TO_XFER_3     0x2B
#define IOPB_ACC_GRP            0x2C
#define IOPB_RES_ADDR_2D        0x2D
#define IOPB_DEV_ID             0x2E
#define IOPB_RES_ADDR_2F        0x2F
#define IOPB_SCSI_DATA          0x30
#define IOPB_RES_ADDR_31        0x31
#define IOPB_RES_ADDR_32        0x32
#define IOPB_SCSI_DATA_HSHK     0x33
#define IOPB_SCSI_CTRL          0x34
#define IOPB_RES_ADDR_35        0x35
#define IOPB_RES_ADDR_36        0x36
#define IOPB_RES_ADDR_37        0x37
#define IOPB_RES_ADDR_38        0x38
#define IOPB_RES_ADDR_39        0x39
#define IOPB_RES_ADDR_3A        0x3A
#define IOPB_RES_ADDR_3B        0x3B
#define IOPB_RFIFO_CNT          0x3C
#define IOPB_RES_ADDR_3D        0x3D
#define IOPB_RES_ADDR_3E        0x3E
#define IOPB_RES_ADDR_3F        0x3F
#define IOPW_CHIP_ID_0          0x00
#define IOPW_CTRL_REG           0x02
#define IOPW_RAM_ADDR           0x04
#define IOPW_RAM_DATA           0x06
#define IOPW_RES_ADDR_08        0x08
#define IOPW_RISC_CSR           0x0A
#define IOPW_SCSI_CFG0          0x0C
#define IOPW_SCSI_CFG1          0x0E
#define IOPW_RES_ADDR_10        0x10
#define IOPW_SEL_MASK           0x12
#define IOPW_RES_ADDR_14        0x14
#define IOPW_FLASH_ADDR         0x16
#define IOPW_RES_ADDR_18        0x18
#define IOPW_EE_CMD             0x1A
#define IOPW_EE_DATA            0x1C
#define IOPW_SFIFO_CNT          0x1E
#define IOPW_RES_ADDR_20        0x20
#define IOPW_Q_BASE             0x22
#define IOPW_QP                 0x24
#define IOPW_IX                 0x26
#define IOPW_SP                 0x28
#define IOPW_PC                 0x2A
#define IOPW_RES_ADDR_2C        0x2C
#define IOPW_RES_ADDR_2E        0x2E
#define IOPW_SCSI_DATA          0x30
#define IOPW_SCSI_DATA_HSHK     0x32
#define IOPW_SCSI_CTRL          0x34
#define IOPW_HSHK_CFG           0x36
#define IOPW_SXFR_STATUS        0x36
#define IOPW_SXFR_CNTL          0x38
#define IOPW_SXFR_CNTH          0x3A
#define IOPW_RES_ADDR_3C        0x3C
#define IOPW_RFIFO_DATA         0x3E
#define IOPDW_RES_ADDR_0         0x00
#define IOPDW_RAM_DATA           0x04
#define IOPDW_RES_ADDR_8         0x08
#define IOPDW_RES_ADDR_C         0x0C
#define IOPDW_RES_ADDR_10        0x10
#define IOPDW_RES_ADDR_14        0x14
#define IOPDW_RES_ADDR_18        0x18
#define IOPDW_RES_ADDR_1C        0x1C
#define IOPDW_SDMA_ADDR0         0x20
#define IOPDW_SDMA_ADDR1         0x24
#define IOPDW_SDMA_COUNT         0x28
#define IOPDW_SDMA_ERROR         0x2C
#define IOPDW_RDMA_ADDR0         0x30
#define IOPDW_RDMA_ADDR1         0x34
#define IOPDW_RDMA_COUNT         0x38
#define IOPDW_RDMA_ERROR         0x3C
#define ADV_CHIP_ID_BYTE         0x25
#define ADV_CHIP_ID_WORD         0x04C1
#define ADV_SC_SCSI_BUS_RESET    0x2000
#define ADV_INTR_ENABLE_HOST_INTR                   0x01
#define ADV_INTR_ENABLE_SEL_INTR                    0x02
#define ADV_INTR_ENABLE_DPR_INTR                    0x04
#define ADV_INTR_ENABLE_RTA_INTR                    0x08
#define ADV_INTR_ENABLE_RMA_INTR                    0x10
#define ADV_INTR_ENABLE_RST_INTR                    0x20
#define ADV_INTR_ENABLE_DPE_INTR                    0x40
#define ADV_INTR_ENABLE_GLOBAL_INTR                 0x80
#define ADV_INTR_STATUS_INTRA            0x01
#define ADV_INTR_STATUS_INTRB            0x02
#define ADV_INTR_STATUS_INTRC            0x04
#define ADV_RISC_CSR_STOP           (0x0000)
#define ADV_RISC_TEST_COND          (0x2000)
#define ADV_RISC_CSR_RUN            (0x4000)
#define ADV_RISC_CSR_SINGLE_STEP    (0x8000)
#define ADV_CTRL_REG_HOST_INTR      0x0100
#define ADV_CTRL_REG_SEL_INTR       0x0200
#define ADV_CTRL_REG_DPR_INTR       0x0400
#define ADV_CTRL_REG_RTA_INTR       0x0800
#define ADV_CTRL_REG_RMA_INTR       0x1000
#define ADV_CTRL_REG_RES_BIT14      0x2000
#define ADV_CTRL_REG_DPE_INTR       0x4000
#define ADV_CTRL_REG_POWER_DONE     0x8000
#define ADV_CTRL_REG_ANY_INTR       0xFF00
#define ADV_CTRL_REG_CMD_RESET             0x00C6
#define ADV_CTRL_REG_CMD_WR_IO_REG         0x00C5
#define ADV_CTRL_REG_CMD_RD_IO_REG         0x00C4
#define ADV_CTRL_REG_CMD_WR_PCI_CFG_SPACE  0x00C3
#define ADV_CTRL_REG_CMD_RD_PCI_CFG_SPACE  0x00C2
#define ADV_SCSI_CTRL_RSTOUT        0x2000
#define AdvIsIntPending(port)  \
(AdvReadWordRegister(port, IOPW_CTRL_REG) & ADV_CTRL_REG_HOST_INTR)
#define TIMER_MODEAB    0xC000
#define PARITY_EN       0x2000
#define EVEN_PARITY     0x1000
#define WD_LONG         0x0800
#define QUEUE_128       0x0400
#define PRIM_MODE       0x0100
#define SCAM_EN         0x0080
#define SEL_TMO_LONG    0x0040
#define CFRM_ID         0x0020
#define OUR_ID_EN       0x0010
#define OUR_ID          0x000F
#define BIG_ENDIAN      0x8000
#define TERM_POL        0x2000
#define SLEW_RATE       0x1000
#define FILTER_SEL      0x0C00
#define  FLTR_DISABLE    0x0000
#define  FLTR_11_TO_20NS 0x0800
#define  FLTR_21_TO_39NS 0x0C00
#define ACTIVE_DBL      0x0200
#define DIFF_MODE       0x0100
#define DIFF_SENSE      0x0080
#define TERM_CTL_SEL    0x0040
#define TERM_CTL        0x0030
#define  TERM_CTL_H      0x0020
#define  TERM_CTL_L      0x0010
#define CABLE_DETECT    0x000F
#define CABLE_ILLEGAL_A 0x7
#define CABLE_ILLEGAL_B 0xB
#define BIOS_EN         0x40
#define FAST_EE_CLK     0x20
#define RAM_SZ          0x1C
#define  RAM_SZ_2KB      0x00
#define  RAM_SZ_4KB      0x04
#define  RAM_SZ_8KB      0x08
#define  RAM_SZ_16KB     0x0C
#define  RAM_SZ_32KB     0x10
#define  RAM_SZ_64KB     0x14
#define BC_THRESH_ENB   0x80
#define FIFO_THRESH     0x70
#define  FIFO_THRESH_16B  0x00
#define  FIFO_THRESH_32B  0x20
#define  FIFO_THRESH_48B  0x30
#define  FIFO_THRESH_64B  0x40
#define  FIFO_THRESH_80B  0x50
#define  FIFO_THRESH_96B  0x60
#define  FIFO_THRESH_112B 0x70
#define START_CTL       0x0C
#define  START_CTL_TH    0x00
#define  START_CTL_ID    0x04
#define  START_CTL_THID  0x08
#define  START_CTL_EMFU  0x0C
#define READ_CMD        0x03
#define  READ_CMD_MR     0x00
#define  READ_CMD_MRL    0x02
#define  READ_CMD_MRM    0x03
#define ADV_TRUE        1
#define ADV_FALSE       0
#define ADV_NOERROR     1
#define ADV_SUCCESS     1
#define ADV_BUSY        0
#define ADV_ERROR       (-1)
#define ASC_WARN_EEPROM_CHKSUM          0x0002
#define ASC_WARN_EEPROM_TERMINATION     0x0004
#define ASC_WARN_SET_PCI_CONFIG_SPACE   0x0080
#define ASC_WARN_ERROR                  0xFFFF
#define ADV_MAX_TID                     15
#define ADV_MAX_LUN                     7
#define ASC_IERR_WRITE_EEPROM       0x0001
#define ASC_IERR_MCODE_CHKSUM       0x0002
#define ASC_IERR_START_STOP_CHIP    0x0008
#define ASC_IERR_CHIP_VERSION       0x0040
#define ASC_IERR_SET_SCSI_ID        0x0080
#define ASC_IERR_BAD_SIGNATURE      0x0200
#define ASC_IERR_ILLEGAL_CONNECTION 0x0400
#define ASC_IERR_SINGLE_END_DEVICE  0x0800
#define ASC_IERR_REVERSED_CABLE     0x1000
#define ASC_IERR_RW_LRAM            0x8000
#define ASC_MC_CODE_BEGIN_ADDR          0x0028
#define ASC_MC_CODE_END_ADDR            0x002A
#define ASC_MC_CODE_CHK_SUM             0x002C
#define ASC_MC_STACK_BEGIN              0x002E
#define ASC_MC_STACK_END                0x0030
#define ASC_MC_VERSION_DATE             0x0038
#define ASC_MC_VERSION_NUM              0x003A
#define ASCV_VER_SERIAL_W               0x003C
#define ASC_MC_BIOSMEM                  0x0040
#define ASC_MC_BIOSLEN                  0x0050
#define ASC_MC_HALTCODE                 0x0094
#define ASC_MC_CALLERPC                 0x0096
#define ASC_MC_ADAPTER_SCSI_ID          0x0098
#define ASC_MC_ULTRA_ABLE               0x009C
#define ASC_MC_SDTR_ABLE                0x009E
#define ASC_MC_TAGQNG_ABLE              0x00A0
#define ASC_MC_DISC_ENABLE              0x00A2
#define ASC_MC_IDLE_CMD                 0x00A6
#define ASC_MC_IDLE_PARA_STAT           0x00A8
#define ASC_MC_DEFAULT_SCSI_CFG0        0x00AC
#define ASC_MC_DEFAULT_SCSI_CFG1        0x00AE
#define ASC_MC_DEFAULT_MEM_CFG          0x00B0
#define ASC_MC_DEFAULT_SEL_MASK         0x00B2
#define ASC_MC_RISC_NEXT_READY          0x00B4
#define ASC_MC_RISC_NEXT_DONE           0x00B5
#define ASC_MC_SDTR_DONE                0x00B6
#define ASC_MC_NUMBER_OF_QUEUED_CMD     0x00C0
#define ASC_MC_NUMBER_OF_MAX_CMD        0x00D0
#define ASC_MC_DEVICE_HSHK_CFG_TABLE    0x0100
#define ASC_MC_WDTR_ABLE                0x0120
#define ASC_MC_CONTROL_FLAG             0x0122
#define ASC_MC_WDTR_DONE                0x0124
#define ASC_MC_HOST_NEXT_READY          0x0128
#define ASC_MC_HOST_NEXT_DONE           0x0129
#define BIOS_CODESEG    0x54
#define BIOS_CODELEN    0x56
#define BIOS_SIGNATURE  0x58
#define BIOS_VERSION    0x5A
#define BIOS_SIGNATURE  0x58
#define CONTROL_FLAG_IGNORE_PERR        0x0001
#define HSHK_CFG_WIDE_XFR       0x8000
#define HSHK_CFG_RATE           0x0F00
#define HSHK_CFG_OFFSET         0x001F
#define ASC_MC_RISC_Q_LIST_BASE         0x1200
#define ASC_MC_RISC_Q_LIST_SIZE         0x0008
#define ASC_MC_RISC_Q_TOTAL_CNT         0x00FF
#define ASC_MC_RISC_Q_FIRST             0x0001
#define ASC_MC_RISC_Q_LAST              0x00FF
#define ASC_DEF_MAX_HOST_QNG    0xFD
#define ASC_DEF_MIN_HOST_QNG    0x10
#define ASC_DEF_MAX_DVC_QNG     0x3F
#define ASC_DEF_MIN_DVC_QNG     0x04
#define RQL_FWD     0
#define RQL_BWD     1
#define RQL_STATE   2
#define RQL_TID     3
#define RQL_PHYADDR 4
#define ASC_MC_QS_FREE                  0x00
#define ASC_MC_QS_READY                 0x01
#define ASC_MC_QS_DONE                  0x40
#define ASC_MC_QS_ABORTED               0x80
#define ASC_MC_NULL_Q                   0x00
#define ASC_MC_BIOS_Q                   0xFF
#define ASC_MC_QC_START_MOTOR           0x02
#define ASC_MC_QC_NO_OVERRUN            0x04
#define ASC_MC_QC_FIRST_DMA             0x08
#define ASC_MC_QC_ABORTED               0x10
#define ASC_MC_QC_REQ_SENSE             0x20
#define ASC_MC_QC_DOS_REQ               0x80
#define ADV_POLL_REQUEST                0x01
#define ADV_SCSIQ_DONE                  0x02
typedef struct adv_dvc_cfg {
ushort disc_enable;
uchar  chip_version;
uchar  termination;
ushort pci_device_id;
ushort lib_version;
ushort control_flag;
ushort mcode_date;
ushort mcode_version;
ushort pci_slot_info;
ushort bios_boot_wait;
ushort serial1;
ushort serial2;
ushort serial3;
} ADV_DVC_CFG;
typedef struct adv_dvc_var {
AdvPortAddr iop_base;
ushort err_code;
ushort bios_ctrl;
Ptr2Func isr_callback;
Ptr2Func sbreset_callback;
ushort wdtr_able;
ushort sdtr_able;
ushort ultra_able;
ushort tagqng_able;
uchar  max_dvc_qng;
ushort start_motor;
uchar  scsi_reset_wait;
uchar  chip_no;
uchar  max_host_qng;
uchar  cur_host_qng;
uchar  irq_no;
ushort no_scam;
ushort idle_cmd_done;
ulong  drv_ptr;
uchar  chip_scsi_id;
ADV_DVC_CFG *cfg;
} ADV_DVC_VAR;
#define NO_OF_SG_PER_BLOCK              15
typedef struct asc_sg_block {
uchar reserved1;
uchar reserved2;
uchar first_entry_no;
uchar last_entry_no;
struct asc_sg_block *sg_ptr;
struct  {
ulong sg_addr;
ulong sg_count;
} sg_list[NO_OF_SG_PER_BLOCK];
} ADV_SG_BLOCK;
typedef struct adv_scsi_req_q {
uchar       cntl;
uchar       sg_entry_cnt;
uchar       target_id;
uchar       target_lun;
ulong       data_addr;
ulong       data_cnt;
ulong       sense_addr;
ulong       srb_ptr;
uchar       a_flag;
uchar       sense_len;
uchar       cdb_len;
uchar       tag_code;
uchar       done_status;
uchar       scsi_status;
uchar       host_status;
uchar       ux_sg_ix;
uchar       cdb[12];
ulong       sg_real_addr;
struct adv_scsi_req_q *free_scsiq_link;
ulong       ux_wk_data_cnt;
struct adv_scsi_req_q *scsiq_ptr;
ADV_SG_BLOCK *sg_list_ptr;
ulong       vsense_addr;
ulong       vdata_addr;
uchar       orig_sense_len;
} ADV_SCSI_REQ_Q;
#define IDLE_CMD_COMPLETED           0
#define IDLE_CMD_STOP_CHIP           0x0001
#define IDLE_CMD_STOP_CHIP_SEND_INT  0x0002
#define IDLE_CMD_SEND_INT            0x0004
#define IDLE_CMD_ABORT               0x0008
#define IDLE_CMD_DEVICE_RESET        0x0010
#define IDLE_CMD_SCSI_RESET          0x0020
#define ADV_NOWAIT     0x01
#define SCSI_WAIT_10_SEC             10
#define SCSI_MS_PER_SEC              1000
STATIC long  DvcEnterCritical(void);
STATIC void  DvcLeaveCritical(long);
STATIC void  DvcSleepMilliSecond(ulong);
STATIC uchar DvcAdvReadPCIConfigByte(ADV_DVC_VAR *, ushort);
STATIC void  DvcAdvWritePCIConfigByte(ADV_DVC_VAR *, ushort, uchar);
STATIC ulong DvcGetPhyAddr(ADV_DVC_VAR *, ADV_SCSI_REQ_Q *,
uchar *, long *, int);
STATIC void  DvcDelayMicroSecond(ADV_DVC_VAR *, ushort);
STATIC int     AdvExeScsiQueue(ADV_DVC_VAR *,
ADV_SCSI_REQ_Q *);
STATIC int     AdvISR(ADV_DVC_VAR *);
STATIC int     AdvInitGetConfig(ADV_DVC_VAR *);
STATIC int     AdvInitAsc3550Driver(ADV_DVC_VAR *);
STATIC int     AdvResetSB(ADV_DVC_VAR *);
STATIC int    AdvSendIdleCmd(ADV_DVC_VAR *, ushort, ulong, int);
STATIC void   AdvResetChip(ADV_DVC_VAR *);
STATIC int    AdvSendScsiCmd(ADV_DVC_VAR *, ADV_SCSI_REQ_Q *);
STATIC void   AdvInquiryHandling(ADV_DVC_VAR *, ADV_SCSI_REQ_Q *);
STATIC int    AdvInitFromEEP(ADV_DVC_VAR *);
STATIC ushort AdvGetEEPConfig(AdvPortAddr, ADVEEP_CONFIG *);
STATIC void   AdvSetEEPConfig(AdvPortAddr, ADVEEP_CONFIG *);
STATIC void   AdvWaitEEPCmd(AdvPortAddr);
STATIC ushort AdvReadEEPWord(AdvPortAddr, int);
STATIC void   AdvResetSCSIBus(ADV_DVC_VAR *);
#define AscPCICmdRegBits_BusMastering     0x0007
#define AscPCICmdRegBits_ParErrRespCtrl   0x0040
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
#define AdvReadByteRegister(iop_base, reg_off) \
(inp((iop_base) + (reg_off)))
#define AdvWriteByteRegister(iop_base, reg_off, byte) \
(outp((iop_base) + (reg_off), (byte)))
#define AdvReadWordRegister(iop_base, reg_off) \
(inpw((iop_base) + (reg_off)))
#define AdvWriteWordRegister(iop_base, reg_off, word) \
(outpw((iop_base) + (reg_off), (word)))
#define AdvReadByteLram(iop_base, addr, byte) \
do { \
outpw((iop_base) + IOPW_RAM_ADDR, (addr)); \
(byte) = inp((iop_base) + IOPB_RAM_DATA); \
} while (0)
#define AdvWriteByteLram(iop_base, addr, byte) \
(outpw((iop_base) + IOPW_RAM_ADDR, (addr)), \
outp((iop_base) + IOPB_RAM_DATA, (byte)))
#define AdvReadWordLram(iop_base, addr, word) \
do { \
outpw((iop_base) + IOPW_RAM_ADDR, (addr));  \
(word) = inpw((iop_base) + IOPW_RAM_DATA); \
} while (0)
#define AdvWriteWordLram(iop_base, addr, word) \
(outpw((iop_base) + IOPW_RAM_ADDR, (addr)), \
outpw((iop_base) + IOPW_RAM_DATA, (word)))
#define AdvWriteDWordLram(iop_base, addr, dword) \
((outpw((iop_base) + IOPW_RAM_ADDR, (addr)), \
outpw((iop_base) + IOPW_RAM_DATA, (ushort) ((dword) & 0xFFFF))), \
(outpw((iop_base) + IOPW_RAM_ADDR, (addr) + 2), \
outpw((iop_base) + IOPW_RAM_DATA, (ushort) ((dword >> 16) & 0xFFFF))))
#define AdvReadWordAutoIncLram(iop_base) \
(inpw((iop_base) + IOPW_RAM_DATA))
#define AdvWriteWordAutoIncLram(iop_base, word) \
(outpw((iop_base) + IOPW_RAM_DATA, (word)))
#else
#define AdvReadByteRegister(iop_base, reg_off) \
(ADV_MEM_READB((iop_base) + (reg_off)))
#define AdvWriteByteRegister(iop_base, reg_off, byte) \
(ADV_MEM_WRITEB((iop_base) + (reg_off), (byte)))
#define AdvReadWordRegister(iop_base, reg_off) \
(ADV_MEM_READW((iop_base) + (reg_off)))
#define AdvWriteWordRegister(iop_base, reg_off, word) \
(ADV_MEM_WRITEW((iop_base) + (reg_off), (word)))
#define AdvReadByteLram(iop_base, addr, byte) \
do { \
ADV_MEM_WRITEW((iop_base) + IOPW_RAM_ADDR, (addr)); \
(byte) = ADV_MEM_READB((iop_base) + IOPB_RAM_DATA); \
} while (0)
#define AdvWriteByteLram(iop_base, addr, byte) \
(ADV_MEM_WRITEW((iop_base) + IOPW_RAM_ADDR, (addr)), \
ADV_MEM_WRITEB((iop_base) + IOPB_RAM_DATA, (byte)))
#define AdvReadWordLram(iop_base, addr, word) \
do { \
ADV_MEM_WRITEW((iop_base) + IOPW_RAM_ADDR, (addr)); \
(word) = ADV_MEM_READW((iop_base) + IOPW_RAM_DATA); \
} while (0)
#define AdvWriteWordLram(iop_base, addr, word) \
(ADV_MEM_WRITEW((iop_base) + IOPW_RAM_ADDR, (addr)), \
ADV_MEM_WRITEW((iop_base) + IOPW_RAM_DATA, (word)))
#define AdvWriteDWordLram(iop_base, addr, dword) \
((ADV_MEM_WRITEW((iop_base) + IOPW_RAM_ADDR, (addr)), \
ADV_MEM_WRITEW((iop_base) + IOPW_RAM_DATA, \
(ushort) ((dword) & 0xFFFF))), \
(ADV_MEM_WRITEW((iop_base) + IOPW_RAM_ADDR, (addr) + 2), \
ADV_MEM_WRITEW((iop_base) + IOPW_RAM_DATA, \
(ushort) ((dword >> 16) & 0xFFFF))))
#define AdvReadWordAutoIncLram(iop_base) \
(ADV_MEM_READW((iop_base) + IOPW_RAM_DATA))
#define AdvWriteWordAutoIncLram(iop_base, word) \
(ADV_MEM_WRITEW((iop_base) + IOPW_RAM_DATA, (word)))
#endif
#define AdvFindSignature(iop_base) \
(((AdvReadByteRegister((iop_base), IOPB_CHIP_ID_1) == \
ADV_CHIP_ID_BYTE) && \
(AdvReadWordRegister((iop_base), IOPW_CHIP_ID_0) == \
ADV_CHIP_ID_WORD)) ?  ADV_TRUE : ADV_FALSE)
#define AdvGetChipVersion(iop_base, bus_type) \
AdvReadByteRegister((iop_base), IOPB_CHIP_TYPE_REV)
#define AdvAbortSRB(asc_dvc, srb_ptr) \
AdvSendIdleCmd((asc_dvc), (ushort) IDLE_CMD_ABORT, \
(ulong) (srb_ptr), 0)
#define AdvResetDevice(asc_dvc, target_id) \
AdvSendIdleCmd((asc_dvc), (ushort) IDLE_CMD_DEVICE_RESET, \
(ulong) (target_id), 0)
#define ADV_SCSI_BIT_ID_TYPE   ushort
#define ADV_SCAN_LUN           0x01
#define ADV_CAPINFO_NOLUN      0x02
#define ADV_TID_TO_TIDMASK(tid)   (0x01 << ((tid) & ADV_MAX_TID))
#define QD_NO_STATUS         0x00
#define QD_NO_ERROR          0x01
#define QD_ABORTED_BY_HOST   0x02
#define QD_WITH_ERROR        0x04
#define QHSTA_NO_ERROR              0x00
#define QHSTA_M_SEL_TIMEOUT         0x11
#define QHSTA_M_DATA_OVER_RUN       0x12
#define QHSTA_M_UNEXPECTED_BUS_FREE 0x13
#define QHSTA_M_QUEUE_ABORTED       0x15
#define QHSTA_M_SXFR_SDMA_ERR       0x16
#define QHSTA_M_SXFR_SXFR_PERR      0x17
#define QHSTA_M_RDMA_PERR           0x18
#define QHSTA_M_SXFR_OFF_UFLW       0x19
#define QHSTA_M_SXFR_OFF_OFLW       0x20
#define QHSTA_M_SXFR_WD_TMO         0x21
#define QHSTA_M_SXFR_DESELECTED     0x22
#define QHSTA_M_SXFR_XFR_OFLW       0x12
#define QHSTA_M_SXFR_XFR_PH_ERR     0x24
#define QHSTA_M_SXFR_UNKNOWN_ERROR  0x25
#define QHSTA_M_WTM_TIMEOUT         0x41
#define QHSTA_M_BAD_CMPL_STATUS_IN  0x42
#define QHSTA_M_NO_AUTO_REQ_SENSE   0x43
#define QHSTA_M_AUTO_REQ_SENSE_FAIL 0x44
#define QHSTA_M_INVALID_DEVICE      0x45
typedef int (* ADV_ISR_CALLBACK)
(ADV_DVC_VAR *, ADV_SCSI_REQ_Q *);
typedef int (* ADV_SBRESET_CALLBACK)
(ADV_DVC_VAR *);
STATIC ADVEEP_CONFIG Default_EEPROM_Config;
#define ADV_IS_SCSIQ_FLAG       0x01
#define ADV_ASCGETSGLIST_VADDR  0x02
#define ADV_IS_SENSE_FLAG       0x04
#define ADV_IS_DATA_FLAG        0x08
#define ADV_IS_SGLIST_FLAG      0x10
#define ADV_DWALIGN(addr)       (((ulong) (addr) + 0x3) & ~0x3)
#ifndef ADV_MAX_SG_LIST
Forced Error: Driver must define ADV_MAX_SG_LIST.
#endif
#define ADV_SG_LIST_MAX_BYTE_SIZE \
(sizeof(ADV_SG_BLOCK) * \
((ADV_MAX_SG_LIST + (NO_OF_SG_PER_BLOCK - 1))/NO_OF_SG_PER_BLOCK))
#ifndef ADV_ASSERT
#define ADV_ASSERT(a)
#endif
#define ASC_NUM_BOARD_SUPPORTED 16
#define ASC_NUM_IOPORT_PROBE    4
#define ASC_NUM_BUS             4
#define ASC_BOARDP(host) ((asc_board_t *) &((host)->hostdata))
#define ASC_HOST_IN_RESET       0x01
#define ASC_HOST_IN_ABORT       0x02
#define ASC_IS_WIDE_BOARD       0x04
#define ASC_SELECT_QUEUE_DEPTHS 0x08
#define ASC_NARROW_BOARD(boardp) (((boardp)->flags & ASC_IS_WIDE_BOARD) == 0)
#define ASC_WIDE_BOARD(boardp)   ((boardp)->flags & ASC_IS_WIDE_BOARD)
#define NO_ISA_DMA              0xff
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,23)
#define ASC_INITFUNC(func)      func
#define ASC_INITDATA
#define ASC_INIT
#else
#define ASC_INITFUNC(func)      __initfunc(func)
#define ASC_INITDATA            __initdata
#define ASC_INIT                __init
#endif
#define ASC_INFO_SIZE           128
#define ASC_PRTBUF_SIZE         2048
#define ASC_PRTLINE_SIZE        160
#define ASC_PRT_NEXT() \
if (cp) { \
totlen += len; \
leftlen -= len; \
if (leftlen == 0) { \
return totlen; \
} \
cp += len; \
}
#define ASC_MIN(a, b) (((a) < (b)) ? (a) : (b))
#define ASC_TRUE        1
#define ASC_FALSE       0
#define ASC_NOERROR     1
#define ASC_BUSY        0
#define ASC_ERROR       (-1)
#define STATUS_BYTE(byte)   (byte)
#define MSG_BYTE(byte)      ((byte) << 8)
#define HOST_BYTE(byte)     ((byte) << 16)
#define DRIVER_BYTE(byte)   ((byte) << 24)
typedef Scsi_Cmnd            REQ, *REQP;
#define REQPNEXT(reqp)       ((reqp)->host_scribble)
#define REQPNEXTP(reqp)      ((REQP *) &((reqp)->host_scribble))
#define REQPTID(reqp)        ((reqp)->target)
#define REQPTIME(reqp)       ((reqp)->SCp.this_residual)
#define REQTIMESTAMP()       (jiffies)
#define REQTIMESTAT(function, ascq, reqp, tid) \
{ \
\
if (REQPTIME(reqp) <= REQTIMESTAMP()) { \
REQPTIME(reqp) = REQTIMESTAMP() - REQPTIME(reqp); \
} else { \
\
ASC_ASSERT(REQPTIME(reqp) <= REQTIMESTAMP()); \
REQPTIME(reqp) = 0; \
} \
\
if (((ascq)->q_tot_cnt[tid] == 1) ||  \
(REQPTIME(reqp) < (ascq)->q_min_tim[tid])) { \
(ascq)->q_min_tim[tid] = REQPTIME(reqp); \
ASC_DBG3(1, "%s: new q_min_tim[%d] %u\n", \
(function), (tid), (ascq)->q_min_tim[tid]); \
} \
if (REQPTIME(reqp) > (ascq)->q_max_tim[tid]) { \
(ascq)->q_max_tim[tid] = REQPTIME(reqp); \
ASC_DBG3(1, "%s: new q_max_tim[%d] %u\n", \
(function), tid, (ascq)->q_max_tim[tid]); \
} \
(ascq)->q_tot_tim[tid] += REQPTIME(reqp); \
\
REQPTIME(reqp) = 0; \
}
#define ASC_FRONT       1
#define ASC_BACK        2
#define ASC_TID_ALL        (-1)
#define ASC_QUEUE_EMPTY(ascq)    ((ascq)->q_tidmask == 0)
#define PCI_BASE_CLASS_PREDEFINED               0x00
#define PCI_BASE_CLASS_MASS_STORAGE             0x01
#define PCI_BASE_CLASS_NETWORK                  0x02
#define PCI_BASE_CLASS_DISPLAY                  0x03
#define PCI_BASE_CLASS_MULTIMEDIA               0x04
#define PCI_BASE_CLASS_MEMORY_CONTROLLER        0x05
#define PCI_BASE_CLASS_BRIDGE_DEVICE            0x06
#define PCI_SUB_CLASS_SCSI_CONTROLLER           0x00
#define PCI_SUB_CLASS_IDE_CONTROLLER            0x01
#define PCI_SUB_CLASS_FLOPPY_DISK_CONTROLLER    0x02
#define PCI_SUB_CLASS_IPI_BUS_CONTROLLER        0x03
#define PCI_SUB_CLASS_OTHER_MASS_CONTROLLER     0x80
#define PCI_SUB_CLASS_ETHERNET_CONTROLLER       0x00
#define PCI_SUB_CLASS_TOKEN_RING_CONTROLLER     0x01
#define PCI_SUB_CLASS_FDDI_CONTROLLER           0x02
#define PCI_SUB_CLASS_OTHER_NETWORK_CONTROLLER  0x80
#define PCI_SUB_CLASS_VGA_CONTROLLER            0x00
#define PCI_SUB_CLASS_XGA_CONTROLLER            0x01
#define PCI_SUB_CLASS_OTHER_DISPLAY_CONTROLLER  0x80
#define PCI_SUB_CLASS_VIDEO_DEVICE              0x00
#define PCI_SUB_CLASS_AUDIO_DEVICE              0x01
#define PCI_SUB_CLASS_OTHER_MULTIMEDIA_DEVICE   0x80
#define PCI_SUB_CLASS_RAM_CONTROLLER            0x00
#define PCI_SUB_CLASS_FLASH_CONTROLLER          0x01
#define PCI_SUB_CLASS_OTHER_MEMORY_CONTROLLER   0x80
#define PCI_SUB_CLASS_HOST_BRIDGE_CONTROLLER    0x00
#define PCI_SUB_CLASS_ISA_BRIDGE_CONTROLLER     0x01
#define PCI_SUB_CLASS_EISA_BRIDGE_CONTROLLER    0x02
#define PCI_SUB_CLASS_MC_BRIDGE_CONTROLLER      0x03
#define PCI_SUB_CLASS_PCI_TO_PCI_BRIDGE_CONTROLLER    0x04
#define PCI_SUB_CLASS_PCMCIA_BRIDGE_CONTROLLER  0x05
#define PCI_SUB_CLASS_OTHER_BRIDGE_CONTROLLER   0x80
#define PCI_MAX_SLOT            0x1F
#define PCI_MAX_BUS             0xFF
#define PCI_IOADDRESS_MASK      0xFFFE
#define ASC_PCI_VENDORID        0x10CD
#define ASC_PCI_DEVICE_ID_CNT   4
#define ASC_PCI_DEVICE_ID_1100  0x1100
#define ASC_PCI_DEVICE_ID_1200  0x1200
#define ASC_PCI_DEVICE_ID_1300  0x1300
#define ASC_PCI_DEVICE_ID_2300  0x2300
#define PCI_CONFIG_ADDRESS_MECH1          0x0CF8
#define PCI_CONFIG_DATA_MECH1             0x0CFC
#define PCI_CONFIG_FORWARD_REGISTER       0x0CFA
#define PCI_CONFIG_BUS_NUMBER_MASK        0x00FF0000
#define PCI_CONFIG_DEVICE_FUNCTION_MASK   0x0000FF00
#define PCI_CONFIG_REGISTER_NUMBER_MASK   0x000000F8
#define PCI_DEVICE_FOUND                0x0000
#define PCI_DEVICE_NOT_FOUND            0xffff
#define SUBCLASS_OFFSET         0x0A
#define CLASSCODE_OFFSET        0x0B
#define VENDORID_OFFSET         0x00
#define DEVICEID_OFFSET         0x02
#ifndef ADVANSYS_STATS
#define ASC_STATS(shp, counter)
#define ASC_STATS_ADD(shp, counter, count)
#else
#define ASC_STATS(shp, counter) \
(ASC_BOARDP(shp)->asc_stats.counter++)
#define ASC_STATS_ADD(shp, counter, count) \
(ASC_BOARDP(shp)->asc_stats.counter += (count))
#endif
#define ASC_CEILING(val, unit) (((val) + ((unit) - 1))/(unit))
#define ASC_TENTHS(num, den) \
(((10 * ((num)/(den))) > (((num) * 10)/(den))) ? \
0 : ((((num) * 10)/(den)) - (10 * ((num)/(den)))))
#define ASC_PRINT(s) \
{ \
printk("advansys: "); \
printk(s); \
}
#define ASC_PRINT1(s, a1) \
{ \
printk("advansys: "); \
printk((s), (a1)); \
}
#define ASC_PRINT2(s, a1, a2) \
{ \
printk("advansys: "); \
printk((s), (a1), (a2)); \
}
#define ASC_PRINT3(s, a1, a2, a3) \
{ \
printk("advansys: "); \
printk((s), (a1), (a2), (a3)); \
}
#define ASC_PRINT4(s, a1, a2, a3, a4) \
{ \
printk("advansys: "); \
printk((s), (a1), (a2), (a3), (a4)); \
}
#ifndef ADVANSYS_DEBUG
#define ASC_DBG(lvl, s)
#define ASC_DBG1(lvl, s, a1)
#define ASC_DBG2(lvl, s, a1, a2)
#define ASC_DBG3(lvl, s, a1, a2, a3)
#define ASC_DBG4(lvl, s, a1, a2, a3, a4)
#define ASC_DBG_PRT_SCSI_HOST(lvl, s)
#define ASC_DBG_PRT_SCSI_CMND(lvl, s)
#define ASC_DBG_PRT_ASC_SCSI_Q(lvl, scsiqp)
#define ASC_DBG_PRT_ADV_SCSI_REQ_Q(lvl, scsiqp)
#define ASC_DBG_PRT_ASC_QDONE_INFO(lvl, qdone)
#define ADV_DBG_PRT_ADV_SCSI_REQ_Q(lvl, scsiqp)
#define ASC_DBG_PRT_HEX(lvl, name, start, length)
#define ASC_DBG_PRT_CDB(lvl, cdb, len)
#define ASC_DBG_PRT_SENSE(lvl, sense, len)
#define ASC_DBG_PRT_INQUIRY(lvl, inq, len)
#else
#define ASC_DBG(lvl, s) \
{ \
if (asc_dbglvl >= (lvl)) { \
printk(s); \
} \
}
#define ASC_DBG1(lvl, s, a1) \
{ \
if (asc_dbglvl >= (lvl)) { \
printk((s), (a1)); \
} \
}
#define ASC_DBG2(lvl, s, a1, a2) \
{ \
if (asc_dbglvl >= (lvl)) { \
printk((s), (a1), (a2)); \
} \
}
#define ASC_DBG3(lvl, s, a1, a2, a3) \
{ \
if (asc_dbglvl >= (lvl)) { \
printk((s), (a1), (a2), (a3)); \
} \
}
#define ASC_DBG4(lvl, s, a1, a2, a3, a4) \
{ \
if (asc_dbglvl >= (lvl)) { \
printk((s), (a1), (a2), (a3), (a4)); \
} \
}
#define ASC_DBG_PRT_SCSI_HOST(lvl, s) \
{ \
if (asc_dbglvl >= (lvl)) { \
asc_prt_scsi_host(s); \
} \
}
#define ASC_DBG_PRT_SCSI_CMND(lvl, s) \
{ \
if (asc_dbglvl >= (lvl)) { \
asc_prt_scsi_cmnd(s); \
} \
}
#define ASC_DBG_PRT_ASC_SCSI_Q(lvl, scsiqp) \
{ \
if (asc_dbglvl >= (lvl)) { \
asc_prt_asc_scsi_q(scsiqp); \
} \
}
#define ASC_DBG_PRT_ASC_QDONE_INFO(lvl, qdone) \
{ \
if (asc_dbglvl >= (lvl)) { \
asc_prt_asc_qdone_info(qdone); \
} \
}
#define ASC_DBG_PRT_ADV_SCSI_REQ_Q(lvl, scsiqp) \
{ \
if (asc_dbglvl >= (lvl)) { \
asc_prt_adv_scsi_req_q(scsiqp); \
} \
}
#define ASC_DBG_PRT_HEX(lvl, name, start, length) \
{ \
if (asc_dbglvl >= (lvl)) { \
asc_prt_hex((name), (start), (length)); \
} \
}
#define ASC_DBG_PRT_CDB(lvl, cdb, len) \
ASC_DBG_PRT_HEX((lvl), "CDB", (uchar *) (cdb), (len));
#define ASC_DBG_PRT_SENSE(lvl, sense, len) \
ASC_DBG_PRT_HEX((lvl), "SENSE", (uchar *) (sense), (len));
#define ASC_DBG_PRT_INQUIRY(lvl, inq, len) \
ASC_DBG_PRT_HEX((lvl), "INQUIRY", (uchar *) (inq), (len));
#endif
#ifndef ADVANSYS_ASSERT
#define ASC_ASSERT(a)
#else
#define ASC_ASSERT(a) \
{ \
if (!(a)) { \
printk("ASC_ASSERT() Failure: file %s, line %d\n", \
__FILE__, __LINE__); \
} \
}
#endif
#ifdef ADVANSYS_STATS
struct asc_stats {
ulong     command;
ulong     queuecommand;
ulong     abort;
ulong     reset;
ulong     biosparam;
ulong     interrupt;
ulong     callback;
ulong     done;
ulong     build_error;
ulong     adv_build_noreq;
ulong     adv_build_nosg;
ulong     exe_noerror;
ulong     exe_busy;
ulong     exe_error;
ulong     exe_unknown;
ulong     cont_cnt;
ulong     cont_xfer;
ulong     sg_cnt;
ulong     sg_elem;
ulong     sg_xfer;
};
#endif
typedef struct asc_queue {
ADV_SCSI_BIT_ID_TYPE  q_tidmask;
REQP                  q_first[ADV_MAX_TID+1];
REQP                  q_last[ADV_MAX_TID+1];
#ifdef ADVANSYS_STATS
short                 q_cur_cnt[ADV_MAX_TID+1];
short                 q_max_cnt[ADV_MAX_TID+1];
ulong                 q_tot_cnt[ADV_MAX_TID+1];
ulong                 q_tot_tim[ADV_MAX_TID+1];
ushort                q_max_tim[ADV_MAX_TID+1];
ushort                q_min_tim[ADV_MAX_TID+1];
#endif
} asc_queue_t;
typedef struct adv_sgblk {
ADV_SG_BLOCK        sg_block[ADV_NUM_SG_BLOCK + ADV_NUM_PAGE_CROSSING];
uchar               align2[4];
struct adv_sgblk    *next_sgblkp;
} adv_sgblk_t;
typedef struct adv_req {
ADV_SCSI_REQ_Q      scsi_req_q;
uchar               align1[4];
Scsi_Cmnd           *cmndp;
adv_sgblk_t         *sgblkp;
struct adv_req      *next_reqp;
} adv_req_t;
typedef struct asc_board {
int                  id;
uint                 flags;
union {
ASC_DVC_VAR      asc_dvc_var;
ADV_DVC_VAR      adv_dvc_var;
} dvc_var;
union {
ASC_DVC_CFG      asc_dvc_cfg;
ADV_DVC_CFG      adv_dvc_cfg;
} dvc_cfg;
asc_queue_t          active;
asc_queue_t          waiting;
asc_queue_t          done;
ADV_SCSI_BIT_ID_TYPE init_tidmask;
Scsi_Device          *device[ADV_MAX_TID+1];
ushort               reqcnt[ADV_MAX_TID+1];
#if ASC_QUEUE_FLOW_CONTROL
ushort               nerrcnt[ADV_MAX_TID+1];
#endif
ADV_SCSI_BIT_ID_TYPE queue_full;
ushort               queue_full_cnt[ADV_MAX_TID+1];
union {
ASCEEP_CONFIG    asc_eep;
ADVEEP_CONFIG    adv_eep;
} eep_config;
ulong                last_reset;
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
char                 *prtbuf;
#endif
#ifdef ADVANSYS_STATS
struct asc_stats     asc_stats;
#endif
ASC_SCSI_REQ_Q       scsireqq;
ASC_CAP_INFO         cap_info;
ASC_SCSI_INQUIRY     inquiry;
uchar                sdtr_data[ASC_MAX_TID+1];
void                 *ioremap_addr;
ushort               ioport;
adv_req_t            *orig_reqp;
adv_req_t            *adv_reqp;
adv_sgblk_t          *orig_sgblkp;
adv_sgblk_t          *adv_sgblkp;
ushort               bios_signature;
ushort               bios_version;
ushort               bios_codeseg;
ushort               bios_codelen;
} asc_board_t;
typedef struct _PCI_DATA_
{
uchar    type;
uchar    bus;
uchar    slot;
uchar    func;
uchar    offset;
} PCI_DATA;
typedef struct _PCI_DEVICE_
{
ushort   vendorID;
ushort   deviceID;
ushort   slotNumber;
ushort   slotFound;
uchar    busNumber;
uchar    maxBusNumber;
uchar    devFunc;
ushort   startSlot;
ushort   endSlot;
uchar    bridge;
uchar    type;
} PCI_DEVICE;
typedef struct _PCI_CONFIG_SPACE_
{
ushort   vendorID;
ushort   deviceID;
ushort   command;
ushort   status;
uchar    revision;
uchar    classCode[3];
uchar    cacheSize;
uchar    latencyTimer;
uchar    headerType;
uchar    bist;
ulong    baseAddress[6];
ushort   reserved[4];
ulong    optionRomAddr;
ushort   reserved2[4];
uchar    irqLine;
uchar    irqPin;
uchar    minGnt;
uchar    maxLatency;
} PCI_CONFIG_SPACE;
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
struct proc_dir_entry proc_scsi_advansys =
{
PROC_SCSI_ADVANSYS,
8,
"advansys",
S_IFDIR | S_IRUGO | S_IXUGO,
2
};
#endif
STATIC int asc_board_count = 0;
STATIC struct Scsi_Host    *asc_host[ASC_NUM_BOARD_SUPPORTED] = { 0 };
STATIC uchar overrun_buf[ASC_OVERRUN_BSIZE] = { 0 };
STATIC ASC_SCSI_Q asc_scsi_q = { { 0 } };
STATIC ASC_SG_HEAD asc_sg_head = { 0 };
STATIC ushort asc_bus[ASC_NUM_BUS] ASC_INITDATA = {
ASC_IS_ISA,
ASC_IS_VL,
ASC_IS_EISA,
ASC_IS_PCI,
};
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
STATIC int pci_scan_method ASC_INITDATA = -1;
#endif
#endif
STATIC int asc_iopflag = ASC_FALSE;
STATIC int asc_ioport[ASC_NUM_IOPORT_PROBE] = { 0, 0, 0, 0 };
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
uchar adv_req_buf[16 * sizeof(adv_req_t)] = { 0 };
uchar adv_sgblk_buf[16 * sizeof(adv_sgblk_t)] = { 0 };
#endif
#ifdef ADVANSYS_DEBUG
STATIC char *
asc_bus_name[ASC_NUM_BUS] = {
"ASC_IS_ISA",
"ASC_IS_VL",
"ASC_IS_EISA",
"ASC_IS_PCI",
};
STATIC int          asc_dbglvl = 0;
#endif
STATIC PortAddr     _asc_def_iop_base[];
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
STATIC int          asc_proc_copy(off_t, off_t, char *, int , char *, int);
#endif
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,70)
STATIC void         advansys_interrupt(int, struct pt_regs *);
#else
STATIC void         advansys_interrupt(int, void *, struct pt_regs *);
#endif
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC void         advansys_select_queue_depths(struct Scsi_Host *,
Scsi_Device *);
#endif
STATIC void       advansys_command_done(Scsi_Cmnd *);
STATIC void       asc_scsi_done_list(Scsi_Cmnd *);
STATIC int        asc_execute_scsi_cmnd(Scsi_Cmnd *);
STATIC int        asc_build_req(asc_board_t *, Scsi_Cmnd *);
STATIC int        adv_build_req(asc_board_t *, Scsi_Cmnd *, ADV_SCSI_REQ_Q **);
STATIC int        adv_get_sglist(ADV_DVC_VAR *, ADV_SCSI_REQ_Q *, Scsi_Cmnd *);
STATIC void       asc_isr_callback(ASC_DVC_VAR *, ASC_QDONE_INFO *);
STATIC void       adv_isr_callback(ADV_DVC_VAR *, ADV_SCSI_REQ_Q *);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
STATIC int        asc_srch_pci_dev(PCI_DEVICE *);
STATIC uchar      asc_scan_method(void);
STATIC int        asc_pci_find_dev(PCI_DEVICE *);
STATIC void       asc_get_pci_cfg(PCI_DEVICE *, PCI_CONFIG_SPACE *);
STATIC ushort     asc_get_cfg_word(PCI_DATA *);
STATIC uchar      asc_get_cfg_byte(PCI_DATA *);
STATIC void       asc_put_cfg_byte(PCI_DATA *, uchar);
#endif
#endif
STATIC void       asc_enqueue(asc_queue_t *, REQP, int);
STATIC REQP       asc_dequeue(asc_queue_t *, int);
STATIC REQP       asc_dequeue_list(asc_queue_t *, REQP *, int);
STATIC int        asc_rmqueue(asc_queue_t *, REQP);
STATIC int        asc_isqueued(asc_queue_t *, REQP);
STATIC void       asc_execute_queue(asc_queue_t *);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
STATIC int        asc_prt_board_devices(struct Scsi_Host *, char *, int);
STATIC int        asc_prt_adv_bios(struct Scsi_Host *, char *, int);
STATIC int        asc_get_eeprom_string(ushort *serialnum, uchar *cp);
STATIC int        asc_prt_asc_board_eeprom(struct Scsi_Host *, char *, int);
STATIC int        asc_prt_adv_board_eeprom(struct Scsi_Host *, char *, int);
STATIC int        asc_prt_driver_conf(struct Scsi_Host *, char *, int);
STATIC int        asc_prt_asc_board_info(struct Scsi_Host *, char *, int);
STATIC int        asc_prt_adv_board_info(struct Scsi_Host *, char *, int);
STATIC int        asc_prt_line(char *, int, char *fmt, ...);
#endif
STATIC int          AscFindSignature(PortAddr);
STATIC ushort       AscGetEEPConfig(PortAddr, ASCEEP_CONFIG *, ushort);
#ifdef ADVANSYS_STATS
STATIC int          asc_prt_board_stats(struct Scsi_Host *, char *, int);
#endif
#ifdef ADVANSYS_DEBUG
STATIC void         asc_prt_scsi_host(struct Scsi_Host *);
STATIC void         asc_prt_scsi_cmnd(Scsi_Cmnd *);
STATIC void         asc_prt_asc_dvc_cfg(ASC_DVC_CFG *);
STATIC void         asc_prt_asc_dvc_var(ASC_DVC_VAR *);
STATIC void         asc_prt_asc_scsi_q(ASC_SCSI_Q *);
STATIC void         asc_prt_asc_qdone_info(ASC_QDONE_INFO *);
STATIC void         asc_prt_adv_dvc_cfg(ADV_DVC_CFG *);
STATIC void         asc_prt_adv_dvc_var(ADV_DVC_VAR *);
STATIC void         asc_prt_adv_scsi_req_q(ADV_SCSI_REQ_Q *);
STATIC void         asc_prt_adv_sgblock(int, ADV_SG_BLOCK *);
STATIC void         asc_prt_hex(char *f, uchar *, int);
#endif
#ifdef ADVANSYS_ASSERT
STATIC int             interrupts_enabled(void);
#endif
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
int
advansys_proc_info(char *buffer, char **start, off_t offset, int length,
int hostno, int inout)
{
struct Scsi_Host    *shp;
asc_board_t         *boardp;
int                 i;
char                *cp;
int                 cplen;
int                 cnt;
int                 totcnt;
int                 leftlen;
char                *curbuf;
off_t               advoffset;
Scsi_Device         *scd;
ASC_DBG(1, "advansys_proc_info: begin\n");
if (inout == TRUE) {
return(-ENOSYS);
}
for (i = 0; i < asc_board_count; i++) {
if (asc_host[i]->host_no == hostno) {
break;
}
}
if (i == asc_board_count) {
return(-ENOENT);
}
shp = asc_host[i];
boardp = ASC_BOARDP(shp);
*start = buffer;
curbuf = buffer;
advoffset = 0;
totcnt = 0;
leftlen = length;
cp = (char *) advansys_info(shp);
strcat(cp, "\n");
cplen = strlen(cp);
cnt = asc_proc_copy(advoffset, offset, curbuf, leftlen, cp, cplen);
totcnt += cnt;
leftlen -= cnt;
if (leftlen == 0) {
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
advoffset += cplen;
curbuf += cnt;
if (ASC_WIDE_BOARD(boardp)) {
cp = boardp->prtbuf;
cplen = asc_prt_adv_bios(shp, cp, ASC_PRTBUF_SIZE);
ASC_ASSERT(cplen < ASC_PRTBUF_SIZE);
cnt = asc_proc_copy(advoffset, offset, curbuf, leftlen, cp, cplen);
totcnt += cnt;
leftlen -= cnt;
if (leftlen == 0) {
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
advoffset += cplen;
curbuf += cnt;
}
cp = boardp->prtbuf;
cplen = asc_prt_board_devices(shp, cp, ASC_PRTBUF_SIZE);
ASC_ASSERT(cplen < ASC_PRTBUF_SIZE);
cnt = asc_proc_copy(advoffset, offset, curbuf, leftlen, cp, cplen);
totcnt += cnt;
leftlen -= cnt;
if (leftlen == 0) {
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
advoffset += cplen;
curbuf += cnt;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,75)
for (scd = scsi_devices; scd; scd = scd->next)
#else
for (scd = shp->host_queue; scd; scd = scd->next)
#endif
{
if (scd->host == shp) {
cp = boardp->prtbuf;
proc_print_scsidevice(scd, cp, &cplen, 0);
ASC_ASSERT(cplen < ASC_PRTBUF_SIZE);
cnt = asc_proc_copy(advoffset, offset, curbuf, leftlen, cp, cplen);
totcnt += cnt;
leftlen -= cnt;
if (leftlen == 0) {
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
advoffset += cplen;
curbuf += cnt;
}
}
cp = boardp->prtbuf;
if (ASC_NARROW_BOARD(boardp)) {
cplen = asc_prt_asc_board_eeprom(shp, cp, ASC_PRTBUF_SIZE);
} else {
cplen = asc_prt_adv_board_eeprom(shp, cp, ASC_PRTBUF_SIZE);
}
ASC_ASSERT(cplen < ASC_PRTBUF_SIZE);
cnt = asc_proc_copy(advoffset, offset, curbuf, leftlen, cp, cplen);
totcnt += cnt;
leftlen -= cnt;
if (leftlen == 0) {
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
advoffset += cplen;
curbuf += cnt;
cp = boardp->prtbuf;
cplen = asc_prt_driver_conf(shp, cp, ASC_PRTBUF_SIZE);
ASC_ASSERT(cplen < ASC_PRTBUF_SIZE);
cnt = asc_proc_copy(advoffset, offset, curbuf, leftlen, cp, cplen);
totcnt += cnt;
leftlen -= cnt;
if (leftlen == 0) {
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
advoffset += cplen;
curbuf += cnt;
#ifdef ADVANSYS_STATS
cp = boardp->prtbuf;
cplen = asc_prt_board_stats(shp, cp, ASC_PRTBUF_SIZE);
ASC_ASSERT(cplen < ASC_PRTBUF_SIZE);
cnt = asc_proc_copy(advoffset, offset, curbuf, leftlen, cp, cplen);
totcnt += cnt;
leftlen -= cnt;
if (leftlen == 0) {
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
advoffset += cplen;
curbuf += cnt;
#endif
cp = boardp->prtbuf;
if (ASC_NARROW_BOARD(boardp)) {
cplen = asc_prt_asc_board_info(shp, cp, ASC_PRTBUF_SIZE);
} else {
cplen = asc_prt_adv_board_info(shp, cp, ASC_PRTBUF_SIZE);
}
ASC_ASSERT(cplen < ASC_PRTBUF_SIZE);
cnt = asc_proc_copy(advoffset, offset, curbuf, leftlen, cp, cplen);
totcnt += cnt;
leftlen -= cnt;
if (leftlen == 0) {
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
advoffset += cplen;
curbuf += cnt;
ASC_DBG1(1, "advansys_proc_info: totcnt %d\n", totcnt);
return totcnt;
}
#endif
ASC_INITFUNC(
int
advansys_detect(Scsi_Host_Template *tpnt)
)
{
static int          detect_called = ASC_FALSE;
int                 iop;
int                 bus;
struct Scsi_Host    *shp;
asc_board_t         *boardp;
ASC_DVC_VAR         *asc_dvc_varp = NULL;
ADV_DVC_VAR         *adv_dvc_varp = NULL;
int                 ioport = 0;
int                 share_irq = FALSE;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
PCI_DEVICE          pciDevice;
PCI_CONFIG_SPACE    pciConfig;
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
unsigned long       pci_memory_address;
#endif
#endif
#else
#ifdef CONFIG_PCI
struct pci_dev      *pci_devp = NULL;
int                 pci_device_id_cnt = 0;
unsigned int        pci_device_id[ASC_PCI_DEVICE_ID_CNT] = {
ASC_PCI_DEVICE_ID_1100,
ASC_PCI_DEVICE_ID_1200,
ASC_PCI_DEVICE_ID_1300,
ASC_PCI_DEVICE_ID_2300
};
unsigned long       pci_memory_address;
#endif
#endif
int                 warn_code, err_code;
int                 ret;
if (detect_called == ASC_FALSE) {
detect_called = ASC_TRUE;
} else {
printk("AdvanSys SCSI: advansys_detect() multiple calls ignored\n");
return 0;
}
ASC_DBG(1, "advansys_detect: begin\n");
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
tpnt->proc_dir = &proc_scsi_advansys;
#endif
asc_board_count = 0;
if (asc_iopflag == ASC_TRUE) {
for (ioport = 0; ioport < ASC_NUM_IOPORT_PROBE; ioport++) {
ASC_DBG2(1, "advansys_detect: asc_ioport[%d] %x\n",
ioport, asc_ioport[ioport]);
if (asc_ioport[ioport] != 0) {
for (iop = 0; iop < ASC_IOADR_TABLE_MAX_IX; iop++) {
if (_asc_def_iop_base[iop] == asc_ioport[ioport]) {
break;
}
}
if (iop == ASC_IOADR_TABLE_MAX_IX) {
printk(
"AdvanSys SCSI: specified I/O Port 0x%X is invalid\n",
asc_ioport[ioport]);
asc_ioport[ioport] = 0;
}
}
}
ioport = 0;
}
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
memset(&pciDevice, 0, sizeof(PCI_DEVICE));
memset(&pciConfig, 0, sizeof(PCI_CONFIG_SPACE));
pciDevice.maxBusNumber = PCI_MAX_BUS;
pciDevice.endSlot = PCI_MAX_SLOT;
#endif
#endif
for (bus = 0; bus < ASC_NUM_BUS; bus++) {
ASC_DBG2(1, "advansys_detect: bus search type %d (%s)\n",
bus, asc_bus_name[bus]);
iop = 0;
while (asc_board_count < ASC_NUM_BOARD_SUPPORTED) {
ASC_DBG1(2, "advansys_detect: asc_board_count %d\n",
asc_board_count);
switch (asc_bus[bus]) {
case ASC_IS_ISA:
case ASC_IS_VL:
if (asc_iopflag == ASC_FALSE) {
iop = AscSearchIOPortAddr(iop, asc_bus[bus]);
} else {
ASC_DBG(1, "advansys_detect: I/O port scanning modified\n");
ioport_try_again:
iop = 0;
for (; ioport < ASC_NUM_IOPORT_PROBE; ioport++) {
if ((iop = asc_ioport[ioport]) != 0) {
break;
}
}
if (iop) {
ASC_DBG1(1, "advansys_detect: probing I/O port %x...\n",
iop);
if (check_region(iop, ASC_IOADR_GAP) != 0) {
printk(
"AdvanSys SCSI: specified I/O Port 0x%X is busy\n", iop);
asc_ioport[ioport] = 0;
goto ioport_try_again;
} else if (AscFindSignature(iop) == ASC_FALSE) {
printk(
"AdvanSys SCSI: specified I/O Port 0x%X has no adapter\n", iop);
asc_ioport[ioport] = 0;
goto ioport_try_again;
} else {
if (asc_bus[bus] == ASC_IS_ISA &&
(AscGetChipVersion(iop, ASC_IS_ISA) &
ASC_CHIP_VER_ISA_BIT) == 0) {
ioport++;
goto ioport_try_again;
}
}
asc_ioport[ioport++] = 0;
}
}
break;
case ASC_IS_EISA:
iop = AscSearchIOPortAddr(iop, asc_bus[bus]);
break;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
case ASC_IS_PCI:
if (asc_srch_pci_dev(&pciDevice) != PCI_DEVICE_FOUND) {
iop = 0;
} else {
ASC_DBG2(2,
"advansys_detect: slotFound %d, busNumber %d\n",
pciDevice.slotFound, pciDevice.busNumber);
asc_get_pci_cfg(&pciDevice, &pciConfig);
iop = pciConfig.baseAddress[0] & PCI_IOADDRESS_MASK;
ASC_DBG2(1,
"advansys_detect: vendorID %X, deviceID %X\n",
pciConfig.vendorID, pciConfig.deviceID);
ASC_DBG2(2, "advansys_detect: iop %X, irqLine %d\n",
iop, pciConfig.irqLine);
}
break;
#endif
#else
#ifdef CONFIG_PCI
case ASC_IS_PCI:
while (pci_device_id_cnt < ASC_PCI_DEVICE_ID_CNT) {
if ((pci_devp = pci_find_device(ASC_PCI_VENDORID,
pci_device_id[pci_device_id_cnt], pci_devp)) == NULL) {
pci_device_id_cnt++;
} else {
break;
}
}
if (pci_devp == NULL) {
iop = 0;
} else {
ASC_DBG2(2,
"advansys_detect: devfn %d, bus number %d\n",
pci_devp->devfn, pci_devp->bus->number);
iop = pci_devp->base_address[0] & PCI_IOADDRESS_MASK;
ASC_DBG2(1,
"advansys_detect: vendorID %X, deviceID %X\n",
pci_devp->vendor, pci_devp->device);
ASC_DBG2(2, "advansys_detect: iop %X, irqLine %d\n",
iop, pci_devp->irq);
}
break;
#endif
#endif
default:
ASC_PRINT1("advansys_detect: unknown bus type: %d\n",
asc_bus[bus]);
break;
}
ASC_DBG1(1, "advansys_detect: iop %x\n", iop);
if (iop == 0) {
break;
}
ASC_DBG(2, "advansys_detect: scsi_register()\n");
shp = scsi_register(tpnt, sizeof(asc_board_t));
asc_host[asc_board_count++] = shp;
boardp = ASC_BOARDP(shp);
memset(boardp, 0, sizeof(asc_board_t));
boardp->id = asc_board_count - 1;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
if (asc_bus[bus] == ASC_IS_PCI &&
pciConfig.deviceID == ASC_PCI_DEVICE_ID_2300) {
boardp->flags |= ASC_IS_WIDE_BOARD;
}
#endif
#else
#ifdef CONFIG_PCI
if (asc_bus[bus] == ASC_IS_PCI &&
pci_devp->device == ASC_PCI_DEVICE_ID_2300) {
boardp->flags |= ASC_IS_WIDE_BOARD;
}
#endif
#endif
if (ASC_NARROW_BOARD(boardp)) {
ASC_DBG(1, "advansys_detect: narrow board\n");
asc_dvc_varp = &boardp->dvc_var.asc_dvc_var;
asc_dvc_varp->bus_type = asc_bus[bus];
asc_dvc_varp->drv_ptr = (ulong) boardp;
asc_dvc_varp->cfg = &boardp->dvc_cfg.asc_dvc_cfg;
asc_dvc_varp->cfg->overrun_buf = &overrun_buf[0];
asc_dvc_varp->iop_base = iop;
asc_dvc_varp->isr_callback = (Ptr2Func) asc_isr_callback;
} else {
ASC_DBG(1, "advansys_detect: wide board\n");
adv_dvc_varp = &boardp->dvc_var.adv_dvc_var;
adv_dvc_varp->drv_ptr = (ulong) boardp;
adv_dvc_varp->cfg = &boardp->dvc_cfg.adv_dvc_cfg;
adv_dvc_varp->isr_callback = (Ptr2Func) adv_isr_callback;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
adv_dvc_varp->iop_base = iop;
#else
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
pci_memory_address = pciConfig.baseAddress[1];
if ((boardp->ioremap_addr =
ioremap(pci_memory_address & PAGE_MASK,
PAGE_SIZE)) == 0) {
ASC_PRINT3(
"advansys_detect: board %d: ioremap(%lx, %d) returned NULL\n",
boardp->id, pci_memory_address, ADV_CONDOR_IOLEN);
scsi_unregister(shp);
asc_board_count--;
continue;
}
adv_dvc_varp->iop_base = (AdvPortAddr)
(boardp->ioremap_addr +
(pci_memory_address - (pci_memory_address & PAGE_MASK)));
#endif
#else
#ifdef CONFIG_PCI
pci_memory_address = pci_devp->base_address[1];
if ((boardp->ioremap_addr =
ioremap(pci_memory_address & PAGE_MASK,
PAGE_SIZE)) == 0) {
ASC_PRINT3(
"advansys_detect: board %d: ioremap(%lx, %d) returned NULL\n",
boardp->id, pci_memory_address, ADV_CONDOR_IOLEN);
scsi_unregister(shp);
asc_board_count--;
continue;
}
adv_dvc_varp->iop_base = (AdvPortAddr)
(boardp->ioremap_addr +
(pci_memory_address - (pci_memory_address & PAGE_MASK)));
#endif
#endif
#endif
boardp->ioport = iop;
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
if ((boardp->prtbuf =
kmalloc(ASC_PRTBUF_SIZE, GFP_ATOMIC)) == NULL) {
ASC_PRINT3(
"advansys_detect: board %d: kmalloc(%d, %d) returned NULL\n",
boardp->id, ASC_PRTBUF_SIZE, GFP_ATOMIC);
scsi_unregister(shp);
asc_board_count--;
continue;
}
#endif
if (ASC_NARROW_BOARD(boardp)) {
switch (asc_dvc_varp->bus_type) {
case ASC_IS_ISA:
shp->unchecked_isa_dma = TRUE;
share_irq = FALSE;
break;
case ASC_IS_VL:
shp->unchecked_isa_dma = FALSE;
share_irq = FALSE;
break;
case ASC_IS_EISA:
shp->unchecked_isa_dma = FALSE;
share_irq = TRUE;
break;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
case ASC_IS_PCI:
shp->irq = asc_dvc_varp->irq_no = pciConfig.irqLine;
asc_dvc_varp->cfg->pci_device_id = pciConfig.deviceID;
asc_dvc_varp->cfg->pci_slot_info =
ASC_PCI_MKID(pciDevice.busNumber,
pciDevice.slotFound,
pciDevice.devFunc);
shp->unchecked_isa_dma = FALSE;
share_irq = TRUE;
break;
#endif
#else
#ifdef CONFIG_PCI
case ASC_IS_PCI:
shp->irq = asc_dvc_varp->irq_no = pci_devp->irq;
asc_dvc_varp->cfg->pci_device_id = pci_devp->device;
asc_dvc_varp->cfg->pci_slot_info =
ASC_PCI_MKID(pci_devp->bus->number,
PCI_SLOT(pci_devp->devfn),
PCI_FUNC(pci_devp->devfn));
shp->unchecked_isa_dma = FALSE;
share_irq = TRUE;
break;
#endif
#endif
default:
ASC_PRINT2(
"advansys_detect: board %d: unknown adapter type: %d\n",
boardp->id, asc_dvc_varp->bus_type);
shp->unchecked_isa_dma = TRUE;
share_irq = FALSE;
break;
}
} else {
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
shp->irq = adv_dvc_varp->irq_no = pciConfig.irqLine;
adv_dvc_varp->cfg->pci_device_id = pciConfig.deviceID;
adv_dvc_varp->cfg->pci_slot_info =
ASC_PCI_MKID(pciDevice.busNumber,
pciDevice.slotFound,
pciDevice.devFunc);
shp->unchecked_isa_dma = FALSE;
share_irq = TRUE;
#endif
#else
#ifdef CONFIG_PCI
shp->irq = adv_dvc_varp->irq_no = pci_devp->irq;
adv_dvc_varp->cfg->pci_device_id = pci_devp->device;
adv_dvc_varp->cfg->pci_slot_info =
ASC_PCI_MKID(pci_devp->bus->number,
PCI_SLOT(pci_devp->devfn),
PCI_FUNC(pci_devp->devfn));
shp->unchecked_isa_dma = FALSE;
share_irq = TRUE;
#endif
#endif
}
if (ASC_NARROW_BOARD(boardp)) {
ASC_DBG(2, "advansys_detect: AscInitGetConfig()\n");
switch(ret = AscInitGetConfig(asc_dvc_varp)) {
case 0:
break;
case ASC_WARN_IO_PORT_ROTATE:
ASC_PRINT1(
"AscInitGetConfig: board %d: I/O port address modified\n",
boardp->id);
break;
case ASC_WARN_AUTO_CONFIG:
ASC_PRINT1(
"AscInitGetConfig: board %d: I/O port increment switch enabled\n",
boardp->id);
break;
case ASC_WARN_EEPROM_CHKSUM:
ASC_PRINT1(
"AscInitGetConfig: board %d: EEPROM checksum error\n",
boardp->id);
break;
case ASC_WARN_IRQ_MODIFIED:
ASC_PRINT1(
"AscInitGetConfig: board %d: IRQ modified\n",
boardp->id);
break;
case ASC_WARN_CMD_QNG_CONFLICT:
ASC_PRINT1(
"AscInitGetConfig: board %d: tag queuing enabled w/o disconnects\n",
boardp->id);
break;
default:
ASC_PRINT2(
"AscInitGetConfig: board %d: unknown warning: %x\n",
boardp->id, ret);
break;
}
if ((err_code = asc_dvc_varp->err_code) != 0) {
ASC_PRINT3(
"AscInitGetConfig: board %d error: init_state %x, err_code %x\n",
boardp->id, asc_dvc_varp->init_state,
asc_dvc_varp->err_code);
}
} else {
ASC_DBG(2, "advansys_detect: AdvInitGetConfig()\n");
if ((ret = AdvInitGetConfig(adv_dvc_varp)) != 0) {
ASC_PRINT2("AdvInitGetConfig: board %d: warning: %x\n",
boardp->id, ret);
}
if ((err_code = adv_dvc_varp->err_code) != 0) {
ASC_PRINT2(
"AdvInitGetConfig: board %d error: err_code %x\n",
boardp->id, adv_dvc_varp->err_code);
}
}
if (err_code != 0) {
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
kfree(boardp->prtbuf);
#endif
scsi_unregister(shp);
asc_board_count--;
continue;
}
if (ASC_NARROW_BOARD(boardp)) {
ASCEEP_CONFIG *ep;
boardp->init_tidmask |=
ADV_TID_TO_TIDMASK(asc_dvc_varp->cfg->chip_scsi_id);
ep = &boardp->eep_config.asc_eep;
ep->init_sdtr = asc_dvc_varp->cfg->sdtr_enable;
ep->disc_enable = asc_dvc_varp->cfg->disc_enable;
ep->use_cmd_qng = asc_dvc_varp->cfg->cmd_qng_enabled;
ep->isa_dma_speed = asc_dvc_varp->cfg->isa_dma_speed;
ep->start_motor = asc_dvc_varp->start_motor;
ep->cntl = asc_dvc_varp->dvc_cntl;
ep->no_scam = asc_dvc_varp->no_scam;
ep->max_total_qng = asc_dvc_varp->max_total_qng;
ep->chip_scsi_id = asc_dvc_varp->cfg->chip_scsi_id;
ep->max_tag_qng = asc_dvc_varp->cfg->max_tag_qng[0];
ep->adapter_info[0] = asc_dvc_varp->cfg->adapter_info[0];
ep->adapter_info[1] = asc_dvc_varp->cfg->adapter_info[1];
ep->adapter_info[2] = asc_dvc_varp->cfg->adapter_info[2];
ep->adapter_info[3] = asc_dvc_varp->cfg->adapter_info[3];
ep->adapter_info[4] = asc_dvc_varp->cfg->adapter_info[4];
ep->adapter_info[5] = asc_dvc_varp->cfg->adapter_info[5];
ASC_DBG(2, "advansys_detect: AscInitSetConfig()\n");
switch (ret = AscInitSetConfig(asc_dvc_varp)) {
case 0:
break;
case ASC_WARN_IO_PORT_ROTATE:
ASC_PRINT1(
"AscInitSetConfig: board %d: I/O port address modified\n",
boardp->id);
break;
case ASC_WARN_AUTO_CONFIG:
ASC_PRINT1(
"AscInitSetConfig: board %d: I/O port increment switch enabled\n",
boardp->id);
break;
case ASC_WARN_EEPROM_CHKSUM:
ASC_PRINT1(
"AscInitSetConfig: board %d: EEPROM checksum error\n",
boardp->id);
break;
case ASC_WARN_IRQ_MODIFIED:
ASC_PRINT1(
"AscInitSetConfig: board %d: IRQ modified\n",
boardp->id);
break;
case ASC_WARN_CMD_QNG_CONFLICT:
ASC_PRINT1(
"AscInitSetConfig: board %d: tag queuing w/o disconnects\n",
boardp->id);
break;
default:
ASC_PRINT2(
"AscInitSetConfig: board %d: unknown warning: %x\n",
boardp->id, ret);
break;
}
if (asc_dvc_varp->err_code != 0) {
ASC_PRINT3(
"AscInitSetConfig: board %d error: init_state %x, err_code %x\n",
boardp->id, asc_dvc_varp->init_state,
asc_dvc_varp->err_code);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
kfree(boardp->prtbuf);
#endif
scsi_unregister(shp);
asc_board_count--;
continue;
}
if ((asc_dvc_varp->bus_type & ASC_IS_PCI) == 0) {
shp->irq = asc_dvc_varp->irq_no;
}
} else {
ADVEEP_CONFIG *ep;
ep = &boardp->eep_config.adv_eep;
ep->adapter_scsi_id = adv_dvc_varp->chip_scsi_id;
ep->max_host_qng = adv_dvc_varp->max_host_qng;
ep->max_dvc_qng = adv_dvc_varp->max_dvc_qng;
ep->termination = adv_dvc_varp->cfg->termination;
ep->disc_enable = adv_dvc_varp->cfg->disc_enable;
ep->bios_ctrl = adv_dvc_varp->bios_ctrl;
ep->wdtr_able = adv_dvc_varp->wdtr_able;
ep->sdtr_able = adv_dvc_varp->sdtr_able;
ep->ultra_able = adv_dvc_varp->ultra_able;
ep->tagqng_able = adv_dvc_varp->tagqng_able;
ep->start_motor = adv_dvc_varp->start_motor;
ep->scsi_reset_delay = adv_dvc_varp->scsi_reset_wait;
ep->bios_boot_delay = adv_dvc_varp->cfg->bios_boot_wait;
ep->serial_number_word1 = adv_dvc_varp->cfg->serial1;
ep->serial_number_word2 = adv_dvc_varp->cfg->serial2;
ep->serial_number_word3 = adv_dvc_varp->cfg->serial3;
boardp->init_tidmask |=
ADV_TID_TO_TIDMASK(adv_dvc_varp->chip_scsi_id);
shp->irq = adv_dvc_varp->irq_no;
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
shp->max_channel = 0;
#endif
if (ASC_NARROW_BOARD(boardp)) {
shp->max_id = ASC_MAX_TID + 1;
shp->max_lun = ASC_MAX_LUN + 1;
shp->io_port = asc_dvc_varp->iop_base;
shp->n_io_port = ASC_IOADR_GAP;
shp->this_id = asc_dvc_varp->cfg->chip_scsi_id;
shp->can_queue = asc_dvc_varp->max_total_qng;
} else {
shp->max_id = ADV_MAX_TID + 1;
shp->max_lun = ADV_MAX_LUN + 1;
shp->io_port = iop;
shp->n_io_port = ADV_CONDOR_IOLEN;
shp->this_id = adv_dvc_varp->chip_scsi_id;
shp->can_queue = adv_dvc_varp->max_host_qng;
}
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,89)
#ifdef MODULE
shp->cmd_per_lun = 1;
#else
shp->cmd_per_lun = 4;
#endif
ASC_DBG1(1, "advansys_detect: cmd_per_lun: %d\n", shp->cmd_per_lun);
#else
#ifdef MODULE
shp->cmd_per_lun = 1;
#else
shp->cmd_per_lun = 0;
#endif
shp->select_queue_depths = advansys_select_queue_depths;
#endif
if (ASC_NARROW_BOARD(boardp)) {
shp->sg_tablesize =
(((asc_dvc_varp->max_total_qng - 2) / 2) *
ASC_SG_LIST_PER_Q) + 1;
} else {
shp->sg_tablesize = ADV_MAX_SG_LIST;
}
#ifdef MODULE
if (shp->sg_tablesize > 64) {
shp->sg_tablesize = 64;
}
#endif
if (shp->sg_tablesize > SG_ALL) {
shp->sg_tablesize = SG_ALL;
}
ASC_DBG1(1, "advansys_detect: sg_tablesize: %d\n",
shp->sg_tablesize);
if (ASC_NARROW_BOARD(boardp)) {
shp->base = (char *) ((ulong) AscGetChipBiosAddress(
asc_dvc_varp->iop_base,
asc_dvc_varp->bus_type));
} else {
AdvReadWordLram(adv_dvc_varp->iop_base, BIOS_SIGNATURE,
boardp->bios_signature);
AdvReadWordLram(adv_dvc_varp->iop_base, BIOS_VERSION,
boardp->bios_version);
AdvReadWordLram(adv_dvc_varp->iop_base, BIOS_CODESEG,
boardp->bios_codeseg);
AdvReadWordLram(adv_dvc_varp->iop_base, BIOS_CODELEN,
boardp->bios_codelen);
ASC_DBG2(1,
"advansys_detect: bios_signature %x, bios_version %x\n",
boardp->bios_signature, boardp->bios_version);
ASC_DBG2(1,
"advansys_detect: bios_codeseg %x, bios_codelen %x\n",
boardp->bios_codeseg, boardp->bios_codelen);
if (boardp->bios_signature == 0x55AA) {
shp->base = (uchar *) (boardp->bios_codeseg << 4);
} else {
shp->base = 0;
}
}
ASC_DBG(2, "advansys_detect: request_region()\n");
request_region(shp->io_port, shp->n_io_port, "advansys");
shp->dma_channel = NO_ISA_DMA;
if (ASC_NARROW_BOARD(boardp)) {
if (asc_dvc_varp->bus_type & ASC_IS_ISA) {
shp->dma_channel = asc_dvc_varp->cfg->isa_dma_channel;
if ((ret =
request_dma(shp->dma_channel, "advansys")) != 0) {
ASC_PRINT3(
"advansys_detect: board %d: request_dma() %d failed %d\n",
boardp->id, shp->dma_channel, ret);
release_region(shp->io_port, shp->n_io_port);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
kfree(boardp->prtbuf);
#endif
scsi_unregister(shp);
asc_board_count--;
continue;
}
AscEnableIsaDma(shp->dma_channel);
}
}
ASC_DBG1(2, "advansys_detect: request_irq() %d\n", shp->irq);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,70)
if ((ret = request_irq(shp->irq, advansys_interrupt,
SA_INTERRUPT, "advansys")) != 0)
#else
if (((ret = request_irq(shp->irq, advansys_interrupt,
SA_INTERRUPT | (share_irq == TRUE ? SA_SHIRQ : 0),
"advansys", boardp)) != 0) &&
((ret = request_irq(shp->irq, advansys_interrupt,
(share_irq == TRUE ? SA_SHIRQ : 0),
"advansys", boardp)) != 0))
#endif
{
if (ret == -EBUSY) {
ASC_PRINT2(
"advansys_detect: board %d: request_irq(): IRQ %d already in use.\n",
boardp->id, shp->irq);
} else if (ret == -EINVAL) {
ASC_PRINT2(
"advansys_detect: board %d: request_irq(): IRQ %d not valid.\n",
boardp->id, shp->irq);
} else {
ASC_PRINT3(
"advansys_detect: board %d: request_irq(): IRQ %d failed with %d\n",
boardp->id, shp->irq, ret);
}
release_region(shp->io_port, shp->n_io_port);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
iounmap(boardp->ioremap_addr);
#endif
if (shp->dma_channel != NO_ISA_DMA) {
free_dma(shp->dma_channel);
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
kfree(boardp->prtbuf);
#endif
scsi_unregister(shp);
asc_board_count--;
continue;
}
if (ASC_NARROW_BOARD(boardp)) {
ASC_DBG(2, "advansys_detect: AscInitAsc1000Driver()\n");
warn_code = AscInitAsc1000Driver(asc_dvc_varp);
err_code = asc_dvc_varp->err_code;
if (warn_code || err_code) {
ASC_PRINT4(
"AscInitAsc1000Driver: board %d: error: init_state %x, warn %x error %x\n",
boardp->id, asc_dvc_varp->init_state,
warn_code, err_code);
}
} else {
int             req_cnt;
adv_req_t       *reqp = NULL;
int             sg_cnt;
adv_sgblk_t     *sgp = NULL;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
req_cnt = sizeof(adv_req_buf)/sizeof(adv_req_t);
sg_cnt = sizeof(adv_sgblk_buf)/sizeof(adv_sgblk_t);
reqp = (adv_req_t *) &adv_req_buf[0];
sgp = (adv_sgblk_t *) &adv_sgblk_buf[0];
#else
for (req_cnt = adv_dvc_varp->max_host_qng;
req_cnt > 0; req_cnt--) {
reqp = (adv_req_t *)
kmalloc(sizeof(adv_req_t) * req_cnt, GFP_ATOMIC);
ASC_DBG3(1,
"advansys_detect: reqp %x, req_cnt %d, bytes %d\n",
(unsigned) reqp, req_cnt, sizeof(adv_req_t) * req_cnt);
if (reqp != NULL) {
break;
}
}
for (sg_cnt = ADV_TOT_SG_LIST; sg_cnt > 0; sg_cnt--) {
sgp = (adv_sgblk_t *)
kmalloc(sizeof(adv_sgblk_t) * sg_cnt, GFP_ATOMIC);
ASC_DBG3(1,
"advansys_detect: sgp %x, sg_cnt %d, bytes %d\n",
(unsigned) sgp, sg_cnt, sizeof(adv_sgblk_t) * sg_cnt);
if (sgp != NULL) {
break;
}
}
#endif
if (reqp == NULL) {
ASC_PRINT1(
"advansys_detect: board %d: error: failed to kmalloc() adv_req_t buffer.\n",
boardp->id);
err_code = ADV_ERROR;
} else if (sgp == NULL) {
kfree(reqp);
ASC_PRINT1(
"advansys_detect: board %d: error: failed to kmalloc() adv_sgblk_t buffer.\n",
boardp->id);
err_code = ADV_ERROR;
} else {
boardp->orig_reqp = reqp;
req_cnt--;
reqp[req_cnt].next_reqp = NULL;
for (; req_cnt > 0; req_cnt--) {
reqp[req_cnt - 1].next_reqp = &reqp[req_cnt];
}
boardp->adv_reqp = &reqp[0];
boardp->orig_sgblkp = sgp;
sg_cnt--;
sgp[sg_cnt].next_sgblkp = NULL;
for (; sg_cnt > 0; sg_cnt--) {
sgp[sg_cnt - 1].next_sgblkp = &sgp[sg_cnt];
}
boardp->adv_sgblkp = &sgp[0];
ASC_DBG(2, "advansys_detect: AdvInitAsc3550Driver()\n");
warn_code = AdvInitAsc3550Driver(adv_dvc_varp);
err_code = adv_dvc_varp->err_code;
if (warn_code || err_code) {
ASC_PRINT3(
"AdvInitAsc3550Driver: board %d: error: warn %x, error %x\n",
boardp->id, warn_code, adv_dvc_varp->err_code);
}
}
}
if (err_code != 0) {
release_region(shp->io_port, shp->n_io_port);
if (ASC_WIDE_BOARD(boardp)) {
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
iounmap(boardp->ioremap_addr);
#endif
if (boardp->orig_reqp) {
kfree(boardp->orig_reqp);
boardp->orig_reqp = boardp->adv_reqp = NULL;
}
if (boardp->orig_sgblkp) {
kfree(boardp->orig_sgblkp);
boardp->orig_sgblkp = boardp->adv_sgblkp = NULL;
}
}
if (shp->dma_channel != NO_ISA_DMA) {
free_dma(shp->dma_channel);
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
kfree(boardp->prtbuf);
#endif
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,70)
free_irq(shp->irq);
#else
free_irq(shp->irq, boardp);
#endif
scsi_unregister(shp);
asc_board_count--;
continue;
}
ASC_DBG_PRT_SCSI_HOST(2, shp);
}
}
ASC_DBG1(1, "advansys_detect: done: asc_board_count %d\n", asc_board_count);
return asc_board_count;
}
int
advansys_release(struct Scsi_Host *shp)
{
asc_board_t    *boardp;
ASC_DBG(1, "advansys_release: begin\n");
boardp = ASC_BOARDP(shp);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,70)
free_irq(shp->irq);
#else
free_irq(shp->irq, boardp);
#endif
if (shp->dma_channel != NO_ISA_DMA) {
ASC_DBG(1, "advansys_release: free_dma()\n");
free_dma(shp->dma_channel);
}
release_region(shp->io_port, shp->n_io_port);
if (ASC_WIDE_BOARD(boardp)) {
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
iounmap(boardp->ioremap_addr);
#endif
if (boardp->orig_reqp) {
kfree(boardp->orig_reqp);
boardp->orig_reqp = boardp->adv_reqp = NULL;
}
if (boardp->orig_sgblkp) {
kfree(boardp->orig_sgblkp);
boardp->orig_sgblkp = boardp->adv_sgblkp = NULL;
}
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
ASC_ASSERT(boardp->prtbuf != NULL);
kfree(boardp->prtbuf);
#endif
scsi_unregister(shp);
ASC_DBG(1, "advansys_release: end\n");
return 0;
}
const char *
advansys_info(struct Scsi_Host *shp)
{
static char     info[ASC_INFO_SIZE];
asc_board_t     *boardp;
ASC_DVC_VAR     *asc_dvc_varp;
ADV_DVC_VAR     *adv_dvc_varp;
char            *busname;
boardp = ASC_BOARDP(shp);
if (ASC_NARROW_BOARD(boardp)) {
asc_dvc_varp = &boardp->dvc_var.asc_dvc_var;
ASC_DBG(1, "advansys_info: begin\n");
if (asc_dvc_varp->bus_type & ASC_IS_ISA) {
if ((asc_dvc_varp->bus_type & ASC_IS_ISAPNP) == ASC_IS_ISAPNP) {
busname = "ISA PnP";
} else {
busname = "ISA";
}
sprintf(info,
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,92)
"AdvanSys SCSI %s: %s %u CDB: BIOS %X, IO %X/%X, IRQ %u, DMA %u",
#else
"AdvanSys SCSI %s: %s %u CDB: BIOS %X, IO %lX/%X, IRQ %u, DMA %u",
#endif
ASC_VERSION, busname, asc_dvc_varp->max_total_qng,
(unsigned) shp->base,
shp->io_port, shp->n_io_port - 1,
shp->irq, shp->dma_channel);
} else if (asc_dvc_varp->bus_type & ASC_IS_PCI) {
if ((asc_dvc_varp->bus_type & ASC_IS_PCI_ULTRA)
== ASC_IS_PCI_ULTRA) {
busname = "PCI Ultra";
} else {
busname = "PCI";
}
sprintf(info,
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,92)
"AdvanSys SCSI %s: %s %u CDB: IO %X/%X, IRQ %u",
#else
"AdvanSys SCSI %s: %s %u CDB: IO %lX/%X, IRQ %u",
#endif
ASC_VERSION, busname, asc_dvc_varp->max_total_qng,
shp->io_port, shp->n_io_port - 1, shp->irq);
} else {
if (asc_dvc_varp->bus_type & ASC_IS_VL) {
busname = "VL";
} else if (asc_dvc_varp->bus_type & ASC_IS_EISA) {
busname = "EISA";
} else {
busname = "?";
ASC_PRINT2(
"advansys_info: board %d: unknown bus type %d\n",
boardp->id, asc_dvc_varp->bus_type);
}
sprintf(info,
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,92)
"AdvanSys SCSI %s: %s %u CDB: BIOS %X, IO %X/%X, IRQ %u",
#else
"AdvanSys SCSI %s: %s %u CDB: BIOS %X, IO %lX/%X, IRQ %u",
#endif
ASC_VERSION, busname, asc_dvc_varp->max_total_qng,
(unsigned) shp->base, shp->io_port - 1,
shp->n_io_port, shp->irq);
}
} else {
adv_dvc_varp = &boardp->dvc_var.adv_dvc_var;
if (boardp->bios_signature == 0x55AA) {
sprintf(info,
"AdvanSys SCSI %s: PCI Ultra-Wide: BIOS %X/%X, IO %X/%X, IRQ %u",
ASC_VERSION,
boardp->bios_codeseg << 4,
boardp->bios_codelen > 0 ?
(boardp->bios_codelen << 9) - 1 : 0,
(unsigned) boardp->ioport, ADV_CONDOR_IOLEN - 1,
shp->irq);
} else {
sprintf(info,
"AdvanSys SCSI %s: PCI Ultra-Wide: IO %X/%X, IRQ %u",
ASC_VERSION,
(unsigned) boardp->ioport,
(ADV_CONDOR_IOLEN - 1),
shp->irq);
}
}
ASC_ASSERT(strlen(info) < ASC_INFO_SIZE);
ASC_DBG(1, "advansys_info: end\n");
return info;
}
int
advansys_command(Scsi_Cmnd *scp)
{
ASC_DBG1(1, "advansys_command: scp %x\n", (unsigned) scp);
ASC_STATS(scp->host, command);
scp->SCp.Status = 0;
advansys_queuecommand(scp, advansys_command_done);
while (scp->SCp.Status == 0) {
continue;
}
ASC_DBG1(1, "advansys_command: result %x\n", scp->result);
return scp->result;
}
int
advansys_queuecommand(Scsi_Cmnd *scp, void (*done)(Scsi_Cmnd *))
{
struct Scsi_Host    *shp;
asc_board_t         *boardp;
long                flags;
Scsi_Cmnd           *done_scp;
shp = scp->host;
boardp = ASC_BOARDP(shp);
ASC_STATS(shp, queuecommand);
save_flags(flags);
cli();
if (boardp->flags & (ASC_HOST_IN_RESET | ASC_HOST_IN_ABORT)) {
if (boardp->flags & ASC_HOST_IN_RESET) {
ASC_DBG1(1,
"advansys_queuecommand: scp %x blocked for reset request\n",
(unsigned) scp);
scp->result = HOST_BYTE(DID_RESET);
} else {
ASC_DBG1(1,
"advansys_queuecommand: scp %x blocked for abort request\n",
(unsigned) scp);
scp->result = HOST_BYTE(DID_ABORT);
}
asc_enqueue(&boardp->done, scp, ASC_BACK);
restore_flags(flags);
return 0;
}
if (!ASC_QUEUE_EMPTY(&boardp->waiting)) {
ASC_DBG(1,
"advansys_queuecommand: before asc_execute_queue() waiting\n");
asc_execute_queue(&boardp->waiting);
}
scp->scsi_done = done;
switch (asc_execute_scsi_cmnd(scp)) {
case ASC_NOERROR:
break;
case ASC_BUSY:
asc_enqueue(&boardp->waiting, scp, ASC_BACK);
break;
case ASC_ERROR:
default:
done_scp = asc_dequeue_list(&boardp->done, NULL, ASC_TID_ALL);
asc_scsi_done_list(done_scp);
break;
}
restore_flags(flags);
return 0;
}
int
advansys_abort(Scsi_Cmnd *scp)
{
struct Scsi_Host    *shp;
asc_board_t         *boardp;
ASC_DVC_VAR         *asc_dvc_varp;
ADV_DVC_VAR         *adv_dvc_varp;
long                flags;
int                 do_scsi_done;
int                 scp_found;
Scsi_Cmnd           *done_scp = NULL;
int                 ret;
save_flags(flags);
cli();
ASC_DBG1(1, "advansys_abort: scp %x\n", (unsigned) scp);
#ifdef ADVANSYS_STATS
if (scp->host != NULL) {
ASC_STATS(scp->host, abort);
}
#endif
#ifdef ADVANSYS_ASSERT
do_scsi_done = ASC_ERROR;
scp_found = ASC_ERROR;
ret = ASC_ERROR;
#endif
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
if (scp->serial_number != scp->serial_number_at_timeout) {
ASC_PRINT1(
"advansys_abort: timeout serial number changed for request %x\n",
(unsigned) scp);
do_scsi_done = ASC_FALSE;
scp_found = ASC_FALSE;
ret = SCSI_ABORT_NOT_RUNNING;
} else
#endif
if ((shp = scp->host) == NULL) {
scp->result = HOST_BYTE(DID_ERROR);
do_scsi_done = ASC_TRUE;
scp_found = ASC_FALSE;
ret = SCSI_ABORT_ERROR;
} else if ((boardp = ASC_BOARDP(shp))->flags &
(ASC_HOST_IN_RESET | ASC_HOST_IN_ABORT)) {
ASC_PRINT2(
"advansys_abort: board %d: Nested host reset or abort, flags 0x%x\n",
boardp->id, boardp->flags);
do_scsi_done = ASC_TRUE;
if ((asc_rmqueue(&boardp->active, scp) == ASC_TRUE) ||
(asc_rmqueue(&boardp->waiting, scp) == ASC_TRUE)) {
scp_found = ASC_TRUE;
} else {
scp_found = ASC_FALSE;
}
scp->result = HOST_BYTE(DID_ERROR);
ret = SCSI_ABORT_ERROR;
} else {
boardp->flags |= ASC_HOST_IN_ABORT;
do_scsi_done = ASC_TRUE;
if (asc_rmqueue(&boardp->waiting, scp) == ASC_TRUE) {
ASC_DBG1(1, "advansys_abort: scp %x found on waiting queue\n",
(unsigned) scp);
scp_found = ASC_TRUE;
scp->result = HOST_BYTE(DID_ABORT);
ret = SCSI_ABORT_SUCCESS;
} else if (asc_isqueued(&boardp->active, scp) == ASC_TRUE) {
if (ASC_NARROW_BOARD(boardp)) {
asc_dvc_varp = &boardp->dvc_var.asc_dvc_var;
scp->result = HOST_BYTE(DID_ABORT);
sti();
ASC_DBG1(1, "advansys_abort: before AscAbortSRB(), scp %x\n",
(unsigned) scp);
switch (AscAbortSRB(asc_dvc_varp, (ulong) scp)) {
case ASC_TRUE:
ASC_DBG(1, "advansys_abort: AscAbortSRB() TRUE\n");
ret = SCSI_ABORT_PENDING;
break;
case ASC_FALSE:
ASC_DBG(1, "advansys_abort: AscAbortSRB() FALSE\n");
ret = SCSI_ABORT_NOT_RUNNING;
break;
case ASC_ERROR:
default:
ASC_DBG(1, "advansys_abort: AscAbortSRB() ERROR\n");
ret = SCSI_ABORT_ERROR;
break;
}
cli();
} else {
adv_dvc_varp = &boardp->dvc_var.adv_dvc_var;
scp->result = HOST_BYTE(DID_ABORT);
ASC_DBG1(1, "advansys_abort: before AdvAbortSRB(), scp %x\n",
(unsigned) scp);
switch (AdvAbortSRB(adv_dvc_varp, (ulong) scp)) {
case ASC_TRUE:
ASC_DBG(1, "advansys_abort: AdvAbortSRB() TRUE\n");
ret = SCSI_ABORT_PENDING;
break;
case ASC_FALSE:
ASC_DBG(1, "advansys_abort: AdvAbortSRB() FALSE\n");
ret = SCSI_ABORT_NOT_RUNNING;
break;
case ASC_ERROR:
default:
ASC_DBG(1, "advansys_abort: AdvAbortSRB() ERROR\n");
ret = SCSI_ABORT_ERROR;
break;
}
(void) AdvISR(adv_dvc_varp);
}
if (asc_rmqueue(&boardp->active, scp) == ASC_TRUE) {
scp->result = HOST_BYTE(DID_ABORT);
scp_found = ASC_TRUE;
} else {
scp_found = asc_rmqueue(&boardp->done, scp);
ASC_ASSERT(scp_found == ASC_TRUE);
}
} else {
do_scsi_done = ASC_TRUE;
scp_found = ASC_FALSE;
ret = SCSI_ABORT_NOT_RUNNING;
}
boardp->flags &= ~ASC_HOST_IN_ABORT;
done_scp = asc_dequeue_list(&boardp->done, NULL, ASC_TID_ALL);
if (!ASC_QUEUE_EMPTY(&boardp->waiting)) {
ASC_DBG(1, "advansys_interrupt: before asc_execute_queue()\n");
asc_execute_queue(&boardp->waiting);
}
}
ASC_ASSERT(do_scsi_done != ASC_ERROR);
ASC_ASSERT(scp_found != ASC_ERROR);
if (do_scsi_done == ASC_TRUE) {
if (scp->scsi_done == NULL) {
ASC_PRINT1(
"advansys_abort: aborted request scsi_done() is NULL, %x\n",
(unsigned) scp);
} else {
if (scp_found == ASC_FALSE) {
ASC_PRINT1(
"advansys_abort: abort request not active or waiting, completing anyway %x\n",
(unsigned) scp);
}
ASC_STATS(scp->host, done);
scp->scsi_done(scp);
}
}
if (done_scp != NULL) {
asc_scsi_done_list(done_scp);
}
ASC_DBG1(1, "advansys_abort: ret %d\n", ret);
restore_flags(flags);
ASC_ASSERT(ret != ASC_ERROR);
return ret;
}
int
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,89)
advansys_reset(Scsi_Cmnd *scp)
#else
advansys_reset(Scsi_Cmnd *scp, unsigned int reset_flags)
#endif
{
struct Scsi_Host     *shp;
asc_board_t          *boardp;
ASC_DVC_VAR          *asc_dvc_varp;
ADV_DVC_VAR          *adv_dvc_varp;
long                 flags;
Scsi_Cmnd            *done_scp = NULL, *last_scp = NULL;
Scsi_Cmnd            *tscp, *new_last_scp;
int                  do_scsi_done;
int                  scp_found;
int                  status;
int                  target;
int                  ret;
int                  device_reset = ASC_FALSE;
save_flags(flags);
cli();
ASC_DBG1(1, "advansys_reset: %x\n", (unsigned) scp);
#ifdef ADVANSYS_STATS
if (scp->host != NULL) {
ASC_STATS(scp->host, reset);
}
#endif
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
if ((reset_flags & SCSI_RESET_ASYNCHRONOUS) &&
(scp->serial_number != scp->serial_number_at_timeout)) {
ASC_PRINT1(
"advansys_reset: timeout serial number changed for request %x\n",
(unsigned) scp);
do_scsi_done = ASC_FALSE;
scp_found = ASC_FALSE;
ret = SCSI_RESET_NOT_RUNNING;
} else
#endif
if ((shp = scp->host) == NULL) {
scp->result = HOST_BYTE(DID_ERROR);
do_scsi_done = ASC_TRUE;
scp_found = ASC_FALSE;
ret = SCSI_RESET_ERROR;
} else if ((boardp = ASC_BOARDP(shp))->flags &
(ASC_HOST_IN_RESET | ASC_HOST_IN_ABORT)) {
ASC_PRINT2(
"advansys_reset: board %d: Nested host reset or abort, flags 0x%x\n",
boardp->id, boardp->flags);
do_scsi_done = ASC_TRUE;
if ((asc_rmqueue(&boardp->active, scp) == ASC_TRUE) ||
(asc_rmqueue(&boardp->waiting, scp) == ASC_TRUE)) {
scp_found = ASC_TRUE;
} else {
scp_found = ASC_FALSE;
}
scp->result = HOST_BYTE(DID_ERROR);
ret = SCSI_RESET_ERROR;
} else if (jiffies >= boardp->last_reset &&
jiffies < (boardp->last_reset + (10 * HZ))) {
ASC_DBG(1,
"advansys_reset: reset within 10 sec of last reset ignored\n");
do_scsi_done = ASC_TRUE;
if ((asc_rmqueue(&boardp->active, scp) == ASC_TRUE) ||
(asc_rmqueue(&boardp->waiting, scp) == ASC_TRUE)) {
scp_found = ASC_TRUE;
} else {
scp_found = ASC_FALSE;
}
scp->result = HOST_BYTE(DID_ERROR);
ret = SCSI_RESET_ERROR;
} else {
do_scsi_done = ASC_TRUE;
boardp->flags |= ASC_HOST_IN_RESET;
if (asc_rmqueue(&boardp->active, scp) == ASC_TRUE) {
ASC_DBG(1, "advansys_reset: active scp_found = TRUE\n");
scp_found = ASC_TRUE;
} else if (asc_rmqueue(&boardp->waiting, scp) == ASC_TRUE) {
ASC_DBG(1, "advansys_reset: waiting scp_found = TRUE\n");
scp_found = ASC_TRUE;
} else if (asc_rmqueue(&boardp->done, scp) == ASC_TRUE) {
scp_found = ASC_TRUE;
} else {
scp_found = ASC_FALSE;
}
if (ASC_NARROW_BOARD(boardp)) {
asc_dvc_varp = &boardp->dvc_var.asc_dvc_var;
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
if (reset_flags &
(SCSI_RESET_SUGGEST_BUS_RESET |
SCSI_RESET_SUGGEST_HOST_RESET)) {
#endif
ASC_DBG(1, "advansys_reset: before AscResetSB()\n");
sti();
status = AscResetSB(asc_dvc_varp);
cli();
switch (status) {
case ASC_TRUE:
ASC_DBG(1, "advansys_reset: AscResetSB() success\n");
ret = SCSI_RESET_SUCCESS;
break;
case ASC_ERROR:
default:
ASC_DBG(1, "advansys_reset: AscResetSB() failed\n");
ret = SCSI_RESET_ERROR;
break;
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
} else {
ASC_DBG1(1,
"advansys_reset: before AscResetDevice(), target %d\n",
scp->target);
sti();
status = AscResetDevice(asc_dvc_varp, scp->target);
cli();
switch (status) {
case ASC_TRUE:
ASC_DBG(1, "advansys_reset: AscResetDevice() success\n");
device_reset = ASC_TRUE;
ret = SCSI_RESET_SUCCESS;
break;
case ASC_ERROR:
default:
ASC_DBG(1,
"advansys_reset: AscResetDevice() failed; Calling AscResetSB()\n");
sti();
status = AscResetSB(asc_dvc_varp);
cli();
switch (status) {
case ASC_TRUE:
ASC_DBG(1, "advansys_reset: AscResetSB() TRUE\n");
ret = SCSI_RESET_SUCCESS;
break;
case ASC_ERROR:
default:
ASC_DBG(1, "advansys_reset: AscResetSB() ERROR\n");
ret = SCSI_RESET_ERROR;
break;
}
break;
}
}
#endif
} else {
adv_dvc_varp = &boardp->dvc_var.adv_dvc_var;
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
if (reset_flags &
(SCSI_RESET_SUGGEST_BUS_RESET |
SCSI_RESET_SUGGEST_HOST_RESET)) {
#endif
ASC_DBG(1, "advansys_reset: before AdvResetSB()\n");
switch (AdvResetSB(adv_dvc_varp)) {
case ASC_TRUE:
ASC_DBG(1, "advansys_reset: AdvResetSB() success\n");
ret = SCSI_RESET_SUCCESS;
break;
case ASC_FALSE:
default:
ASC_DBG(1, "advansys_reset: AdvResetSB() failed\n");
ret = SCSI_RESET_ERROR;
break;
}
(void) AdvISR(adv_dvc_varp);
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
} else {
ASC_DBG1(1,
"advansys_reset: before AdvResetDevice(), target %d\n",
scp->target);
switch (AdvResetDevice(adv_dvc_varp, scp->target)) {
case ASC_TRUE:
ASC_DBG(1, "advansys_reset: AdvResetDevice() success\n");
device_reset = ASC_TRUE;
ret = SCSI_RESET_SUCCESS;
break;
case ASC_FALSE:
default:
ASC_DBG(1,
"advansys_reset: AdvResetDevice() failed; Calling AdvResetSB()\n");
switch (AdvResetSB(adv_dvc_varp)) {
case ASC_TRUE:
ASC_DBG(1, "advansys_reset: AdvResetSB() TRUE\n");
ret = SCSI_RESET_SUCCESS;
break;
case ASC_FALSE:
default:
ASC_DBG(1, "advansys_reset: AdvResetSB() ERROR\n");
ret = SCSI_RESET_ERROR;
break;
}
break;
}
(void) AdvISR(adv_dvc_varp);
}
#endif
}
done_scp = asc_dequeue_list(&boardp->done, &last_scp,
ASC_TID_ALL);
if (device_reset == ASC_TRUE) {
target = scp->target;
} else {
target = ASC_TID_ALL;
}
if (done_scp == NULL) {
done_scp = asc_dequeue_list(&boardp->active, &last_scp, target);
for (tscp = done_scp; tscp; tscp = (REQP) REQPNEXT(tscp)) {
tscp->result = HOST_BYTE(DID_RESET);
}
} else {
ASC_ASSERT(last_scp != NULL);
REQPNEXT(last_scp) =
(unsigned char *) asc_dequeue_list(&boardp->active,
&new_last_scp, target);
if (new_last_scp != (Scsi_Cmnd *) NULL) {
ASC_ASSERT((REQP) REQPNEXT(last_scp) != NULL);
for (tscp = (Scsi_Cmnd *) REQPNEXT(last_scp);
tscp;
tscp = (Scsi_Cmnd *) REQPNEXT(tscp)) {
tscp->result = HOST_BYTE(DID_RESET);
}
last_scp = new_last_scp;
}
}
if (done_scp == NULL) {
done_scp = asc_dequeue_list(&boardp->waiting, &last_scp, target);
for (tscp = done_scp; tscp; tscp = (REQP) REQPNEXT(tscp)) {
tscp->result = HOST_BYTE(DID_RESET);
}
} else {
ASC_ASSERT(last_scp != NULL);
REQPNEXT(last_scp) =
(unsigned char *) asc_dequeue_list(&boardp->waiting,
&new_last_scp, target);
if (new_last_scp != NULL) {
ASC_ASSERT((REQP) REQPNEXT(last_scp) != NULL);
for (tscp = (REQP) REQPNEXT(last_scp);
tscp;
tscp = (REQP) REQPNEXT(tscp)) {
tscp->result = HOST_BYTE(DID_RESET);
}
last_scp = new_last_scp;
}
}
boardp->last_reset = jiffies;
boardp->flags &= ~ASC_HOST_IN_RESET;
if (!ASC_QUEUE_EMPTY(&boardp->waiting)) {
ASC_DBG(1, "advansys_interrupt: before asc_execute_queue()\n");
asc_execute_queue(&boardp->waiting);
}
ret = SCSI_RESET_SUCCESS;
}
ASC_ASSERT(do_scsi_done != ASC_ERROR);
ASC_ASSERT(scp_found != ASC_ERROR);
if (do_scsi_done == ASC_TRUE) {
if (scp->scsi_done == NULL) {
ASC_PRINT1(
"advansys_reset: reset request scsi_done() is NULL, %x\n",
(unsigned) scp);
} else {
if (scp_found == ASC_FALSE) {
ASC_PRINT1(
"advansys_reset: reset request not active or waiting, completing anyway %x\n",
(unsigned) scp);
}
ASC_STATS(scp->host, done);
scp->scsi_done(scp);
}
}
if (done_scp != NULL) {
asc_scsi_done_list(done_scp);
}
ASC_DBG1(1, "advansys_reset: ret %d\n", ret);
restore_flags(flags);
ASC_ASSERT(ret != ASC_ERROR);
return ret;
}
int
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
advansys_biosparam(Disk *dp, int dep, int ip[])
#else
advansys_biosparam(Disk *dp, kdev_t dep, int ip[])
#endif
{
asc_board_t     *boardp;
ASC_DBG(1, "advansys_biosparam: begin\n");
ASC_STATS(dp->device->host, biosparam);
boardp = ASC_BOARDP(dp->device->host);
if (ASC_NARROW_BOARD(boardp)) {
if ((boardp->dvc_var.asc_dvc_var.dvc_cntl &
ASC_CNTL_BIOS_GT_1GB) && dp->capacity > 0x200000) {
ip[0] = 255;
ip[1] = 63;
} else {
ip[0] = 64;
ip[1] = 32;
}
} else {
if ((boardp->dvc_var.adv_dvc_var.bios_ctrl &
BIOS_CTRL_EXTENDED_XLAT) && dp->capacity > 0x200000) {
ip[0] = 255;
ip[1] = 63;
} else {
ip[0] = 64;
ip[1] = 32;
}
}
ip[2] = dp->capacity / (ip[0] * ip[1]);
ASC_DBG(1, "advansys_biosparam: end\n");
return 0;
}
ASC_INITFUNC(
void
advansys_setup(char *str, int *ints)
)
{
int    i;
if (asc_iopflag == ASC_TRUE) {
printk("AdvanSys SCSI: 'advansys' LILO option may appear only once\n");
return;
}
asc_iopflag = ASC_TRUE;
if (ints[0] > ASC_NUM_IOPORT_PROBE) {
#ifdef ADVANSYS_DEBUG
if ((ints[0] == ASC_NUM_IOPORT_PROBE + 1) &&
(ints[ASC_NUM_IOPORT_PROBE + 1] >> 4 == 0xdeb)) {
asc_dbglvl = ints[ASC_NUM_IOPORT_PROBE + 1] & 0xf;
} else {
#endif
printk("AdvanSys SCSI: only %d I/O ports accepted\n",
ASC_NUM_IOPORT_PROBE);
#ifdef ADVANSYS_DEBUG
}
#endif
}
#ifdef ADVANSYS_DEBUG
ASC_DBG1(1, "advansys_setup: ints[0] %d\n", ints[0]);
for (i = 1; i < ints[0]; i++) {
ASC_DBG2(1, " ints[%d] %x", i, ints[i]);
}
ASC_DBG(1, "\n");
#endif
for (i = 1; i <= ints[0] && i <= ASC_NUM_IOPORT_PROBE; i++) {
asc_ioport[i-1] = ints[i];
ASC_DBG2(1, "advansys_setup: asc_ioport[%d] %x\n",
i - 1, asc_ioport[i-1]);
}
}
#ifdef MODULE
Scsi_Host_Template driver_template = ADVANSYS;
# include "scsi_module.c"
#endif
STATIC void
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,70)
advansys_interrupt(int irq, struct pt_regs *regs)
#else
advansys_interrupt(int irq, void *dev_id, struct pt_regs *regs)
#endif
{
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,95)
long            flags;
#else
unsigned long   flags;
#endif
int             i;
asc_board_t     *boardp;
Scsi_Cmnd       *done_scp = NULL, *last_scp = NULL;
Scsi_Cmnd       *new_last_scp;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,95)
save_flags(flags);
cli();
#else
spin_lock_irqsave(&io_request_lock, flags);
#endif
ASC_DBG(1, "advansys_interrupt: begin\n");
for (i = 0; i < asc_board_count; i++) {
boardp = ASC_BOARDP(asc_host[i]);
ASC_DBG2(2, "advansys_interrupt: i %d, boardp %lx\n",
i, (ulong) boardp)
if (ASC_NARROW_BOARD(boardp)) {
if (AscIsIntPending(asc_host[i]->io_port)) {
ASC_STATS(asc_host[i], interrupt);
ASC_DBG(1, "advansys_interrupt: before AscISR()\n");
AscISR(&boardp->dvc_var.asc_dvc_var);
}
} else {
ASC_DBG(1, "advansys_interrupt: before AdvISR()\n");
if (AdvISR(&boardp->dvc_var.adv_dvc_var)) {
ASC_STATS(asc_host[i], interrupt);
}
}
if ((boardp->flags & (ASC_HOST_IN_RESET | ASC_HOST_IN_ABORT)) == 0) {
ASC_DBG2(1, "advansys_interrupt: done_scp %lx, last_scp %lx\n",
(ulong) done_scp, (ulong) last_scp);
if (!ASC_QUEUE_EMPTY(&boardp->waiting)) {
ASC_DBG(1, "advansys_interrupt: before asc_execute_queue()\n");
asc_execute_queue(&boardp->waiting);
}
if (done_scp == NULL) {
done_scp = asc_dequeue_list(&boardp->done, &last_scp,
ASC_TID_ALL);
} else {
ASC_ASSERT(last_scp != NULL);
REQPNEXT(last_scp) =
(unsigned char *) asc_dequeue_list(&boardp->done,
&new_last_scp,
ASC_TID_ALL);
if (new_last_scp != NULL) {
ASC_ASSERT(REQPNEXT(last_scp) != NULL);
last_scp = new_last_scp;
}
}
}
}
asc_scsi_done_list(done_scp);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,95)
restore_flags(flags);
#else
spin_unlock_irqrestore(&io_request_lock, flags);
#endif
ASC_DBG(1, "advansys_interrupt: end\n");
return;
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC void
advansys_select_queue_depths(struct Scsi_Host *shp, Scsi_Device *devicelist)
{
Scsi_Device        *device;
asc_board_t        *boardp;
boardp = ASC_BOARDP(shp);
boardp->flags |= ASC_SELECT_QUEUE_DEPTHS;
for (device = devicelist; device != NULL; device = device->next) {
if (device->host != shp) {
continue;
}
boardp->device[device->id] = device;
if (ASC_NARROW_BOARD(boardp)) {
device->queue_depth =
boardp->dvc_var.asc_dvc_var.max_dvc_qng[device->id];
} else {
device->queue_depth =
boardp->dvc_var.adv_dvc_var.max_dvc_qng;
}
ASC_DBG3(1, "advansys_select_queue_depths: shp %x, id %d, depth %d\n",
(unsigned) shp, device->id, device->queue_depth);
}
}
#endif
STATIC void
advansys_command_done(Scsi_Cmnd *scp)
{
ASC_DBG1(1, "advansys_command_done: scp %x\n", (unsigned) scp);
scp->SCp.Status = 1;
}
STATIC void
asc_scsi_done_list(Scsi_Cmnd *scp)
{
Scsi_Cmnd    *tscp;
ASC_DBG(2, "asc_scsi_done_list: begin\n");
while (scp != NULL) {
ASC_DBG1(3, "asc_scsi_done_list: scp %x\n", (unsigned) scp);
tscp = (REQP) REQPNEXT(scp);
REQPNEXT(scp) = NULL;
ASC_STATS(scp->host, done);
ASC_ASSERT(scp->scsi_done != NULL);
scp->scsi_done(scp);
scp = tscp;
}
ASC_DBG(2, "asc_scsi_done_list: done\n");
return;
}
STATIC int
asc_execute_scsi_cmnd(Scsi_Cmnd *scp)
{
asc_board_t        *boardp;
ASC_DVC_VAR        *asc_dvc_varp;
ADV_DVC_VAR        *adv_dvc_varp;
ADV_SCSI_REQ_Q     *adv_scsiqp;
Scsi_Device        *device;
int                ret;
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
ASC_DBG2(1, "asc_execute_scsi_cmnd: scp %x, done %x\n",
(unsigned) scp, (unsigned) scp->scsi_done);
boardp = ASC_BOARDP(scp->host);
device = boardp->device[scp->target];
if (ASC_NARROW_BOARD(boardp)) {
asc_dvc_varp = &boardp->dvc_var.asc_dvc_var;
if (asc_build_req(boardp, scp) == ASC_ERROR) {
ASC_STATS(scp->host, build_error);
return ASC_ERROR;
}
switch (ret = AscExeScsiQueue(asc_dvc_varp, &asc_scsi_q)) {
case ASC_NOERROR:
ASC_STATS(scp->host, exe_noerror);
boardp->reqcnt[scp->target]++;
#if ASC_QUEUE_FLOW_CONTROL
if (boardp->nerrcnt[scp->target]++ > 100) {
boardp->nerrcnt[scp->target] = 0;
if (device != NULL &&
(device->queue_curr_depth < device->queue_depth) &&
(!(boardp->queue_full &
ADV_TID_TO_TIDMASK(scp->target)) ||
(boardp->queue_full_cnt[scp->target] >
device->queue_curr_depth))) {
device->queue_curr_depth++;
}
}
#endif
asc_enqueue(&boardp->active, scp, ASC_BACK);
ASC_DBG(1,
"asc_execute_scsi_cmnd: AscExeScsiQueue(), ASC_NOERROR\n");
break;
case ASC_BUSY:
ASC_STATS(scp->host, exe_busy);
#if ASC_QUEUE_FLOW_CONTROL
boardp->nerrcnt[scp->target] = 0;
if (device != NULL && device->queue_curr_depth > 1) {
device->queue_curr_depth--;
}
#endif
break;
case ASC_ERROR:
ASC_PRINT2(
"asc_execute_scsi_cmnd: board %d: AscExeScsiQueue() ASC_ERROR, err_code %x\n",
boardp->id, asc_dvc_varp->err_code);
ASC_STATS(scp->host, exe_error);
#if ASC_QUEUE_FLOW_CONTROL
boardp->nerrcnt[scp->target] = 0;
#endif
scp->result = HOST_BYTE(DID_ERROR);
asc_enqueue(&boardp->done, scp, ASC_BACK);
break;
default:
ASC_PRINT2(
"asc_execute_scsi_cmnd: board %d: AscExeScsiQueue() unknown, err_code %x\n",
boardp->id, asc_dvc_varp->err_code);
ASC_STATS(scp->host, exe_unknown);
#if ASC_QUEUE_FLOW_CONTROL
boardp->nerrcnt[scp->target] = 0;
#endif
scp->result = HOST_BYTE(DID_ERROR);
asc_enqueue(&boardp->done, scp, ASC_BACK);
break;
}
} else {
adv_dvc_varp = &boardp->dvc_var.adv_dvc_var;
switch (adv_build_req(boardp, scp, &adv_scsiqp)) {
case ASC_NOERROR:
ASC_DBG(3, "asc_execute_scsi_cmnd: adv_build_req ASC_NOERROR\n");
break;
case ASC_BUSY:
ASC_DBG(1, "asc_execute_scsi_cmnd: adv_build_req ASC_BUSY\n");
return ASC_BUSY;
case ASC_ERROR:
default:
ASC_DBG(1, "asc_execute_scsi_cmnd: adv_build_req ASC_ERROR\n");
ASC_STATS(scp->host, build_error);
return ASC_ERROR;
}
switch (ret = AdvExeScsiQueue(adv_dvc_varp, adv_scsiqp)) {
case ASC_NOERROR:
ASC_STATS(scp->host, exe_noerror);
boardp->reqcnt[scp->target]++;
asc_enqueue(&boardp->active, scp, ASC_BACK);
ASC_DBG(1,
"asc_execute_scsi_cmnd: AdvExeScsiQueue(), ASC_NOERROR\n");
break;
case ASC_BUSY:
ASC_STATS(scp->host, exe_busy);
break;
case ASC_ERROR:
ASC_PRINT2(
"asc_execute_scsi_cmnd: board %d: AdvExeScsiQueue() ASC_ERROR, err_code %x\n",
boardp->id, adv_dvc_varp->err_code);
ASC_STATS(scp->host, exe_error);
scp->result = HOST_BYTE(DID_ERROR);
asc_enqueue(&boardp->done, scp, ASC_BACK);
break;
default:
ASC_PRINT2(
"asc_execute_scsi_cmnd: board %d: AdvExeScsiQueue() unknown, err_code %x\n",
boardp->id, adv_dvc_varp->err_code);
ASC_STATS(scp->host, exe_unknown);
scp->result = HOST_BYTE(DID_ERROR);
asc_enqueue(&boardp->done, scp, ASC_BACK);
break;
}
}
ASC_DBG(1, "asc_execute_scsi_cmnd: end\n");
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
return ret;
}
STATIC int
asc_build_req(asc_board_t *boardp, Scsi_Cmnd *scp)
{
memset(&asc_scsi_q, 0, sizeof(ASC_SCSI_Q));
asc_scsi_q.q2.srb_ptr = (ulong) scp;
ASC_ASSERT(scp->cmd_len <= ASC_MAX_CDB_LEN);
if (scp->cmd_len > ASC_MAX_CDB_LEN) {
scp->cmd_len = ASC_MAX_CDB_LEN;
}
asc_scsi_q.cdbptr = &scp->cmnd[0];
asc_scsi_q.q2.cdb_len = scp->cmd_len;
asc_scsi_q.q1.target_id = ASC_TID_TO_TARGET_ID(scp->target);
asc_scsi_q.q1.target_lun = scp->lun;
asc_scsi_q.q2.target_ix = ASC_TIDLUN_TO_IX(scp->target, scp->lun);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
asc_scsi_q.q1.sense_addr = (ulong) &scp->sense_buffer[0];
#else
asc_scsi_q.q1.sense_addr = virt_to_bus(&scp->sense_buffer[0]);
#endif
asc_scsi_q.q1.sense_len = sizeof(scp->sense_buffer);
if ((boardp->dvc_var.asc_dvc_var.cur_dvc_qng[scp->target] > 0) &&
(boardp->reqcnt[scp->target] % 255) == 0) {
asc_scsi_q.q2.tag_code = M2_QTAG_MSG_ORDERED;
} else {
asc_scsi_q.q2.tag_code = M2_QTAG_MSG_SIMPLE;
}
if (scp->use_sg == 0) {
ASC_STATS(scp->host, cont_cnt);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
asc_scsi_q.q1.data_addr = (ulong) scp->request_buffer;
#else
asc_scsi_q.q1.data_addr = virt_to_bus(scp->request_buffer);
#endif
asc_scsi_q.q1.data_cnt = scp->request_bufflen;
ASC_STATS_ADD(scp->host, cont_xfer,
ASC_CEILING(scp->request_bufflen, 512));
asc_scsi_q.q1.sg_queue_cnt = 0;
asc_scsi_q.sg_head = NULL;
} else {
int                     sgcnt;
struct scatterlist      *slp;
if (scp->use_sg > scp->host->sg_tablesize) {
ASC_PRINT3(
"asc_build_req: board %d: use_sg %d > sg_tablesize %d\n",
boardp->id, scp->use_sg, scp->host->sg_tablesize);
scp->result = HOST_BYTE(DID_ERROR);
asc_enqueue(&boardp->done, scp, ASC_BACK);
return ASC_ERROR;
}
ASC_STATS(scp->host, sg_cnt);
memset(&asc_sg_head, 0, sizeof(ASC_SG_HEAD));
asc_scsi_q.q1.cntl |= QC_SG_HEAD;
asc_scsi_q.sg_head = &asc_sg_head;
asc_scsi_q.q1.data_cnt = 0;
asc_scsi_q.q1.data_addr = 0;
asc_sg_head.entry_cnt = asc_scsi_q.q1.sg_queue_cnt = scp->use_sg;
ASC_STATS_ADD(scp->host, sg_elem, asc_sg_head.entry_cnt);
slp = (struct scatterlist *) scp->request_buffer;
for (sgcnt = 0; sgcnt < scp->use_sg; sgcnt++, slp++) {
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
asc_sg_head.sg_list[sgcnt].addr = (ulong) slp->address;
#else
asc_sg_head.sg_list[sgcnt].addr = virt_to_bus(slp->address);
#endif
asc_sg_head.sg_list[sgcnt].bytes = slp->length;
ASC_STATS_ADD(scp->host, sg_xfer, ASC_CEILING(slp->length, 512));
}
}
ASC_DBG_PRT_ASC_SCSI_Q(2, &asc_scsi_q);
ASC_DBG_PRT_CDB(1, scp->cmnd, scp->cmd_len);
return ASC_NOERROR;
}
STATIC int
adv_build_req(asc_board_t *boardp, Scsi_Cmnd *scp,
ADV_SCSI_REQ_Q **adv_scsiqpp)
{
adv_req_t           *reqp;
ADV_SCSI_REQ_Q      *scsiqp;
int                 i;
if (boardp->adv_reqp == NULL) {
ASC_DBG(1, "adv_build_req: no free adv_req_t\n");
ASC_STATS(scp->host, adv_build_noreq);
return ASC_BUSY;
} else {
reqp = boardp->adv_reqp;
boardp->adv_reqp = reqp->next_reqp;
reqp->next_reqp = NULL;
}
scsiqp = (ADV_SCSI_REQ_Q *) ADV_DWALIGN(&reqp->scsi_req_q);
memset(scsiqp, 0, sizeof(ADV_SCSI_REQ_Q));
scsiqp->srb_ptr = (ulong) reqp;
reqp->cmndp = scp;
ASC_ASSERT(scp->cmd_len <= ASC_MAX_CDB_LEN);
if (scp->cmd_len > ASC_MAX_CDB_LEN) {
scp->cmd_len = ASC_MAX_CDB_LEN;
}
scsiqp->cdb_len = scp->cmd_len;
for (i = 0; i < scp->cmd_len; i++) {
scsiqp->cdb[i] = scp->cmnd[i];
}
scsiqp->target_id = scp->target;
scsiqp->target_lun = scp->lun;
scsiqp->vsense_addr = (ulong) &scp->sense_buffer[0];
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
scsiqp->sense_addr = (ulong) &scp->sense_buffer[0];
#else
scsiqp->sense_addr = virt_to_bus(&scp->sense_buffer[0]);
#endif
scsiqp->sense_len = sizeof(scp->sense_buffer);
scsiqp->data_cnt = scp->request_bufflen;
scsiqp->vdata_addr = (ulong) scp->request_buffer;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
scsiqp->data_addr = (ulong) scp->request_buffer;
#else
scsiqp->data_addr = virt_to_bus(scp->request_buffer);
#endif
if (scp->use_sg == 0) {
reqp->sgblkp = NULL;
scsiqp->sg_list_ptr = NULL;
ASC_STATS(scp->host, cont_cnt);
ASC_STATS_ADD(scp->host, cont_xfer,
ASC_CEILING(scp->request_bufflen, 512));
} else {
if (scp->use_sg > ADV_MAX_SG_LIST) {
ASC_PRINT3(
"adv_build_req: board %d: use_sg %d > ADV_MAX_SG_LIST %d\n",
boardp->id, scp->use_sg, scp->host->sg_tablesize);
scp->result = HOST_BYTE(DID_ERROR);
asc_enqueue(&boardp->done, scp, ASC_BACK);
reqp->next_reqp = boardp->adv_reqp;
boardp->adv_reqp = reqp;
return ASC_ERROR;
}
if (boardp->adv_sgblkp == NULL) {
ASC_DBG(1, "adv_build_req: no free adv_sgblk_t\n");
ASC_STATS(scp->host, adv_build_nosg);
reqp->next_reqp = boardp->adv_reqp;
boardp->adv_reqp = reqp;
return ASC_BUSY;
} else {
reqp->sgblkp = boardp->adv_sgblkp;
boardp->adv_sgblkp = reqp->sgblkp->next_sgblkp;
reqp->sgblkp->next_sgblkp = NULL;
}
scsiqp->sg_list_ptr = (ADV_SG_BLOCK *)
ADV_DWALIGN(&reqp->sgblkp->sg_block[0]);
memset(scsiqp->sg_list_ptr, 0, sizeof(ADV_SG_BLOCK) *
(ADV_NUM_SG_BLOCK + ADV_NUM_PAGE_CROSSING));
if (adv_get_sglist(&boardp->dvc_var.adv_dvc_var, scsiqp, scp) ==
ADV_ERROR) {
ASC_ASSERT(reqp->sgblkp != NULL);
reqp->sgblkp->next_sgblkp = boardp->adv_sgblkp;
boardp->adv_sgblkp = reqp->sgblkp;
reqp->next_reqp = boardp->adv_reqp;
boardp->adv_reqp = reqp;
return ADV_ERROR;
}
ASC_STATS(scp->host, sg_cnt);
ASC_STATS_ADD(scp->host, sg_elem, scp->use_sg);
}
ASC_DBG_PRT_ADV_SCSI_REQ_Q(2, scsiqp);
ASC_DBG_PRT_CDB(1, scp->cmnd, scp->cmd_len);
*adv_scsiqpp = scsiqp;
return ASC_NOERROR;
}
STATIC int
adv_get_sglist(ADV_DVC_VAR *adv_dvc_varp, ADV_SCSI_REQ_Q *scsiqp,
Scsi_Cmnd *scp)
{
ADV_SG_BLOCK        *sg_block;
ulong               sg_block_next_addr;
ulong               sg_block_physical_addr;
int                 sg_block_index, i;
struct scatterlist  *slp;
int                 sg_elem_cnt;
slp = (struct scatterlist *) scp->request_buffer;
sg_elem_cnt = scp->use_sg;
sg_block = scsiqp->sg_list_ptr;
sg_block_next_addr = (ulong) sg_block;
sg_block_physical_addr =
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
(ulong) scsiqp->sg_list_ptr;
#else
virt_to_bus(scsiqp->sg_list_ptr);
#endif
ADV_ASSERT(ADV_DWALIGN(sg_block_physical_addr) ==
sg_block_physical_addr);
scsiqp->sg_real_addr = sg_block_physical_addr;
sg_block_index = 0;
do
{
sg_block->first_entry_no = sg_block_index;
for (i = 0; i < NO_OF_SG_PER_BLOCK; i++)
{
sg_block->sg_list[i].sg_addr =
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
(ulong) slp->address;
#else
virt_to_bus(slp->address);
#endif
sg_block->sg_list[i].sg_count = slp->length;
ASC_STATS_ADD(scp->host, sg_xfer, ASC_CEILING(slp->length, 512));
if (--sg_elem_cnt == 0)
{
scsiqp->sg_entry_cnt = sg_block_index + i + 1;
sg_block->last_entry_no = sg_block_index + i;
sg_block->sg_ptr = 0L;
return ADV_SUCCESS;
}
slp++;
}
sg_block_next_addr += sizeof(ADV_SG_BLOCK);
sg_block_physical_addr += sizeof(ADV_SG_BLOCK);
ADV_ASSERT(ADV_DWALIGN(sg_block_physical_addr) ==
sg_block_physical_addr);
sg_block_index += NO_OF_SG_PER_BLOCK;
sg_block->sg_ptr = (ADV_SG_BLOCK *) sg_block_physical_addr;
sg_block->last_entry_no = sg_block_index - 1;
sg_block = (ADV_SG_BLOCK *) sg_block_next_addr;
}
while (1);
}
STATIC void
asc_isr_callback(ASC_DVC_VAR *asc_dvc_varp, ASC_QDONE_INFO *qdonep)
{
asc_board_t         *boardp;
Scsi_Cmnd           *scp;
struct Scsi_Host    *shp;
int                 underrun = ASC_FALSE;
int                 i;
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
ASC_DBG2(1, "asc_isr_callback: asc_dvc_varp %x, qdonep %x\n",
(unsigned) asc_dvc_varp, (unsigned) qdonep);
ASC_DBG_PRT_ASC_QDONE_INFO(2, qdonep);
scp = (Scsi_Cmnd *) qdonep->d2.srb_ptr;
ASC_DBG1(1, "asc_isr_callback: scp %x\n", (unsigned) scp);
if (scp == NULL) {
ASC_PRINT("asc_isr_callback: scp is NULL\n");
return;
}
ASC_DBG_PRT_CDB(2, scp->cmnd, scp->cmd_len);
shp = scp->host;
for (i = 0; i < asc_board_count; i++) {
if (asc_host[i] == shp) {
break;
}
}
if (i == asc_board_count) {
ASC_PRINT2("asc_isr_callback: scp %x has bad host pointer, host %x\n",
(unsigned) scp, (unsigned) shp);
return;
}
ASC_STATS(shp, callback);
ASC_DBG1(1, "asc_isr_callback: shp %x\n", (unsigned) shp);
boardp = ASC_BOARDP(shp);
ASC_ASSERT(asc_dvc_varp == &boardp->dvc_var.asc_dvc_var);
if (asc_rmqueue(&boardp->active, scp) == ASC_FALSE) {
ASC_PRINT2("asc_isr_callback: board %d: scp %x not on active queue\n",
boardp->id, (unsigned) scp);
return;
}
if (scp->request_bufflen != 0 && qdonep->remain_bytes != 0 &&
qdonep->remain_bytes <= scp->request_bufflen != 0) {
ASC_DBG1(1, "asc_isr_callback: underrun condition %u bytes\n",
(unsigned) qdonep->remain_bytes);
underrun = ASC_TRUE;
}
switch (qdonep->d3.done_stat) {
case QD_NO_ERROR:
ASC_DBG(2, "asc_isr_callback: QD_NO_ERROR\n");
switch (qdonep->d3.host_stat) {
case QHSTA_NO_ERROR:
scp->result = 0;
break;
default:
scp->result = HOST_BYTE(DID_ERROR);
break;
}
if (scp->cmnd[0] == SCSICMD_Inquiry && scp->lun == 0 &&
(scp->request_bufflen - qdonep->remain_bytes) >= 8)
{
AscInquiryHandling(asc_dvc_varp, scp->target & 0x7,
(ASC_SCSI_INQUIRY *) scp->request_buffer);
}
if (scp->result == 0 && underrun == ASC_TRUE) {
scp->result = HOST_BYTE(DID_UNDERRUN);
}
break;
case QD_WITH_ERROR:
ASC_DBG(2, "asc_isr_callback: QD_WITH_ERROR\n");
switch (qdonep->d3.host_stat) {
case QHSTA_NO_ERROR:
if (qdonep->d3.scsi_stat == SS_CHK_CONDITION) {
ASC_DBG(2, "asc_isr_callback: SS_CHK_CONDITION\n");
ASC_DBG_PRT_SENSE(2, scp->sense_buffer,
sizeof(scp->sense_buffer));
scp->result = DRIVER_BYTE(DRIVER_SENSE) |
STATUS_BYTE(qdonep->d3.scsi_stat);
} else {
scp->result = STATUS_BYTE(qdonep->d3.scsi_stat);
}
break;
default:
ASC_DBG1(1, "asc_isr_callback: host_stat %x\n",
qdonep->d3.host_stat);
scp->result = HOST_BYTE(DID_BAD_TARGET);
break;
}
break;
case QD_ABORTED_BY_HOST:
ASC_DBG(1, "asc_isr_callback: QD_ABORTED_BY_HOST\n");
scp->result = HOST_BYTE(DID_ABORT) | MSG_BYTE(qdonep->d3.scsi_msg) |
STATUS_BYTE(qdonep->d3.scsi_stat);
break;
default:
ASC_DBG1(1, "asc_isr_callback: done_stat %x\n", qdonep->d3.done_stat);
scp->result = HOST_BYTE(DID_ERROR) | MSG_BYTE(qdonep->d3.scsi_msg) |
STATUS_BYTE(qdonep->d3.scsi_stat);
break;
}
if ((boardp->init_tidmask & ADV_TID_TO_TIDMASK(scp->target)) == 0 &&
qdonep->d3.done_stat == QD_NO_ERROR &&
qdonep->d3.host_stat == QHSTA_NO_ERROR) {
boardp->init_tidmask |= ADV_TID_TO_TIDMASK(scp->target);
}
asc_enqueue(&boardp->done, scp, ASC_BACK);
return;
}
STATIC void
adv_isr_callback(ADV_DVC_VAR *adv_dvc_varp, ADV_SCSI_REQ_Q *scsiqp)
{
asc_board_t         *boardp;
adv_req_t           *reqp;
Scsi_Cmnd           *scp;
struct Scsi_Host    *shp;
int                 underrun = ASC_FALSE;
int                 i;
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
ASC_DBG2(1, "adv_isr_callback: adv_dvc_varp %x, scsiqp %x\n",
(unsigned) adv_dvc_varp, (unsigned) scsiqp);
ASC_DBG_PRT_ADV_SCSI_REQ_Q(2, scsiqp);
reqp = (adv_req_t *) scsiqp->srb_ptr;
ASC_DBG1(1, "adv_isr_callback: reqp %x\n", (unsigned) reqp);
if (reqp == NULL) {
ASC_PRINT("adv_isr_callback: reqp is NULL\n");
return;
}
scp = reqp->cmndp;
ASC_DBG1(1, "adv_isr_callback: scp %x\n", (unsigned) scp);
if (scp == NULL) {
ASC_PRINT("adv_isr_callback: scp is NULL; adv_req_t dropped.\n");
return;
}
ASC_DBG_PRT_CDB(2, scp->cmnd, scp->cmd_len);
shp = scp->host;
for (i = 0; i < asc_board_count; i++) {
if (asc_host[i] == shp) {
break;
}
}
if (i == asc_board_count) {
ASC_PRINT2("adv_isr_callback: scp %x has bad host pointer, host %x\n",
(unsigned) scp, (unsigned) shp);
return;
}
ASC_STATS(shp, callback);
ASC_DBG1(1, "adv_isr_callback: shp %x\n", (unsigned) shp);
boardp = ASC_BOARDP(shp);
ASC_ASSERT(adv_dvc_varp == &boardp->dvc_var.adv_dvc_var);
if (asc_rmqueue(&boardp->active, scp) == ASC_FALSE) {
ASC_PRINT2("adv_isr_callback: board %d: scp %x not on active queue\n",
boardp->id, (unsigned) scp);
return;
}
if (scp->request_bufflen != 0 && scsiqp->data_cnt != 0) {
ASC_DBG1(1, "adv_isr_callback: underrun condition %lu bytes\n",
scsiqp->data_cnt);
underrun = ASC_TRUE;
}
switch (scsiqp->done_status) {
case QD_NO_ERROR:
ASC_DBG(2, "adv_isr_callback: QD_NO_ERROR\n");
switch (scsiqp->host_status) {
case QHSTA_NO_ERROR:
scp->result = 0;
break;
default:
ASC_DBG1(2, "adv_isr_callback: host_status %x\n",
scsiqp->host_status);
scp->result = HOST_BYTE(DID_ERROR);
break;
}
if (scp->result == 0 && underrun == ASC_TRUE) {
scp->result = HOST_BYTE(DID_UNDERRUN);
}
break;
case QD_WITH_ERROR:
ASC_DBG(2, "adv_isr_callback: QD_WITH_ERROR\n");
switch (scsiqp->host_status) {
case QHSTA_NO_ERROR:
if (scsiqp->scsi_status == SS_CHK_CONDITION) {
ASC_DBG(2, "adv_isr_callback: SS_CHK_CONDITION\n");
ASC_DBG_PRT_SENSE(2, scp->sense_buffer,
sizeof(scp->sense_buffer));
scp->result = DRIVER_BYTE(DRIVER_SENSE) |
STATUS_BYTE(scsiqp->scsi_status);
} else {
scp->result = STATUS_BYTE(scsiqp->scsi_status);
}
break;
default:
ASC_DBG1(1, "adv_isr_callback: host_status %x\n",
scsiqp->host_status);
scp->result = HOST_BYTE(DID_BAD_TARGET);
break;
}
break;
case QD_ABORTED_BY_HOST:
ASC_DBG(1, "adv_isr_callback: QD_ABORTED_BY_HOST\n");
scp->result = HOST_BYTE(DID_ABORT) | STATUS_BYTE(scsiqp->scsi_status);
break;
default:
ASC_DBG1(1, "adv_isr_callback: done_status %x\n", scsiqp->done_status);
scp->result = HOST_BYTE(DID_ERROR) | STATUS_BYTE(scsiqp->scsi_status);
break;
}
if ((boardp->init_tidmask & ADV_TID_TO_TIDMASK(scp->target)) == 0 &&
scsiqp->done_status == QD_NO_ERROR &&
scsiqp->host_status == QHSTA_NO_ERROR) {
boardp->init_tidmask |= ADV_TID_TO_TIDMASK(scp->target);
}
asc_enqueue(&boardp->done, scp, ASC_BACK);
if (reqp->sgblkp != NULL) {
reqp->sgblkp->next_sgblkp = boardp->adv_sgblkp;
boardp->adv_sgblkp = reqp->sgblkp;
}
reqp->next_reqp = boardp->adv_reqp;
boardp->adv_reqp = reqp;
ASC_DBG(1, "adv_isr_callback: done\n");
return;
}
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
ASC_INITFUNC(
STATIC int
asc_srch_pci_dev(PCI_DEVICE *pciDevice)
)
{
int                        ret = PCI_DEVICE_NOT_FOUND;
ASC_DBG(2, "asc_srch_pci_dev: begin\n");
if (pci_scan_method == -1) {
pci_scan_method = asc_scan_method();
}
pciDevice->type = pci_scan_method;
ASC_DBG1(2, "asc_srch_pci_dev: type %d\n", pciDevice->type);
ret = asc_pci_find_dev(pciDevice);
ASC_DBG1(2, "asc_srch_pci_dev: asc_pci_find_dev() return %d\n", ret);
if (ret == PCI_DEVICE_FOUND) {
pciDevice->slotNumber = pciDevice->slotFound + 1;
pciDevice->startSlot = pciDevice->slotFound + 1;
} else {
if (pciDevice->bridge > pciDevice->busNumber) {
ASC_DBG2(2, "asc_srch_pci_dev: bridge %x, busNumber %x\n",
pciDevice->bridge, pciDevice->busNumber);
pciDevice->busNumber++;
pciDevice->slotNumber = 0;
pciDevice->startSlot = 0;
pciDevice->endSlot = 0x0f;
ret = asc_srch_pci_dev(pciDevice);
ASC_DBG1(2, "asc_srch_pci_dev: recursive call return %d\n", ret);
}
}
ASC_DBG1(2, "asc_srch_pci_dev: return %d\n", ret);
return ret;
}
ASC_INITFUNC(
STATIC uchar
asc_scan_method(void)
)
{
ushort      data;
PCI_DATA    pciData;
uchar       type;
uchar       slot;
ASC_DBG(2, "asc_scan_method: begin\n");
memset(&pciData, 0, sizeof(pciData));
for (type = 1; type < 3; type++) {
pciData.type = type;
for (slot = 0; slot < PCI_MAX_SLOT; slot++) {
pciData.slot = slot;
data = asc_get_cfg_word(&pciData);
if ((data != 0xFFFF) && (data != 0x0000)) {
ASC_DBG2(4, "asc_scan_method: data %x, type %d\n", data, type);
return (type);
}
}
}
ASC_DBG1(4, "asc_scan_method: type %d\n", type);
return (type);
}
ASC_INITFUNC(
STATIC int
asc_pci_find_dev(PCI_DEVICE *pciDevice)
)
{
PCI_DATA    pciData;
ushort      vendorid, deviceid;
uchar       classcode, subclass;
uchar       lslot;
ASC_DBG(3, "asc_pci_find_dev: begin\n");
pciData.type = pciDevice->type;
pciData.bus = pciDevice->busNumber;
pciData.func = pciDevice->devFunc;
lslot = pciDevice->startSlot;
for (; lslot < pciDevice->endSlot; lslot++) {
pciData.slot = lslot;
pciData.offset = VENDORID_OFFSET;
vendorid = asc_get_cfg_word(&pciData);
ASC_DBG1(3, "asc_pci_find_dev: vendorid %x\n", vendorid);
if (vendorid != 0xffff) {
pciData.offset = DEVICEID_OFFSET;
deviceid = asc_get_cfg_word(&pciData);
ASC_DBG1(3, "asc_pci_find_dev: deviceid %x\n", deviceid);
if ((vendorid == ASC_PCI_VENDORID) &&
((deviceid == ASC_PCI_DEVICE_ID_1100) ||
(deviceid == ASC_PCI_DEVICE_ID_1200) ||
(deviceid == ASC_PCI_DEVICE_ID_1300) ||
(deviceid == ASC_PCI_DEVICE_ID_2300))) {
pciDevice->slotFound = lslot;
ASC_DBG(3, "asc_pci_find_dev: PCI_DEVICE_FOUND\n");
return PCI_DEVICE_FOUND;
} else {
pciData.offset = SUBCLASS_OFFSET;
subclass = asc_get_cfg_byte(&pciData);
pciData.offset = CLASSCODE_OFFSET;
classcode = asc_get_cfg_byte(&pciData);
if ((classcode & PCI_BASE_CLASS_BRIDGE_DEVICE) &&
(subclass & PCI_SUB_CLASS_PCI_TO_PCI_BRIDGE_CONTROLLER)) {
pciDevice->bridge++;
}
ASC_DBG2(3, "asc_pci_find_dev: subclass %x, classcode %x\n",
subclass, classcode);
}
}
}
return PCI_DEVICE_NOT_FOUND;
}
ASC_INITFUNC(
STATIC void
asc_get_pci_cfg(PCI_DEVICE *pciDevice, PCI_CONFIG_SPACE *pciConfig)
)
{
PCI_DATA    pciData;
uchar       counter;
uchar       *localConfig;
ASC_DBG1(4, "asc_get_pci_cfg: slotFound %d\n ",
pciDevice->slotFound);
pciData.type = pciDevice->type;
pciData.bus = pciDevice->busNumber;
pciData.slot = pciDevice->slotFound;
pciData.func = pciDevice->devFunc;
localConfig = (uchar *) pciConfig;
for (counter = 0; counter < sizeof(PCI_CONFIG_SPACE); counter++) {
pciData.offset = counter;
*localConfig = asc_get_cfg_byte(&pciData);
ASC_DBG1(4, "asc_get_pci_cfg: byte %x\n", *localConfig);
localConfig++;
}
ASC_DBG1(4, "asc_get_pci_cfg: counter %d\n", counter);
}
ASC_INITFUNC(
STATIC ushort
asc_get_cfg_word(PCI_DATA *pciData)
)
{
ushort   tmp;
ulong    address;
ulong    lbus = pciData->bus;
ulong    lslot = pciData->slot;
ulong    lfunc = pciData->func;
uchar    t2CFA, t2CF8;
ulong    t1CF8, t1CFC;
ASC_DBG4(4, "asc_get_cfg_word: type %d, bus %lu, slot %lu, func %lu\n",
pciData->type, lbus, lslot, lfunc);
if (pciData->type == 2) {
t2CFA = inp(0xCFA);
t2CF8 = inp(0xCF8);
outp(0xCFA, pciData->bus);
outp(0xCF8, 0x10 | (pciData->func << 1)) ;
tmp = (ushort) inpw(0xC000 | ((pciData->slot << 8) + pciData->offset));
outp(0xCFA, t2CFA);
outp(0xCF8, t2CF8);
} else {
t1CF8 = inpl(0xCF8);
t1CFC = inpl(0xCFC);
address = (ulong) ((lbus << 16) | (lslot << 11) |
(lfunc << 8) | (pciData->offset & 0xFC) | 0x80000000L);
outpl(0xCF8, address);
tmp = (ushort) ((inpl(0xCFC) >>
((pciData->offset & 2) * 8)) & 0xFFFF);
outpl(0xCF8, t1CF8);
outpl(0xCFC, t1CFC);
}
ASC_DBG1(4, "asc_get_cfg_word: config data: %x\n", tmp);
return tmp;
}
ASC_INITFUNC(
STATIC uchar
asc_get_cfg_byte(PCI_DATA *pciData)
)
{
uchar tmp;
ulong address;
ulong lbus = pciData->bus, lslot = pciData->slot, lfunc = pciData->func;
uchar t2CFA, t2CF8;
ulong t1CF8, t1CFC;
ASC_DBG1(4, "asc_get_cfg_byte: type: %d\n", pciData->type);
if (pciData->type == 2) {
t2CFA = inp(0xCFA);
t2CF8 = inp(0xCF8);
outp(0xCFA, pciData->bus);
outp(0xCF8, 0x10 | (pciData->func << 1));
tmp = inp(0xC000 | ((pciData->slot << 8) + pciData->offset));
outp(0xCF8, t2CF8);
outp(0xCFA, t2CFA);
} else {
t1CF8 = inpl(0xCF8);
t1CFC = inpl(0xCFC);
address = (ulong) ((lbus << 16) | (lslot << 11) |
(lfunc << 8) | (pciData->offset & 0xFC) | 0x80000000L);
outpl(0xCF8, address);
tmp = (uchar) ((inpl(0xCFC) >> ((pciData->offset & 3) * 8)) & 0xFF);
outpl(0xCF8, t1CF8);
outpl(0xCFC, t1CFC);
}
ASC_DBG1(4, "asc_get_cfg_byte: config data: %x\n", tmp);
return tmp;
}
ASC_INITFUNC(
STATIC void
asc_put_cfg_byte(PCI_DATA *pciData, uchar byte_data)
)
{
ulong tmpl;
ulong address;
ulong lbus = pciData->bus, lslot = pciData->slot, lfunc = pciData->func;
uchar t2CFA, t2CF8;
ulong t1CF8, t1CFC;
ASC_DBG2(4, "asc_put_cfg_byte: type: %d, byte_data %x\n",
pciData->type, byte_data);
if (pciData->type == 2) {
t2CFA = inp(0xCFA);
t2CF8 = inp(0xCF8);
outp(0xCFA, pciData->bus);
outp(0xCF8, 0x10 | (pciData->func << 1));
outp(0xC000 | ((pciData->slot << 8) + pciData->offset), byte_data);
outp(0xCF8, t2CF8);
outp(0xCFA, t2CFA);
} else {
t1CF8 = inpl(0xCF8);
t1CFC = inpl(0xCFC);
address = (ulong) ((lbus << 16) | (lslot << 11) | (lfunc << 8) |
(pciData->offset & 0xFC) | 0x80000000L);
outpl(0xCF8, address);
tmpl = inpl(0xCFC) & ~(0xFF << ((pciData->offset & 3) * 8));
outpl(0xCFC, tmpl | (byte_data << ((pciData->offset & 3) * 8)));
outpl(0xCF8, t1CF8);
outpl(0xCFC, t1CFC);
}
ASC_DBG(4, "asc_put_cfg_byte: end\n");
}
#endif
#endif
STATIC void
asc_enqueue(asc_queue_t *ascq, REQP reqp, int flag)
{
int        tid;
ASC_DBG3(3, "asc_enqueue: ascq %x, reqp %x, flag %d\n",
(unsigned) ascq, (unsigned) reqp, flag);
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
ASC_ASSERT(reqp != NULL);
ASC_ASSERT(flag == ASC_FRONT || flag == ASC_BACK);
tid = REQPTID(reqp);
ASC_ASSERT(tid >= 0 && tid <= ADV_MAX_TID);
if (flag == ASC_FRONT) {
REQPNEXT(reqp) = (unsigned char *) ascq->q_first[tid];
ascq->q_first[tid] = reqp;
if (ascq->q_last[tid] == NULL) {
ascq->q_last[tid] = reqp;
}
} else {
if (ascq->q_last[tid] != NULL) {
REQPNEXT(ascq->q_last[tid]) = (unsigned char *) reqp;
}
ascq->q_last[tid] = reqp;
REQPNEXT(reqp) = NULL;
if (ascq->q_first[tid] == NULL) {
ascq->q_first[tid] = reqp;
}
}
ascq->q_tidmask |= ADV_TID_TO_TIDMASK(tid);
#ifdef ADVANSYS_STATS
ascq->q_tot_cnt[tid]++;
ascq->q_cur_cnt[tid]++;
if (ascq->q_cur_cnt[tid] > ascq->q_max_cnt[tid]) {
ascq->q_max_cnt[tid] = ascq->q_cur_cnt[tid];
ASC_DBG2(2, "asc_enqueue: new q_max_cnt[%d] %d\n",
tid, ascq->q_max_cnt[tid]);
}
REQPTIME(reqp) = REQTIMESTAMP();
#endif
ASC_DBG1(3, "asc_enqueue: reqp %x\n", (unsigned) reqp);
return;
}
STATIC REQP
asc_dequeue(asc_queue_t *ascq, int tid)
{
REQP    reqp;
ASC_DBG2(3, "asc_dequeue: ascq %x, tid %d\n", (unsigned) ascq, tid);
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
ASC_ASSERT(tid >= 0 && tid <= ADV_MAX_TID);
if ((reqp = ascq->q_first[tid]) != NULL) {
ASC_ASSERT(ascq->q_tidmask & ADV_TID_TO_TIDMASK(tid));
ascq->q_first[tid] = (REQP) REQPNEXT(reqp);
if (ascq->q_first[tid] == NULL) {
ascq->q_tidmask &= ~ADV_TID_TO_TIDMASK(tid);
ASC_ASSERT(ascq->q_last[tid] == reqp);
ascq->q_last[tid] = NULL;
}
#ifdef ADVANSYS_STATS
ascq->q_cur_cnt[tid]--;
ASC_ASSERT(ascq->q_cur_cnt[tid] >= 0);
REQTIMESTAT("asc_dequeue", ascq, reqp, tid);
#endif
}
ASC_DBG1(3, "asc_dequeue: reqp %x\n", (unsigned) reqp);
return reqp;
}
STATIC REQP
asc_dequeue_list(asc_queue_t *ascq, REQP *lastpp, int tid)
{
REQP    firstp, lastp;
int     i;
ASC_DBG2(3, "asc_dequeue_list: ascq %x, tid %d\n", (unsigned) ascq, tid);
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
ASC_ASSERT((tid == ASC_TID_ALL) || (tid >= 0 && tid <= ADV_MAX_TID));
if (tid != ASC_TID_ALL) {
if ((ascq->q_tidmask & ADV_TID_TO_TIDMASK(tid)) == 0) {
firstp = lastp = NULL;
} else {
firstp = ascq->q_first[tid];
lastp = ascq->q_last[tid];
ascq->q_first[tid] = ascq->q_last[tid] = NULL;
ascq->q_tidmask &= ~ADV_TID_TO_TIDMASK(tid);
#ifdef ADVANSYS_STATS
{
REQP reqp;
ascq->q_cur_cnt[tid] = 0;
for (reqp = firstp; reqp; reqp = (REQP) REQPNEXT(reqp)) {
REQTIMESTAT("asc_dequeue_list", ascq, reqp, tid);
}
}
#endif
}
} else {
firstp = lastp = NULL;
for (i = 0; i <= ADV_MAX_TID; i++) {
if (ascq->q_tidmask & ADV_TID_TO_TIDMASK(i)) {
if (firstp == NULL) {
firstp = ascq->q_first[i];
lastp = ascq->q_last[i];
} else {
ASC_ASSERT(lastp != NULL);
REQPNEXT(lastp) = (unsigned char *) ascq->q_first[i];
lastp = ascq->q_last[i];
}
ascq->q_first[i] = ascq->q_last[i] = NULL;
ascq->q_tidmask &= ~ADV_TID_TO_TIDMASK(i);
#ifdef ADVANSYS_STATS
ascq->q_cur_cnt[i] = 0;
#endif
}
}
#ifdef ADVANSYS_STATS
{
REQP reqp;
for (reqp = firstp; reqp; reqp = (REQP) REQPNEXT(reqp)) {
REQTIMESTAT("asc_dequeue_list", ascq, reqp, reqp->target);
}
}
#endif
}
if (lastpp) {
*lastpp = lastp;
}
ASC_DBG1(3, "asc_dequeue_list: firstp %x\n", (unsigned) firstp);
return firstp;
}
STATIC int
asc_rmqueue(asc_queue_t *ascq, REQP reqp)
{
REQP        currp, prevp;
int         tid;
int         ret = ASC_FALSE;
ASC_DBG2(3, "asc_rmqueue: ascq %x, reqp %x\n",
(unsigned) ascq, (unsigned) reqp);
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
ASC_ASSERT(reqp != NULL);
tid = REQPTID(reqp);
ASC_ASSERT(tid >= 0 && tid <= ADV_MAX_TID);
if (reqp == ascq->q_first[tid]) {
ret = ASC_TRUE;
ascq->q_first[tid] = (REQP) REQPNEXT(reqp);
if (ascq->q_first[tid] == NULL) {
ascq->q_tidmask &= ~ADV_TID_TO_TIDMASK(tid);
ASC_ASSERT(ascq->q_last[tid] == reqp);
ascq->q_last[tid] = NULL;
}
} else if (ascq->q_first[tid] != NULL) {
ASC_ASSERT(ascq->q_last[tid] != NULL);
for (prevp = ascq->q_first[tid], currp = (REQP) REQPNEXT(prevp);
currp; prevp = currp, currp = (REQP) REQPNEXT(currp)) {
if (currp == reqp) {
ret = ASC_TRUE;
REQPNEXT(prevp) = REQPNEXT(currp);
REQPNEXT(reqp) = NULL;
if (ascq->q_last[tid] == reqp) {
ascq->q_last[tid] = prevp;
}
break;
}
}
}
#ifdef ADVANSYS_STATS
if (ret == ASC_TRUE) {
ascq->q_cur_cnt[tid]--;
REQTIMESTAT("asc_rmqueue", ascq, reqp, tid);
}
ASC_ASSERT(ascq->q_cur_cnt[tid] >= 0);
#endif
ASC_DBG2(3, "asc_rmqueue: reqp %x, ret %d\n", (unsigned) reqp, ret);
return ret;
}
STATIC int
asc_isqueued(asc_queue_t *ascq, REQP reqp)
{
REQP        treqp;
int            tid;
int            ret = ASC_FALSE;
ASC_DBG2(3, "asc_isqueued: ascq %x, reqp %x\n",
(unsigned) ascq, (unsigned) reqp);
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
ASC_ASSERT(reqp != NULL);
tid = REQPTID(reqp);
ASC_ASSERT(tid >= 0 && tid <= ADV_MAX_TID);
for (treqp = ascq->q_first[tid]; treqp; treqp = (REQP) REQPNEXT(treqp)) {
ASC_ASSERT(ascq->q_tidmask & ADV_TID_TO_TIDMASK(tid));
if (treqp == reqp) {
ret = ASC_TRUE;
break;
}
}
ASC_DBG1(3, "asc_isqueued: ret %x\n", ret);
return ret;
}
STATIC void
asc_execute_queue(asc_queue_t *ascq)
{
ADV_SCSI_BIT_ID_TYPE    scan_tidmask;
REQP                    reqp;
int                     i;
ASC_DBG1(1, "asc_execute_queue: ascq %x\n", (unsigned) ascq);
ASC_ASSERT(interrupts_enabled() == ASC_FALSE);
scan_tidmask = ascq->q_tidmask;
do {
for (i = 0; i <= ADV_MAX_TID; i++) {
if (scan_tidmask & ADV_TID_TO_TIDMASK(i)) {
if ((reqp = asc_dequeue(ascq, i)) == NULL) {
scan_tidmask &= ~ADV_TID_TO_TIDMASK(i);
} else if (asc_execute_scsi_cmnd((Scsi_Cmnd *) reqp)
== ASC_BUSY) {
scan_tidmask &= ~ADV_TID_TO_TIDMASK(i);
asc_enqueue(ascq, reqp, ASC_FRONT);
}
}
}
} while (scan_tidmask);
return;
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,0)
STATIC int
asc_prt_board_devices(struct Scsi_Host *shp, char *cp, int cplen)
{
asc_board_t        *boardp;
int                leftlen;
int                totlen;
int                len;
int                chip_scsi_id;
int                i;
boardp = ASC_BOARDP(shp);
leftlen = cplen;
totlen = len = 0;
len = asc_prt_line(cp, leftlen,
"\nDevice Information for AdvanSys SCSI Host %d:\n", shp->host_no);
ASC_PRT_NEXT();
if (ASC_NARROW_BOARD(boardp)) {
chip_scsi_id = boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id;
} else {
chip_scsi_id = boardp->dvc_var.adv_dvc_var.chip_scsi_id;
}
len = asc_prt_line(cp, leftlen, "Target IDs Detected:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
if (boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) {
len = asc_prt_line(cp, leftlen, " %X,", i);
ASC_PRT_NEXT();
}
}
len = asc_prt_line(cp, leftlen, " (%X=Host Adapter)\n", chip_scsi_id);
ASC_PRT_NEXT();
return totlen;
}
STATIC int
asc_prt_adv_bios(struct Scsi_Host *shp, char *cp, int cplen)
{
asc_board_t        *boardp;
int                leftlen;
int                totlen;
int                len;
int                upgrade = ASC_FALSE;
ushort             major, minor, letter;
boardp = ASC_BOARDP(shp);
leftlen = cplen;
totlen = len = 0;
len = asc_prt_line(cp, leftlen, "\nROM BIOS Version: ");
ASC_PRT_NEXT();
if (boardp->bios_signature != 0x55AA) {
len = asc_prt_line(cp, leftlen, "Pre-3.1\n");
ASC_PRT_NEXT();
upgrade = ASC_TRUE;
} else {
major = (boardp->bios_version >> 12) & 0xF;
minor = (boardp->bios_version >> 8) & 0xF;
letter = (boardp->bios_version & 0xFF);
len = asc_prt_line(cp, leftlen, "%d.%d%c\n",
major, minor, letter >= 26 ? '?' : letter + 'A');
ASC_PRT_NEXT();
if (major < 3 || (major <= 3 && minor < 1) ||
(major <= 3 && minor <= 1 && letter < ('C'- 'A'))) {
upgrade = ASC_TRUE;
}
}
if (upgrade == ASC_TRUE) {
len = asc_prt_line(cp, leftlen,
"Newer version of ROM BIOS available: ftp:
ASC_PRT_NEXT();
}
return totlen;
}
STATIC int
asc_get_eeprom_string(ushort *serialnum, uchar *cp)
{
ushort      w, num;
if ((serialnum[1] & 0xFE00) != ((ushort) 0xAA << 8)) {
return ASC_FALSE;
} else {
w = serialnum[0];
if ((*cp = 'A' + ((w & 0xE000) >> 13)) == 'H') {
*cp += 0x8;
}
cp++;
*cp++ = 'A' + ((w & 0x1C00) >> 10);
num = w & 0x3FF;
*cp++ = '0' + (num / 100);
num %= 100;
*cp++ = '0' + (num / 10);
*cp++ = 'A' + (num % 10);
w = serialnum[1];
if (serialnum[2] & 0x8000) {
*cp++ = '8' + ((w & 0x1C0) >> 6);
} else {
*cp++ = '0' + ((w & 0x1C0) >> 6);
}
num = w & 0x003F;
*cp++ = '0' + num / 10;
num %= 10;
*cp++ = '0' + num;
w = serialnum[2] & 0x7FFF;
*cp++ = 'A' + (w / 1000);
num = w % 1000;
*cp++ = '0' + num / 100;
num %= 100;
*cp++ = '0' + num / 10;
num %= 10;
*cp++ = '0' + num;
*cp = '\0';
return ASC_TRUE;
}
}
STATIC int
asc_prt_asc_board_eeprom(struct Scsi_Host *shp, char *cp, int cplen)
{
asc_board_t        *boardp;
ASC_DVC_VAR        *asc_dvc_varp;
int                leftlen;
int                totlen;
int                len;
ASCEEP_CONFIG      *ep;
int                i;
int                isa_dma_speed[] = { 10, 8, 7, 6, 5, 4, 3, 2 };
uchar              serialstr[13];
boardp = ASC_BOARDP(shp);
asc_dvc_varp = &boardp->dvc_var.asc_dvc_var;
ep = &boardp->eep_config.asc_eep;
leftlen = cplen;
totlen = len = 0;
len = asc_prt_line(cp, leftlen,
"\nEEPROM Settings for AdvanSys SCSI Host %d:\n", shp->host_no);
ASC_PRT_NEXT();
if (asc_get_eeprom_string((ushort *) &ep->adapter_info[0], serialstr) ==
ASC_TRUE) {
len = asc_prt_line(cp, leftlen, " Serial Number: %s\n", serialstr);
ASC_PRT_NEXT();
} else {
if (ep->adapter_info[5] == 0xBB) {
len = asc_prt_line(cp, leftlen,
" Default Settings Used for EEPROM-less Adapter.\n");
ASC_PRT_NEXT();
} else {
len = asc_prt_line(cp, leftlen,
" Serial Number Signature Not Present.\n");
ASC_PRT_NEXT();
}
}
len = asc_prt_line(cp, leftlen,
" Host SCSI ID: %u, Host Queue Size: %u, Device Queue Size: %u\n",
ep->chip_scsi_id, ep->max_total_qng, ep->max_tag_qng);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" cntl %x, no_scam %x\n",
ep->cntl, ep->no_scam);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Target ID:           ");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %d", i);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Disconnects:         ");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->disc_enable & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Command Queuing:     ");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->use_cmd_qng & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Start Motor:         ");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->start_motor & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Synchronous Transfer:");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->init_sdtr & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
if (asc_dvc_varp->bus_type & ASC_IS_ISA) {
len = asc_prt_line(cp, leftlen,
" Host ISA DMA speed:   %d MB/S\n",
isa_dma_speed[ep->isa_dma_speed]);
ASC_PRT_NEXT();
}
return totlen;
}
STATIC int
asc_prt_adv_board_eeprom(struct Scsi_Host *shp, char *cp, int cplen)
{
asc_board_t        *boardp;
ADV_DVC_VAR        *adv_dvc_varp;
int                leftlen;
int                totlen;
int                len;
int                i;
char               *termstr;
uchar              serialstr[13];
ADVEEP_CONFIG      *ep;
boardp = ASC_BOARDP(shp);
adv_dvc_varp = &boardp->dvc_var.adv_dvc_var;
ep = &boardp->eep_config.adv_eep;
leftlen = cplen;
totlen = len = 0;
len = asc_prt_line(cp, leftlen,
"\nEEPROM Settings for AdvanSys SCSI Host %d:\n", shp->host_no);
ASC_PRT_NEXT();
if (asc_get_eeprom_string(&ep->serial_number_word1, serialstr) ==
ASC_TRUE) {
len = asc_prt_line(cp, leftlen, " Serial Number: %s\n", serialstr);
ASC_PRT_NEXT();
} else {
len = asc_prt_line(cp, leftlen,
" Serial Number Signature Not Present.\n");
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen,
" Host SCSI ID: %u, Host Queue Size: %u, Device Queue Size: %u\n",
ep->adapter_scsi_id, ep->max_host_qng, ep->max_dvc_qng);
ASC_PRT_NEXT();
switch (ep->termination) {
case 1:
termstr = "Low Off/High Off";
break;
case 2:
termstr = "Low Off/High On";
break;
case 3:
termstr = "Low On/High On";
break;
default:
case 0:
termstr = "Automatic";
break;
}
len = asc_prt_line(cp, leftlen,
" termination: %u (%s), bios_ctrl: %x\n",
ep->termination, termstr, ep->bios_ctrl);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Target ID:           ");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %X", i);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Disconnects:         ");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->disc_enable & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Command Queuing:     ");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->tagqng_able & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Start Motor:         ");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->start_motor & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Synchronous Transfer:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->sdtr_able & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Ultra Transfer:      ");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->ultra_able & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Wide Transfer:       ");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
len = asc_prt_line(cp, leftlen, " %c",
(ep->wdtr_able & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
return totlen;
}
STATIC int
asc_prt_driver_conf(struct Scsi_Host *shp, char *cp, int cplen)
{
asc_board_t            *boardp;
int                    leftlen;
int                    totlen;
int                    len;
int                    chip_scsi_id;
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
int                    i;
#endif
boardp = ASC_BOARDP(shp);
leftlen = cplen;
totlen = len = 0;
len = asc_prt_line(cp, leftlen,
"\nLinux Driver Configuration and Information for AdvanSys SCSI Host %d:\n",
shp->host_no);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,89)
" host_busy %u, last_reset %u, max_id %u, max_lun %u\n",
shp->host_busy, shp->last_reset, shp->max_id, shp->max_lun);
#else
" host_busy %u, last_reset %u, max_id %u, max_lun %u, max_channel %u\n",
shp->host_busy, shp->last_reset, shp->max_id, shp->max_lun,
shp->max_channel);
#endif
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,57)
" can_queue %d, this_id %d, sg_tablesize %u, cmd_per_lun %u\n",
shp->can_queue, shp->this_id, shp->sg_tablesize, shp->cmd_per_lun);
#else
" unique_id %d, can_queue %d, this_id %d, sg_tablesize %u, cmd_per_lun %u\n",
shp->unique_id, shp->can_queue, shp->this_id, shp->sg_tablesize,
shp->cmd_per_lun);
#endif
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,57)
" unchecked_isa_dma %d, loaded_as_module %d\n",
shp->unchecked_isa_dma, shp->loaded_as_module);
#else
" unchecked_isa_dma %d, use_clustering %d, loaded_as_module %d\n",
shp->unchecked_isa_dma, shp->use_clustering, shp->loaded_as_module);
#endif
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, " flags %x, last_reset %x, jiffies %x\n",
boardp->flags, boardp->last_reset, jiffies);
ASC_PRT_NEXT();
if (ASC_NARROW_BOARD(boardp)) {
chip_scsi_id = boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id;
} else {
chip_scsi_id = boardp->dvc_var.adv_dvc_var.chip_scsi_id;
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
if (boardp->flags & ASC_SELECT_QUEUE_DEPTHS) {
len = asc_prt_line(cp, leftlen, " queue_depth:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
if ((chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
if (boardp->device[i] == NULL) {
continue;
}
len = asc_prt_line(cp, leftlen, " %X:%d",
i, boardp->device[i]->queue_depth);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
}
#endif
#if ASC_QUEUE_FLOW_CONTROL
if (ASC_NARROW_BOARD(boardp)) {
len = asc_prt_line(cp, leftlen, " queue_curr_depth:");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
if ((boardp->asc_dvc_cfg.chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
if (boardp->device[i] == NULL) {
continue;
}
len = asc_prt_line(cp, leftlen, " %d:%d",
i, boardp->device[i]->queue_curr_depth);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, " queue_count:");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
if ((boardp->asc_dvc_cfg.chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
if (boardp->device[i] == NULL) {
continue;
}
len = asc_prt_line(cp, leftlen, " %d:%d",
i, boardp->device[i]->queue_count);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
}
#endif
return totlen;
}
STATIC int
asc_prt_asc_board_info(struct Scsi_Host *shp, char *cp, int cplen)
{
asc_board_t            *boardp;
int                    leftlen;
int                    totlen;
int                    len;
ASC_DVC_VAR            *v;
ASC_DVC_CFG            *c;
int                    i;
boardp = ASC_BOARDP(shp);
v = &boardp->dvc_var.asc_dvc_var;
c = &boardp->dvc_cfg.asc_dvc_cfg;
leftlen = cplen;
totlen = len = 0;
len = asc_prt_line(cp, leftlen,
"\nAsc Library Configuration and Statistics for AdvanSys SCSI Host %d:\n",
shp->host_no);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" chip_version %u, lib_version %x, lib_serial_no %u, mcode_date %x\n",
c->chip_version, c->lib_version, c->lib_serial_no, c->mcode_date);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" mcode_version %x, err_code %u\n",
c->mcode_version, v->err_code);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Total Command Pending: %d\n", v->cur_total_qng);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Command Queuing:");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
if ((boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
len = asc_prt_line(cp, leftlen, " %d:%c",
i, (v->use_tagged_qng & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Command Queue Pending:");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
if ((boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
len = asc_prt_line(cp, leftlen, " %d:%u", i, v->cur_dvc_qng[i]);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Command Queue Limit:");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
if ((boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
len = asc_prt_line(cp, leftlen, " %d:%u", i, v->max_dvc_qng[i]);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Command Queue Full:");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
if ((boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
if (boardp->queue_full & ADV_TID_TO_TIDMASK(i)) {
len = asc_prt_line(cp, leftlen, " %d:Y-%d",
i, boardp->queue_full_cnt[i]);
} else {
len = asc_prt_line(cp, leftlen, " %d:N", i);
}
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Synchronous Transfer:");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
if ((boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
len = asc_prt_line(cp, leftlen, " %d:%c",
i, (v->sdtr_done & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
for (i = 0; i <= ASC_MAX_TID; i++) {
uchar syn_period_ix;
if ((boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
if ((v->sdtr_done & ADV_TID_TO_TIDMASK(i)) == 0) {
continue;
}
syn_period_ix = (boardp->sdtr_data[i] >> 4) & (v->max_sdtr_index - 1);
len = asc_prt_line(cp, leftlen, "  %d:", i);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Transfer Period Factor: %d (%d.%d Mhz),",
v->sdtr_period_tbl[syn_period_ix],
250 / v->sdtr_period_tbl[syn_period_ix],
ASC_TENTHS(250, v->sdtr_period_tbl[syn_period_ix]));
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, " REQ/ACK Offset: %d\n",
boardp->sdtr_data[i] & ASC_SYN_MAX_OFFSET);
ASC_PRT_NEXT();
}
return totlen;
}
STATIC int
asc_prt_adv_board_info(struct Scsi_Host *shp, char *cp, int cplen)
{
asc_board_t            *boardp;
int                    leftlen;
int                    totlen;
int                    len;
int                    i;
ADV_DVC_VAR            *v;
ADV_DVC_CFG            *c;
AdvPortAddr            iop_base;
ushort                 chip_scsi_id;
ushort                 lramword;
uchar                  lrambyte;
ushort                 sdtr_able;
ushort                 period;
boardp = ASC_BOARDP(shp);
v = &boardp->dvc_var.adv_dvc_var;
c = &boardp->dvc_cfg.adv_dvc_cfg;
iop_base = v->iop_base;
chip_scsi_id = v->chip_scsi_id;
leftlen = cplen;
totlen = len = 0;
len = asc_prt_line(cp, leftlen,
"\nAdv Library Configuration and Statistics for AdvanSys SCSI Host %d:\n",
shp->host_no);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" iop_base %lx, cable_detect: %X, err_code %u, idle_cmd_done %u\n",
v->iop_base,
AdvReadWordRegister(iop_base, IOPW_SCSI_CFG1) & CABLE_DETECT,
v->err_code, v->idle_cmd_done);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" chip_version %u, lib_version %x, mcode_date %x, mcode_version %x\n",
c->chip_version, c->lib_version, c->mcode_date, c->mcode_version);
ASC_PRT_NEXT();
AdvReadWordLram(iop_base, ASC_MC_TAGQNG_ABLE, lramword);
len = asc_prt_line(cp, leftlen,
" Queuing Enabled:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
if ((chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
len = asc_prt_line(cp, leftlen, " %X:%c",
i, (lramword & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Queue Limit:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
if ((chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
AdvReadByteLram(iop_base, ASC_MC_NUMBER_OF_MAX_CMD + i, lrambyte);
len = asc_prt_line(cp, leftlen, " %X:%d", i, lrambyte);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Command Pending:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
if ((chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
AdvReadByteLram(iop_base, ASC_MC_NUMBER_OF_QUEUED_CMD + i, lrambyte);
len = asc_prt_line(cp, leftlen, " %X:%d", i, lrambyte);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
AdvReadWordLram(iop_base, ASC_MC_WDTR_ABLE, lramword);
len = asc_prt_line(cp, leftlen,
" Wide Enabled:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
if ((chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
len = asc_prt_line(cp, leftlen, " %X:%c",
i, (lramword & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" Transfer Bit Width:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
if ((chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
AdvReadWordLram(iop_base, ASC_MC_DEVICE_HSHK_CFG_TABLE + (2 * i),
lramword);
len = asc_prt_line(cp, leftlen, " %X:%d",
i, (lramword & 0x8000) ? 16 : 8);
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
AdvReadWordLram(iop_base, ASC_MC_SDTR_ABLE, sdtr_able);
len = asc_prt_line(cp, leftlen,
" Synchronous Enabled:");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
if ((chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
len = asc_prt_line(cp, leftlen, " %X:%c",
i, (sdtr_able & ADV_TID_TO_TIDMASK(i)) ? 'Y' : 'N');
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen, "\n");
ASC_PRT_NEXT();
for (i = 0; i <= ADV_MAX_TID; i++) {
AdvReadWordLram(iop_base, ASC_MC_DEVICE_HSHK_CFG_TABLE + (2 * i),
lramword);
lramword &= ~0x8000;
if ((chip_scsi_id == i) ||
((sdtr_able & ADV_TID_TO_TIDMASK(i)) == 0) ||
(lramword == 0)) {
continue;
}
len = asc_prt_line(cp, leftlen, "  %X:", i);
ASC_PRT_NEXT();
period = (((lramword >> 8) * 25) + 50)/4;
len = asc_prt_line(cp, leftlen,
" Transfer Period Factor: %d (%d.%d Mhz),",
period, 250/period, ASC_TENTHS(250, period));
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, " REQ/ACK Offset: %d\n",
lramword & 0x1F);
ASC_PRT_NEXT();
}
return totlen;
}
STATIC int
asc_proc_copy(off_t advoffset, off_t offset, char *curbuf, int leftlen,
char *cp, int cplen)
{
int cnt = 0;
ASC_DBG3(2, "asc_proc_copy: offset %d, advoffset %d, cplen %d\n",
(unsigned) offset, (unsigned) advoffset, cplen);
if (offset <= advoffset) {
cnt = ASC_MIN(cplen, leftlen);
ASC_DBG3(2, "asc_proc_copy: curbuf %x, cp %x, cnt %d\n",
(unsigned) curbuf, (unsigned) cp, cnt);
memcpy(curbuf, cp, cnt);
} else if (offset < advoffset + cplen) {
cnt = (advoffset + cplen) - offset;
cp = (cp + cplen) - cnt;
cnt = ASC_MIN(cnt, leftlen);
ASC_DBG3(2, "asc_proc_copy: curbuf %x, cp %x, cnt %d\n",
(unsigned) curbuf, (unsigned) cp, cnt);
memcpy(curbuf, cp, cnt);
}
return cnt;
}
STATIC int
asc_prt_line(char *buf, int buflen, char *fmt, ...)
{
va_list        args;
int            ret;
char        s[ASC_PRTLINE_SIZE];
va_start(args, fmt);
ret = vsprintf(s, fmt, args);
ASC_ASSERT(ret < ASC_PRTLINE_SIZE);
if (buf == NULL) {
(void) printk("%s", s);
ret = 0;
} else {
ret = ASC_MIN(buflen, ret);
memcpy(buf, s, ret);
}
va_end(args);
return ret;
}
#endif
STATIC void
DvcSleepMilliSecond(ulong n)
{
ulong i;
ASC_DBG1(4, "DvcSleepMilliSecond: %lu\n", n);
for (i = 0; i < n; i++) {
udelay(1000);
}
}
STATIC long
DvcEnterCritical(void)
{
long   flags;
save_flags(flags);
cli();
return flags;
}
STATIC void
DvcLeaveCritical(long flags)
{
restore_flags(flags);
}
STATIC ulong
DvcGetSGList(ASC_DVC_VAR *asc_dvc_sg, uchar *buf_addr, ulong buf_len,
ASC_SG_HEAD *asc_sg_head_ptr)
{
ulong buf_size;
buf_size = buf_len;
asc_sg_head_ptr->entry_cnt = 1;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
asc_sg_head_ptr->sg_list[0].addr = (ulong) buf_addr;
#else
asc_sg_head_ptr->sg_list[0].addr = virt_to_bus(buf_addr);
#endif
asc_sg_head_ptr->sg_list[0].bytes = buf_size;
return buf_size;
}
STATIC void
DvcPutScsiQ(PortAddr iop_base, ushort s_addr, ushort *outbuf, int words)
{
int    i;
ASC_DBG_PRT_HEX(2, "DvcPutScsiQ", (uchar *) outbuf, 2 * words);
AscSetChipLramAddr(iop_base, s_addr);
for (i = 0; i < words; i++, outbuf++) {
if (i == 2 || i == 10) {
continue;
}
AscSetChipLramDataNoSwap(iop_base, *outbuf);
}
}
STATIC void
DvcGetQinfo(PortAddr iop_base, ushort s_addr, ushort *inbuf, int words)
{
int    i;
AscSetChipLramAddr(iop_base, s_addr);
for (i = 0; i < words; i++, inbuf++) {
if (i == 5) {
continue;
}
*inbuf = AscGetChipLramDataNoSwap(iop_base);
}
ASC_DBG_PRT_HEX(2, "DvcGetQinfo", (uchar *) inbuf, 2 * words);
}
STATIC void
DvcOutPortWords(ushort iop_base, ushort *outbuf, int words)
{
int    i;
for (i = 0; i < words; i++, outbuf++)
outpw(iop_base, *outbuf);
}
STATIC void
DvcInPortWords(ushort iop_base, ushort *inbuf, int words)
{
int    i;
for (i = 0; i < words; i++, inbuf++)
*inbuf = inpw(iop_base);
}
STATIC void
DvcOutPortDWords(PortAddr port, ulong *pdw, int dwords)
{
int        i;
int        words;
ushort    *pw;
pw = (ushort *) pdw;
words = dwords << 1;
for(i = 0; i < words; i++, pw++) {
outpw(port, *pw);
}
return;
}
ASC_INITFUNC(
STATIC uchar
DvcReadPCIConfigByte(
ASC_DVC_VAR asc_ptr_type *asc_dvc,
ushort offset)
)
{
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
PCI_DATA    pciData;
pciData.bus = ASC_PCI_ID2BUS(asc_dvc->cfg->pci_slot_info);
pciData.slot = ASC_PCI_ID2DEV(asc_dvc->cfg->pci_slot_info);
pciData.func = ASC_PCI_ID2FUNC(asc_dvc->cfg->pci_slot_info);
pciData.offset = offset;
pciData.type = pci_scan_method;
return asc_get_cfg_byte(&pciData);
#else
return 0;
#endif
#else
#ifdef CONFIG_PCI
uchar byte_data;
pcibios_read_config_byte(ASC_PCI_ID2BUS(asc_dvc->cfg->pci_slot_info),
PCI_DEVFN(ASC_PCI_ID2DEV(asc_dvc->cfg->pci_slot_info),
ASC_PCI_ID2FUNC(asc_dvc->cfg->pci_slot_info)),
offset, &byte_data);
return byte_data;
#else
return 0;
#endif
#endif
}
ASC_INITFUNC(
STATIC void
DvcWritePCIConfigByte(
ASC_DVC_VAR asc_ptr_type *asc_dvc,
ushort offset,
uchar  byte_data)
)
{
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
PCI_DATA    pciData;
pciData.bus = ASC_PCI_ID2BUS(asc_dvc->cfg->pci_slot_info);
pciData.slot = ASC_PCI_ID2DEV(asc_dvc->cfg->pci_slot_info);
pciData.func = ASC_PCI_ID2FUNC(asc_dvc->cfg->pci_slot_info);
pciData.offset = offset;
pciData.type = pci_scan_method;
asc_put_cfg_byte(&pciData, byte_data);
#endif
#else
#ifdef CONFIG_PCI
pcibios_write_config_byte(ASC_PCI_ID2BUS(asc_dvc->cfg->pci_slot_info),
PCI_DEVFN(ASC_PCI_ID2DEV(asc_dvc->cfg->pci_slot_info),
ASC_PCI_ID2FUNC(asc_dvc->cfg->pci_slot_info)),
offset, byte_data);
#endif
#endif
}
ASC_INITFUNC(
STATIC ushort
AscGetChipBiosAddress(
PortAddr iop_base,
ushort bus_type
)
)
{
ushort  cfg_lsw ;
ushort  bios_addr ;
if (bus_type & ASC_IS_PCI)
{
return(0);
}
if((bus_type & ASC_IS_EISA) != 0)
{
cfg_lsw = AscGetEisaChipCfg(iop_base) ;
cfg_lsw &= 0x000F ;
bios_addr = (ushort)(ASC_BIOS_MIN_ADDR  +
(cfg_lsw * ASC_BIOS_BANK_SIZE)) ;
return(bios_addr) ;
}
cfg_lsw = AscGetChipCfgLsw(iop_base) ;
if (bus_type == ASC_IS_ISAPNP)
{
cfg_lsw &= 0x7FFF;
}
bios_addr = (ushort)(((cfg_lsw >> 12) * ASC_BIOS_BANK_SIZE) +
ASC_BIOS_MIN_ADDR) ;
return(bios_addr) ;
}
ulong
DvcGetPhyAddr(ADV_DVC_VAR *asc_dvc, ADV_SCSI_REQ_Q *scsiq,
uchar *vaddr, long *lenp, int flag)
{
ulong                paddr;
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,0,0)
paddr = (ulong) vaddr;
#else
paddr = virt_to_bus(vaddr);
#endif
ASC_DBG4(4,
"DvcGetPhyAddr: vaddr 0x%lx, lenp 0x%lx *lenp %lu, paddr 0x%lx\n",
(ulong) vaddr, (ulong) lenp, (ulong) *((ulong *) lenp), paddr);
return paddr;
}
ASC_INITFUNC(
STATIC uchar
DvcAdvReadPCIConfigByte(
ADV_DVC_VAR *asc_dvc,
ushort offset)
)
{
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
PCI_DATA    pciData;
pciData.bus = ASC_PCI_ID2BUS(asc_dvc->cfg->pci_slot_info);
pciData.slot = ASC_PCI_ID2DEV(asc_dvc->cfg->pci_slot_info);
pciData.func = ASC_PCI_ID2FUNC(asc_dvc->cfg->pci_slot_info);
pciData.offset = offset;
pciData.type = pci_scan_method;
return asc_get_cfg_byte(&pciData);
#else
return 0;
#endif
#else
#ifdef CONFIG_PCI
uchar byte_data;
pcibios_read_config_byte(ASC_PCI_ID2BUS(asc_dvc->cfg->pci_slot_info),
PCI_DEVFN(ASC_PCI_ID2DEV(asc_dvc->cfg->pci_slot_info),
ASC_PCI_ID2FUNC(asc_dvc->cfg->pci_slot_info)),
offset, &byte_data);
return byte_data;
#else
return 0;
#endif
#endif
}
ASC_INITFUNC(
STATIC void
DvcAdvWritePCIConfigByte(
ADV_DVC_VAR *asc_dvc,
ushort offset,
uchar  byte_data)
)
{
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,93)
#ifdef ASC_CONFIG_PCI
PCI_DATA    pciData;
pciData.bus = ASC_PCI_ID2BUS(asc_dvc->cfg->pci_slot_info);
pciData.slot = ASC_PCI_ID2DEV(asc_dvc->cfg->pci_slot_info);
pciData.func = ASC_PCI_ID2FUNC(asc_dvc->cfg->pci_slot_info);
pciData.offset = offset;
pciData.type = pci_scan_method;
asc_put_cfg_byte(&pciData, byte_data);
#endif
#else
#ifdef CONFIG_PCI
pcibios_write_config_byte(ASC_PCI_ID2BUS(asc_dvc->cfg->pci_slot_info),
PCI_DEVFN(ASC_PCI_ID2DEV(asc_dvc->cfg->pci_slot_info),
ASC_PCI_ID2FUNC(asc_dvc->cfg->pci_slot_info)),
offset, byte_data);
#endif
#endif
}
#ifdef ADVANSYS_STATS
STATIC int
asc_prt_board_stats(struct Scsi_Host *shp, char *cp, int cplen)
{
int                    leftlen;
int                    totlen;
int                    len;
struct asc_stats       *s;
int                    i;
ushort                 chip_scsi_id;
asc_board_t            *boardp;
asc_queue_t            *active;
asc_queue_t            *waiting;
leftlen = cplen;
totlen = len = 0;
boardp = ASC_BOARDP(shp);
s = &boardp->asc_stats;
len = asc_prt_line(cp, leftlen,
"\nLinux Driver Statistics for AdvanSys SCSI Host %d:\n", shp->host_no);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" command %lu, queuecommand %lu, abort %lu, reset %lu, biosparam %lu\n",
s->command, s->queuecommand, s->abort, s->reset, s->biosparam);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" interrupt %lu, callback %lu, done %lu\n",
s->interrupt, s->callback, s->done);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
" exe_noerror %lu, exe_busy %lu, exe_error %lu, exe_unknown %lu\n",
s->exe_noerror, s->exe_busy, s->exe_error, s->exe_unknown);
ASC_PRT_NEXT();
if (ASC_NARROW_BOARD(boardp)) {
len = asc_prt_line(cp, leftlen,
" build_error %lu\n",
s->build_error);
} else {
len = asc_prt_line(cp, leftlen,
" build_error %lu, build_noreq %lu, build_nosg %lu\n",
s->build_error, s->adv_build_noreq, s->adv_build_nosg);
}
ASC_PRT_NEXT();
if (s->cont_cnt > 0) {
len = asc_prt_line(cp, leftlen, " cont_cnt %lu, ", s->cont_cnt);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, "cont_xfer %lu.%01lu kb ",
s->cont_xfer/2,
ASC_TENTHS(s->cont_xfer, 2));
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, "avg_xfer %lu.%01lu kb\n",
(s->cont_xfer/2)/s->cont_cnt,
ASC_TENTHS((s->cont_xfer/2), s->cont_cnt));
ASC_PRT_NEXT();
}
if (s->sg_cnt > 0) {
len = asc_prt_line(cp, leftlen, " sg_cnt %lu, sg_elem %lu, ",
s->sg_cnt, s->sg_elem);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, "sg_xfer %lu.%01lu kb\n",
s->sg_xfer/2,
ASC_TENTHS(s->sg_xfer, 2));
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, " avg_num_elem %lu.%01lu, ",
s->sg_elem/s->sg_cnt,
ASC_TENTHS(s->sg_elem, s->sg_cnt));
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, "avg_elem_size %lu.%01lu kb, ",
(s->sg_xfer/2)/s->sg_elem,
ASC_TENTHS((s->sg_xfer/2), s->sg_elem));
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen, "avg_xfer_size %lu.%01lu kb\n",
(s->sg_xfer/2)/s->sg_cnt,
ASC_TENTHS((s->sg_xfer/2), s->sg_cnt));
ASC_PRT_NEXT();
}
len = asc_prt_line(cp, leftlen,
" Active and Waiting Request Queues (Time Unit: %d HZ):\n", HZ);
ASC_PRT_NEXT();
active = &ASC_BOARDP(shp)->active;
waiting = &ASC_BOARDP(shp)->waiting;
if (ASC_NARROW_BOARD(boardp)) {
chip_scsi_id = boardp->dvc_cfg.asc_dvc_cfg.chip_scsi_id;
} else {
chip_scsi_id = boardp->dvc_var.adv_dvc_var.chip_scsi_id;
}
for (i = 0; i <= ADV_MAX_TID; i++) {
if ((chip_scsi_id == i) ||
((boardp->init_tidmask & ADV_TID_TO_TIDMASK(i)) == 0)) {
continue;
}
if (active->q_tot_cnt[i] > 0 || waiting->q_tot_cnt[i] > 0) {
len = asc_prt_line(cp, leftlen, " target %d\n", i);
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
"   active: cnt [cur %d, max %d, tot %u], time [min %d, max %d, avg %lu.%01lu]\n",
active->q_cur_cnt[i], active->q_max_cnt[i],
active->q_tot_cnt[i],
active->q_min_tim[i], active->q_max_tim[i],
(active->q_tot_cnt[i] == 0) ? 0 :
(active->q_tot_tim[i]/active->q_tot_cnt[i]),
(active->q_tot_cnt[i] == 0) ? 0 :
ASC_TENTHS(active->q_tot_tim[i], active->q_tot_cnt[i]));
ASC_PRT_NEXT();
len = asc_prt_line(cp, leftlen,
"   waiting: cnt [cur %d, max %d, tot %u], time [min %u, max %u, avg %lu.%01lu]\n",
waiting->q_cur_cnt[i], waiting->q_max_cnt[i],
waiting->q_tot_cnt[i],
waiting->q_min_tim[i], waiting->q_max_tim[i],
(waiting->q_tot_cnt[i] == 0) ? 0 :
(waiting->q_tot_tim[i]/waiting->q_tot_cnt[i]),
(waiting->q_tot_cnt[i] == 0) ? 0 :
ASC_TENTHS(waiting->q_tot_tim[i], waiting->q_tot_cnt[i]));
ASC_PRT_NEXT();
}
}
return totlen;
}
#endif
#ifdef ADVANSYS_DEBUG
STATIC void
asc_prt_scsi_host(struct Scsi_Host *s)
{
asc_board_t         *boardp;
boardp = ASC_BOARDP(s);
printk("Scsi_Host at addr %x\n", (unsigned) s);
printk(
" next %x, extra_bytes %u, host_busy %u, host_no %d, last_reset %d,\n",
(unsigned) s->next, s->extra_bytes, s->host_busy, s->host_no,
(unsigned) s->last_reset);
printk(
" host_wait %x, host_queue %x, hostt %x, block %x,\n",
(unsigned) s->host_wait, (unsigned) s->host_queue,
(unsigned) s->hostt, (unsigned) s->block);
printk(
" wish_block %d, base %x, io_port %d, n_io_port %d, irq %d, dma_channel %d,\n",
s->wish_block, (unsigned) s->base, s->io_port, s->n_io_port,
s->irq, s->dma_channel);
printk(
" this_id %d, can_queue %d,\n", s->this_id, s->can_queue);
printk(
" cmd_per_lun %d, sg_tablesize %d, unchecked_isa_dma %d, loaded_as_module %d\n",
s->cmd_per_lun, s->sg_tablesize, s->unchecked_isa_dma,
s->loaded_as_module);
if (ASC_NARROW_BOARD(boardp)) {
asc_prt_asc_dvc_var(&ASC_BOARDP(s)->dvc_var.asc_dvc_var);
asc_prt_asc_dvc_cfg(&ASC_BOARDP(s)->dvc_cfg.asc_dvc_cfg);
} else {
asc_prt_adv_dvc_var(&ASC_BOARDP(s)->dvc_var.adv_dvc_var);
asc_prt_adv_dvc_cfg(&ASC_BOARDP(s)->dvc_cfg.adv_dvc_cfg);
}
}
STATIC void
asc_prt_scsi_cmnd(Scsi_Cmnd *s)
{
printk("Scsi_Cmnd at addr %x\n", (unsigned) s);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
printk(
" host %x, device %x, target %u, lun %u\n",
(unsigned) s->host, (unsigned) s->device, s->target, s->lun);
#else
printk(
" host %x, device %x, target %u, lun %u, channel %u,\n",
(unsigned) s->host, (unsigned) s->device, s->target, s->lun,
s->channel);
#endif
asc_prt_hex(" CDB", s->cmnd, s->cmd_len);
printk(
" use_sg %u, sglist_len %u, abort_reason %x\n",
s->use_sg, s->sglist_len, s->abort_reason);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,89)
printk(
" retries %d, allowed %d\n",
s->retries, s->allowed);
#else
printk(
" serial_number %x, serial_number_at_timeout %x, retries %d, allowed %d\n",
(unsigned) s->serial_number, (unsigned) s->serial_number_at_timeout,
s->retries, s->allowed);
#endif
printk(
" timeout_per_command %d, timeout_total %d, timeout %d\n",
s->timeout_per_command, s->timeout_total, s->timeout);
printk(
" internal_timeout %u, flags %u, this_count %d\n",
s->internal_timeout, s->flags, s->this_count);
printk(
" scsi_done %x, done %x, host_scribble %x, result %x\n",
(unsigned) s->scsi_done, (unsigned) s->done,
(unsigned) s->host_scribble, s->result);
printk(
" tag %u, pid %u\n",
(unsigned) s->tag, (unsigned) s->pid);
}
STATIC void
asc_prt_asc_dvc_var(ASC_DVC_VAR *h)
{
printk("ASC_DVC_VAR at addr %x\n", (unsigned) h);
printk(
" iop_base %x, err_code %x, dvc_cntl %x, bug_fix_cntl %d,\n",
h->iop_base, h->err_code, h->dvc_cntl, h->bug_fix_cntl);
printk(
" bus_type %d, isr_callback %x, exe_callback %x, init_sdtr %x,\n",
h->bus_type, (unsigned) h->isr_callback, (unsigned) h->exe_callback,
(unsigned) h->init_sdtr);
printk(
" sdtr_done %x, use_tagged_qng %x, unit_not_ready %x, chip_no %x,\n",
(unsigned) h->sdtr_done, (unsigned) h->use_tagged_qng,
(unsigned) h->unit_not_ready, (unsigned) h->chip_no);
printk(
" queue_full_or_busy %x, start_motor %x, scsi_reset_wait %x, irq_no %x,\n",
(unsigned) h->queue_full_or_busy, (unsigned) h->start_motor,
(unsigned) h->scsi_reset_wait, (unsigned) h->irq_no);
printk(
" is_in_int %x, max_total_qng %x, cur_total_qng %x, in_critical_cnt %x,\n",
(unsigned) h->is_in_int, (unsigned) h->max_total_qng,
(unsigned) h->cur_total_qng, (unsigned) h->in_critical_cnt);
printk(
" last_q_shortage %x, init_state %x, no_scam %x, pci_fix_asyn_xfer %x,\n",
(unsigned) h->last_q_shortage, (unsigned) h->init_state,
(unsigned) h->no_scam, (unsigned) h->pci_fix_asyn_xfer);
printk(
" cfg %x, saved_ptr2func %x\n",
(unsigned) h->cfg, (unsigned) h->saved_ptr2func);
}
STATIC void
asc_prt_asc_dvc_cfg(ASC_DVC_CFG *h)
{
printk("ASC_DVC_CFG at addr %x\n", (unsigned) h);
printk(
" can_tagged_qng %x, cmd_qng_enabled %x, disc_enable %x, sdtr_enable %x,\n",
h->can_tagged_qng, h->cmd_qng_enabled, h->disc_enable,
h->sdtr_enable);
printk(
" chip_scsi_id %d, isa_dma_speed %d, isa_dma_channel %d, chip_version %d,\n",
h->chip_scsi_id, h->isa_dma_speed, h->isa_dma_channel,
h->chip_version);
printk(
" pci_device_id %d, lib_serial_no %x, lib_version %x, mcode_date %x,\n",
h->pci_device_id, h->lib_serial_no, h->lib_version, h->mcode_date);
printk(
" mcode_version %d, overrun_buf %x\n",
h->mcode_version, (unsigned) h->overrun_buf);
}
STATIC void
asc_prt_asc_scsi_q(ASC_SCSI_Q *q)
{
ASC_SG_HEAD    *sgp;
int i;
printk("ASC_SCSI_Q at addr %x\n", (unsigned) q);
printk(
" target_ix %u, target_lun %u, srb_ptr %x, tag_code %u,\n",
q->q2.target_ix, q->q1.target_lun,
(unsigned) q->q2.srb_ptr, q->q2.tag_code);
printk(
" data_addr %x, data_cnt %lu, sense_addr %x, sense_len %u,\n",
(unsigned) q->q1.data_addr, q->q1.data_cnt,
(unsigned) q->q1.sense_addr, q->q1.sense_len);
printk(
" cdbptr %x, cdb_len %u, sg_head %x, sg_queue_cnt %u\n",
(unsigned) q->cdbptr, q->q2.cdb_len,
(unsigned) q->sg_head, q->q1.sg_queue_cnt);
if (q->sg_head) {
sgp = q->sg_head;
printk("ASC_SG_HEAD at addr %x\n", (unsigned) sgp);
printk(" entry_cnt %u, queue_cnt %u\n", sgp->entry_cnt, sgp->queue_cnt);
for (i = 0; i < sgp->entry_cnt; i++) {
printk(" [%u]: addr %x, bytes %lu\n",
i, (unsigned) sgp->sg_list[i].addr, sgp->sg_list[i].bytes);
}
}
}
STATIC void
asc_prt_asc_qdone_info(ASC_QDONE_INFO *q)
{
printk("ASC_QDONE_INFO at addr %x\n", (unsigned) q);
printk(
" srb_ptr %x, target_ix %u, cdb_len %u, tag_code %u, done_stat %x\n",
(unsigned) q->d2.srb_ptr, q->d2.target_ix, q->d2.cdb_len,
q->d2.tag_code, q->d3.done_stat);
printk(
" host_stat %x, scsi_stat %x, scsi_msg %x\n",
q->d3.host_stat, q->d3.scsi_stat, q->d3.scsi_msg);
}
STATIC void
asc_prt_adv_dvc_var(ADV_DVC_VAR *h)
{
printk(" ADV_DVC_VAR at addr 0x%lx\n", (ulong) h);
printk(
"  iop_base 0x%lx, err_code 0x%x, ultra_able 0x%x\n",
(ulong) h->iop_base, h->err_code, (unsigned) h->ultra_able);
printk(
"  isr_callback 0x%x, sdtr_able 0x%x, wdtr_able 0x%x\n",
(unsigned) h->isr_callback, (unsigned) h->wdtr_able,
(unsigned) h->sdtr_able);
printk(
"  start_motor 0x%x, scsi_reset_wait 0x%x, irq_no 0x%x,\n",
(unsigned) h->start_motor,
(unsigned) h->scsi_reset_wait, (unsigned) h->irq_no);
printk(
"  max_host_qng 0x%x, cur_host_qng 0x%x, max_dvc_qng 0x%x\n",
(unsigned) h->max_host_qng, (unsigned) h->cur_host_qng,
(unsigned) h->max_dvc_qng);
printk(
"  no_scam 0x%x, tagqng_able 0x%x, chip_scsi_id 0x%x, cfg 0x%lx\n",
(unsigned) h->no_scam, (unsigned) h->tagqng_able,
(unsigned) h->chip_scsi_id, (ulong) h->cfg);
}
STATIC void
asc_prt_adv_dvc_cfg(ADV_DVC_CFG *h)
{
printk(" ADV_DVC_CFG at addr 0x%lx\n", (ulong) h);
printk(
"  disc_enable 0x%x, termination 0x%x\n",
h->disc_enable, h->termination);
printk(
"  chip_version 0x%x, mcode_date 0x%x\n",
h->chip_version, h->mcode_date);
printk(
"  mcode_version 0x%x, pci_device_id 0x%x, lib_version 0x%x\n",
h->mcode_version, h->pci_device_id, h->lib_version);
printk(
"  control_flag 0x%x, pci_slot_info 0x%x\n",
h->control_flag, h->pci_slot_info);
}
STATIC void
asc_prt_adv_scsi_req_q(ADV_SCSI_REQ_Q *q)
{
int                 i;
struct asc_sg_block *sg_ptr;
printk("ADV_SCSI_REQ_Q at addr %x\n", (unsigned) q);
printk(
"  target_id %u, target_lun %u, srb_ptr 0x%lx, a_flag 0x%x\n",
q->target_id, q->target_lun, q->srb_ptr, q->a_flag);
printk("  cntl 0x%x, data_addr 0x%lx, vdata_addr 0x%lx\n",
q->cntl, q->data_addr, q->vdata_addr);
printk(
"  data_cnt %lu, sense_addr 0x%lx, sense_len %u,\n",
q->data_cnt, q->sense_addr, q->sense_len);
printk(
"  cdb_len %u, done_status 0x%x, host_status 0x%x, scsi_status 0x%x\n",
q->cdb_len, q->done_status, q->host_status, q->scsi_status);
printk(
"  vsense_addr 0x%lx, scsiq_ptr 0x%lx, ux_wk_data_cnt %lu\n",
(ulong) q->vsense_addr, (ulong) q->scsiq_ptr,
(ulong) q->ux_wk_data_cnt);
printk(
"  sg_list_ptr 0x%lx, sg_real_addr 0x%lx, sg_entry_cnt %u\n",
(ulong) q->sg_list_ptr, (ulong) q->sg_real_addr, q->sg_entry_cnt);
printk(
"  ux_sg_ix %u, orig_sense_len %u\n",
q->ux_sg_ix, q->orig_sense_len);
for (sg_ptr = q->sg_list_ptr, i = 0; sg_ptr != NULL;
sg_ptr = sg_ptr->sg_ptr, i++) {
sg_ptr = &(((ADV_SG_BLOCK *) (q->sg_list_ptr))[i]);
asc_prt_adv_sgblock(i, sg_ptr);
}
}
STATIC void
asc_prt_adv_sgblock(int sgblockno, ADV_SG_BLOCK *b)
{
int i, s;
s = sgblockno * NO_OF_SG_PER_BLOCK;
printk(" ADV_SG_BLOCK at addr 0x%lx (sgblockno %lu)\n",
(ulong) b, (ulong) sgblockno);
printk(
"  first_entry_no %lu, last_entry_no %lu, sg_ptr 0x%lx\n",
(ulong) b->first_entry_no, (ulong) b->last_entry_no, (ulong) b->sg_ptr);
ASC_ASSERT(b->first_entry_no - s >= 0);
ASC_ASSERT(b->last_entry_no - s >= 0);
ASC_ASSERT(b->last_entry_no - s <= NO_OF_SG_PER_BLOCK);
ASC_ASSERT(b->first_entry_no - s <= NO_OF_SG_PER_BLOCK);
ASC_ASSERT(b->first_entry_no - s <= NO_OF_SG_PER_BLOCK);
ASC_ASSERT(b->first_entry_no - s <= b->last_entry_no - s);
for (i = b->first_entry_no - s; i <= b->last_entry_no - s; i++) {
printk("  [%lu]: sg_addr 0x%lx, sg_count 0x%lx\n",
(ulong) i, (ulong) b->sg_list[i].sg_addr,
(ulong) b->sg_list[i].sg_count);
}
}
STATIC void
asc_prt_hex(char *f, uchar *s, int l)
{
int            i;
int            j;
int            k;
int            m;
printk("%s: (%d bytes)\n", f, l);
for (i = 0; i < l; i += 32) {
if ((k = (l - i) / 4) >= 8) {
k = 8;
m = 0;
} else {
m = (l - i) % 4 ;
}
for (j = 0; j < k; j++) {
printk(" %2.2X%2.2X%2.2X%2.2X",
(unsigned) s[i+(j*4)], (unsigned) s[i+(j*4)+1],
(unsigned) s[i+(j*4)+2], (unsigned) s[i+(j*4)+3]);
}
switch (m) {
case 0:
default:
break;
case 1:
printk(" %2.2X",
(unsigned) s[i+(j*4)]);
break;
case 2:
printk(" %2.2X%2.2X",
(unsigned) s[i+(j*4)],
(unsigned) s[i+(j*4)+1]);
break;
case 3:
printk(" %2.2X%2.2X%2.2X",
(unsigned) s[i+(j*4)+1],
(unsigned) s[i+(j*4)+2],
(unsigned) s[i+(j*4)+3]);
break;
}
printk("\n");
}
}
#endif
#ifdef ADVANSYS_ASSERT
STATIC int
interrupts_enabled(void)
{
long flags;
save_flags(flags);
if (flags & 0x0200) {
return ASC_TRUE;
} else {
return ASC_FALSE;
}
}
#endif
ASC_INITFUNC(
STATIC ushort
AscGetEisaChipCfg(
PortAddr iop_base
)
)
{
PortAddr            eisa_cfg_iop;
eisa_cfg_iop = (PortAddr) ASC_GET_EISA_SLOT(iop_base) |
(PortAddr) (ASC_EISA_CFG_IOP_MASK);
return (inpw(eisa_cfg_iop));
}
ASC_INITFUNC(
STATIC uchar
AscSetChipScsiID(
PortAddr iop_base,
uchar new_host_id
)
)
{
ushort              cfg_lsw;
if (AscGetChipScsiID(iop_base) == new_host_id) {
return (new_host_id);
}
cfg_lsw = AscGetChipCfgLsw(iop_base);
cfg_lsw &= 0xF8FF;
cfg_lsw |= (ushort) ((new_host_id & ASC_MAX_TID) << 8);
AscSetChipCfgLsw(iop_base, cfg_lsw);
return (AscGetChipScsiID(iop_base));
}
ASC_INITFUNC(
STATIC uchar
AscGetChipScsiCtrl(
PortAddr iop_base
)
)
{
uchar               sc;
AscSetBank(iop_base, 1);
sc = inp(iop_base + IOP_REG_SC);
AscSetBank(iop_base, 0);
return (sc);
}
ASC_INITFUNC(
STATIC uchar
AscGetChipVersion(
PortAddr iop_base,
ushort bus_type
)
)
{
if ((bus_type & ASC_IS_EISA) != 0) {
PortAddr            eisa_iop;
uchar               revision;
eisa_iop = (PortAddr) ASC_GET_EISA_SLOT(iop_base) |
(PortAddr) ASC_EISA_REV_IOP_MASK;
revision = inp(eisa_iop);
return ((uchar) ((ASC_CHIP_MIN_VER_EISA - 1) + revision));
}
return (AscGetChipVerNo(iop_base));
}
ASC_INITFUNC(
STATIC ushort
AscGetChipBusType(
PortAddr iop_base
)
)
{
ushort              chip_ver;
chip_ver = AscGetChipVerNo(iop_base);
if (
(chip_ver >= ASC_CHIP_MIN_VER_VL)
&& (chip_ver <= ASC_CHIP_MAX_VER_VL)
) {
if (
((iop_base & 0x0C30) == 0x0C30)
|| ((iop_base & 0x0C50) == 0x0C50)
) {
return (ASC_IS_EISA);
}
return (ASC_IS_VL);
}
if ((chip_ver >= ASC_CHIP_MIN_VER_ISA) &&
(chip_ver <= ASC_CHIP_MAX_VER_ISA)) {
if (chip_ver >= ASC_CHIP_MIN_VER_ISA_PNP) {
return (ASC_IS_ISAPNP);
}
return (ASC_IS_ISA);
} else if ((chip_ver >= ASC_CHIP_MIN_VER_PCI) &&
(chip_ver <= ASC_CHIP_MAX_VER_PCI)) {
return (ASC_IS_PCI);
}
return (0);
}
ASC_INITFUNC(
STATIC ulong
AscLoadMicroCode(
PortAddr iop_base,
ushort s_addr,
ushort *mcode_buf,
ushort mcode_size
)
)
{
ulong               chksum;
ushort              mcode_word_size;
ushort              mcode_chksum;
mcode_word_size = (ushort) (mcode_size >> 1);
AscMemWordSetLram(iop_base, s_addr, 0, mcode_word_size);
AscMemWordCopyToLram(iop_base, s_addr, mcode_buf, mcode_word_size);
chksum = AscMemSumLramWord(iop_base, s_addr, mcode_word_size);
mcode_chksum = (ushort) AscMemSumLramWord(iop_base,
(ushort) ASC_CODE_SEC_BEG,
(ushort) ((mcode_size - s_addr - (ushort) ASC_CODE_SEC_BEG) / 2));
AscWriteLramWord(iop_base, ASCV_MCODE_CHKSUM_W, mcode_chksum);
AscWriteLramWord(iop_base, ASCV_MCODE_SIZE_W, mcode_size);
return (chksum);
}
ASC_INITFUNC(
STATIC int
AscFindSignature(
PortAddr iop_base
)
)
{
ushort              sig_word;
if (AscGetChipSignatureByte(iop_base) == (uchar) ASC_1000_ID1B) {
sig_word = AscGetChipSignatureWord(iop_base);
if ((sig_word == (ushort) ASC_1000_ID0W) ||
(sig_word == (ushort) ASC_1000_ID0W_FIX)) {
return (1);
}
}
return (0);
}
STATIC uchar _isa_pnp_inited ASC_INITDATA = 0;
STATIC PortAddr _asc_def_iop_base[ASC_IOADR_TABLE_MAX_IX] ASC_INITDATA =
{
0x100, ASC_IOADR_1, 0x120, ASC_IOADR_2, 0x140, ASC_IOADR_3, ASC_IOADR_4,
ASC_IOADR_5, ASC_IOADR_6, ASC_IOADR_7, ASC_IOADR_8
};
ASC_INITFUNC(
STATIC PortAddr
AscSearchIOPortAddr(
PortAddr iop_beg,
ushort bus_type
)
)
{
if (bus_type & ASC_IS_VL) {
while ((iop_beg = AscSearchIOPortAddr11(iop_beg)) != 0) {
if (AscGetChipVersion(iop_beg, bus_type) <= ASC_CHIP_MAX_VER_VL) {
return (iop_beg);
}
}
return (0);
}
if (bus_type & ASC_IS_ISA) {
if (_isa_pnp_inited == 0) {
AscSetISAPNPWaitForKey();
_isa_pnp_inited++;
}
while ((iop_beg = AscSearchIOPortAddr11(iop_beg)) != 0) {
if ((AscGetChipVersion(iop_beg, bus_type) & ASC_CHIP_VER_ISA_BIT) != 0) {
return (iop_beg);
}
}
return (0);
}
if (bus_type & ASC_IS_EISA) {
if ((iop_beg = AscSearchIOPortAddrEISA(iop_beg)) != 0) {
return (iop_beg);
}
return (0);
}
return (0);
}
ASC_INITFUNC(
STATIC PortAddr
AscSearchIOPortAddr11(
PortAddr s_addr
)
)
{
int                 i;
PortAddr            iop_base;
for (i = 0; i < ASC_IOADR_TABLE_MAX_IX; i++) {
if (_asc_def_iop_base[i] > s_addr) {
break;
}
}
for (; i < ASC_IOADR_TABLE_MAX_IX; i++) {
iop_base = _asc_def_iop_base[i];
if (check_region(iop_base, ASC_IOADR_GAP) != 0) {
ASC_DBG1(1,
"AscSearchIOPortAddr11: check_region() failed I/O port %x\n",
iop_base);
continue;
}
ASC_DBG1(1, "AscSearchIOPortAddr11: probing I/O port %x\n", iop_base);
if (AscFindSignature(iop_base)) {
return (iop_base);
}
}
return (0);
}
ASC_INITFUNC(
STATIC void
AscToggleIRQAct(
PortAddr iop_base
)
)
{
AscSetChipStatus(iop_base, CIW_IRQ_ACT);
AscSetChipStatus(iop_base, 0);
return;
}
ASC_INITFUNC(
STATIC void
AscSetISAPNPWaitForKey(
void)
)
{
outp(ASC_ISA_PNP_PORT_ADDR, 0x02);
outp(ASC_ISA_PNP_PORT_WRITE, 0x02);
return;
}
ASC_INITFUNC(
STATIC uchar
AscGetChipIRQ(
PortAddr iop_base,
ushort bus_type
)
)
{
ushort              cfg_lsw;
uchar               chip_irq;
if ((bus_type & ASC_IS_EISA) != 0) {
cfg_lsw = AscGetEisaChipCfg(iop_base);
chip_irq = (uchar) (((cfg_lsw >> 8) & 0x07) + 10);
if ((chip_irq == 13) || (chip_irq > 15)) {
return (0);
}
return (chip_irq);
}
if ((bus_type & ASC_IS_VL) != 0) {
cfg_lsw = AscGetChipCfgLsw(iop_base);
chip_irq = (uchar) (((cfg_lsw >> 2) & 0x07));
if ((chip_irq == 0) ||
(chip_irq == 4) ||
(chip_irq == 7)) {
return (0);
}
return ((uchar) (chip_irq + (ASC_MIN_IRQ_NO - 1)));
}
cfg_lsw = AscGetChipCfgLsw(iop_base);
chip_irq = (uchar) (((cfg_lsw >> 2) & 0x03));
if (chip_irq == 3)
chip_irq += (uchar) 2;
return ((uchar) (chip_irq + ASC_MIN_IRQ_NO));
}
ASC_INITFUNC(
STATIC uchar
AscSetChipIRQ(
PortAddr iop_base,
uchar irq_no,
ushort bus_type
)
)
{
ushort              cfg_lsw;
if ((bus_type & ASC_IS_VL) != 0) {
if (irq_no != 0) {
if ((irq_no < ASC_MIN_IRQ_NO) || (irq_no > ASC_MAX_IRQ_NO)) {
irq_no = 0;
} else {
irq_no -= (uchar) ((ASC_MIN_IRQ_NO - 1));
}
}
cfg_lsw = (ushort) (AscGetChipCfgLsw(iop_base) & 0xFFE3);
cfg_lsw |= (ushort) 0x0010;
AscSetChipCfgLsw(iop_base, cfg_lsw);
AscToggleIRQAct(iop_base);
cfg_lsw = (ushort) (AscGetChipCfgLsw(iop_base) & 0xFFE0);
cfg_lsw |= (ushort) ((irq_no & 0x07) << 2);
AscSetChipCfgLsw(iop_base, cfg_lsw);
AscToggleIRQAct(iop_base);
return (AscGetChipIRQ(iop_base, bus_type));
}
if ((bus_type & (ASC_IS_ISA)) != 0) {
if (irq_no == 15)
irq_no -= (uchar) 2;
irq_no -= (uchar) ASC_MIN_IRQ_NO;
cfg_lsw = (ushort) (AscGetChipCfgLsw(iop_base) & 0xFFF3);
cfg_lsw |= (ushort) ((irq_no & 0x03) << 2);
AscSetChipCfgLsw(iop_base, cfg_lsw);
return (AscGetChipIRQ(iop_base, bus_type));
}
return (0);
}
ASC_INITFUNC(
STATIC void
AscEnableIsaDma(
uchar dma_channel
)
)
{
if (dma_channel < 4) {
outp(0x000B, (ushort) (0xC0 | dma_channel));
outp(0x000A, dma_channel);
} else if (dma_channel < 8) {
outp(0x00D6, (ushort) (0xC0 | (dma_channel - 4)));
outp(0x00D4, (ushort) (dma_channel - 4));
}
return;
}
STATIC int
AscIsrChipHalted(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc
)
{
EXT_MSG             ext_msg;
EXT_MSG             out_msg;
ushort              halt_q_addr;
int                 sdtr_accept;
ushort              int_halt_code;
ASC_SCSI_BIT_ID_TYPE scsi_busy;
ASC_SCSI_BIT_ID_TYPE target_id;
PortAddr            iop_base;
uchar               tag_code;
uchar               q_status;
uchar               halt_qp;
uchar               sdtr_data;
uchar               target_ix;
uchar               q_cntl, tid_no;
uchar               cur_dvc_qng;
uchar               asyn_sdtr;
uchar               scsi_status;
asc_board_t            *boardp;
ASC_ASSERT(asc_dvc->drv_ptr != 0);
boardp = (asc_board_t *) asc_dvc->drv_ptr;
iop_base = asc_dvc->iop_base;
int_halt_code = AscReadLramWord(iop_base, ASCV_HALTCODE_W);
halt_qp = AscReadLramByte(iop_base, ASCV_CURCDB_B);
halt_q_addr = ASC_QNO_TO_QADDR(halt_qp);
target_ix = AscReadLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_TARGET_IX));
q_cntl = AscReadLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_CNTL));
tid_no = ASC_TIX_TO_TID(target_ix);
target_id = (uchar) ASC_TID_TO_TARGET_ID(tid_no);
if (asc_dvc->pci_fix_asyn_xfer & target_id) {
asyn_sdtr = ASYN_SDTR_DATA_FIX_PCI_REV_AB;
} else {
asyn_sdtr = 0;
}
if (int_halt_code == ASC_HALT_DISABLE_ASYN_USE_SYN_FIX) {
if (asc_dvc->pci_fix_asyn_xfer & target_id) {
AscSetChipSDTR(iop_base, 0, tid_no);
boardp->sdtr_data[tid_no] = 0;
}
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
return (0);
} else if (int_halt_code == ASC_HALT_ENABLE_ASYN_USE_SYN_FIX) {
if (asc_dvc->pci_fix_asyn_xfer & target_id) {
AscSetChipSDTR(iop_base, asyn_sdtr, tid_no);
boardp->sdtr_data[tid_no] = asyn_sdtr;
}
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
return (0);
} else if (int_halt_code == ASC_HALT_EXTMSG_IN) {
AscMemWordCopyFromLram(iop_base,
ASCV_MSGIN_BEG,
(ushort *) & ext_msg,
(ushort) (sizeof (EXT_MSG) >> 1));
if (ext_msg.msg_type == MS_EXTEND &&
ext_msg.msg_req == MS_SDTR_CODE &&
ext_msg.msg_len == MS_SDTR_LEN) {
sdtr_accept = TRUE;
if ((ext_msg.req_ack_offset > ASC_SYN_MAX_OFFSET)) {
sdtr_accept = FALSE;
ext_msg.req_ack_offset = ASC_SYN_MAX_OFFSET;
}
if ((ext_msg.xfer_period <
asc_dvc->sdtr_period_tbl[asc_dvc->host_init_sdtr_index]) ||
(ext_msg.xfer_period >
asc_dvc->sdtr_period_tbl[asc_dvc->max_sdtr_index])) {
sdtr_accept = FALSE;
ext_msg.xfer_period =
asc_dvc->sdtr_period_tbl[asc_dvc->host_init_sdtr_index];
}
if (sdtr_accept) {
sdtr_data = AscCalSDTRData(asc_dvc, ext_msg.xfer_period,
ext_msg.req_ack_offset);
if ((sdtr_data == 0xFF)) {
q_cntl |= QC_MSG_OUT;
asc_dvc->init_sdtr &= ~target_id;
asc_dvc->sdtr_done &= ~target_id;
AscSetChipSDTR(iop_base, asyn_sdtr, tid_no);
boardp->sdtr_data[tid_no] = asyn_sdtr;
}
}
if (ext_msg.req_ack_offset == 0) {
q_cntl &= ~QC_MSG_OUT;
asc_dvc->init_sdtr &= ~target_id;
asc_dvc->sdtr_done &= ~target_id;
AscSetChipSDTR(iop_base, asyn_sdtr, tid_no);
} else {
if (sdtr_accept && (q_cntl & QC_MSG_OUT)) {
q_cntl &= ~QC_MSG_OUT;
asc_dvc->sdtr_done |= target_id;
asc_dvc->init_sdtr |= target_id;
asc_dvc->pci_fix_asyn_xfer &= ~target_id;
sdtr_data = AscCalSDTRData(asc_dvc, ext_msg.xfer_period,
ext_msg.req_ack_offset);
AscSetChipSDTR(iop_base, sdtr_data, tid_no);
boardp->sdtr_data[tid_no] = sdtr_data;
} else {
q_cntl |= QC_MSG_OUT;
AscMsgOutSDTR(asc_dvc,
ext_msg.xfer_period,
ext_msg.req_ack_offset);
asc_dvc->pci_fix_asyn_xfer &= ~target_id;
sdtr_data = AscCalSDTRData(asc_dvc, ext_msg.xfer_period,
ext_msg.req_ack_offset);
AscSetChipSDTR(iop_base, sdtr_data, tid_no);
boardp->sdtr_data[tid_no] = sdtr_data;
asc_dvc->sdtr_done |= target_id;
asc_dvc->init_sdtr |= target_id;
}
}
AscWriteLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_CNTL),
q_cntl);
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
return (0);
} else if (ext_msg.msg_type == MS_EXTEND &&
ext_msg.msg_req == MS_WDTR_CODE &&
ext_msg.msg_len == MS_WDTR_LEN) {
ext_msg.wdtr_width = 0;
AscMemWordCopyToLram(iop_base,
ASCV_MSGOUT_BEG,
(ushort *) & ext_msg,
(ushort) (sizeof (EXT_MSG) >> 1));
q_cntl |= QC_MSG_OUT;
AscWriteLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_CNTL),
q_cntl);
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
return (0);
} else {
ext_msg.msg_type = M1_MSG_REJECT;
AscMemWordCopyToLram(iop_base,
ASCV_MSGOUT_BEG,
(ushort *) & ext_msg,
(ushort) (sizeof (EXT_MSG) >> 1));
q_cntl |= QC_MSG_OUT;
AscWriteLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_CNTL),
q_cntl);
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
return (0);
}
} else if (int_halt_code == ASC_HALT_CHK_CONDITION) {
q_cntl |= QC_REQ_SENSE;
if ((asc_dvc->init_sdtr & target_id) != 0) {
asc_dvc->sdtr_done &= ~target_id;
sdtr_data = AscGetMCodeInitSDTRAtID(iop_base, tid_no);
q_cntl |= QC_MSG_OUT;
AscMsgOutSDTR(asc_dvc,
asc_dvc->sdtr_period_tbl[(sdtr_data >> 4) &
(uchar) (asc_dvc->max_sdtr_index - 1)],
(uchar) (sdtr_data & (uchar) ASC_SYN_MAX_OFFSET));
}
AscWriteLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_CNTL),
q_cntl);
tag_code = AscReadLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_TAG_CODE));
tag_code &= 0xDC;
if (
(asc_dvc->pci_fix_asyn_xfer & target_id)
&& !(asc_dvc->pci_fix_asyn_xfer_always & target_id)
) {
tag_code |= (ASC_TAG_FLAG_DISABLE_DISCONNECT
| ASC_TAG_FLAG_DISABLE_ASYN_USE_SYN_FIX);
}
AscWriteLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_TAG_CODE),
tag_code);
q_status = AscReadLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_STATUS));
q_status |= (QS_READY | QS_BUSY);
AscWriteLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_STATUS),
q_status);
scsi_busy = AscReadLramByte(iop_base,
(ushort) ASCV_SCSIBUSY_B);
scsi_busy &= ~target_id;
AscWriteLramByte(iop_base, (ushort) ASCV_SCSIBUSY_B, scsi_busy);
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
return (0);
} else if (int_halt_code == ASC_HALT_SDTR_REJECTED) {
AscMemWordCopyFromLram(iop_base,
ASCV_MSGOUT_BEG,
(ushort *) & out_msg,
(ushort) (sizeof (EXT_MSG) >> 1));
if ((out_msg.msg_type == MS_EXTEND) &&
(out_msg.msg_len == MS_SDTR_LEN) &&
(out_msg.msg_req == MS_SDTR_CODE)) {
asc_dvc->init_sdtr &= ~target_id;
asc_dvc->sdtr_done &= ~target_id;
AscSetChipSDTR(iop_base, asyn_sdtr, tid_no);
boardp->sdtr_data[tid_no] = asyn_sdtr;
}
q_cntl &= ~QC_MSG_OUT;
AscWriteLramByte(iop_base,
(ushort) (halt_q_addr + (ushort) ASC_SCSIQ_B_CNTL),
q_cntl);
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
return (0);
} else if (int_halt_code == ASC_HALT_SS_QUEUE_FULL) {
scsi_status = AscReadLramByte(iop_base,
(ushort) ((ushort) halt_q_addr + (ushort) ASC_SCSIQ_SCSI_STATUS));
cur_dvc_qng = AscReadLramByte(iop_base,
(ushort) ((ushort) ASC_QADR_BEG + (ushort) target_ix));
if ((cur_dvc_qng > 0) &&
(asc_dvc->cur_dvc_qng[tid_no] > 0)) {
scsi_busy = AscReadLramByte(iop_base,
(ushort) ASCV_SCSIBUSY_B);
scsi_busy |= target_id;
AscWriteLramByte(iop_base,
(ushort) ASCV_SCSIBUSY_B, scsi_busy);
asc_dvc->queue_full_or_busy |= target_id;
if (scsi_status == SS_QUEUE_FULL) {
if (cur_dvc_qng > ASC_MIN_TAGGED_CMD) {
cur_dvc_qng -= 1;
asc_dvc->max_dvc_qng[tid_no] = cur_dvc_qng;
AscWriteLramByte(iop_base,
(ushort) ((ushort) ASCV_MAX_DVC_QNG_BEG +
(ushort) tid_no),
cur_dvc_qng);
boardp->queue_full |= target_id;
boardp->queue_full_cnt[tid_no] = cur_dvc_qng;
#if ASC_QUEUE_FLOW_CONTROL
if (boardp->device[tid_no] != NULL &&
boardp->device[tid_no]->queue_curr_depth >
cur_dvc_qng) {
boardp->device[tid_no]->queue_curr_depth =
cur_dvc_qng;
}
#endif
}
}
}
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
return (0);
}
return (0);
}
STATIC uchar
_AscCopyLramScsiDoneQ(
PortAddr iop_base,
ushort q_addr,
REG ASC_QDONE_INFO * scsiq,
ulong max_dma_count
)
{
ushort              _val;
uchar               sg_queue_cnt;
DvcGetQinfo(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_DONE_INFO_BEG),
(ushort *) scsiq,
(ushort) ((sizeof (ASC_SCSIQ_2) + sizeof (ASC_SCSIQ_3)) / 2));
_val = AscReadLramWord(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_B_STATUS));
scsiq->q_status = (uchar) _val;
scsiq->q_no = (uchar) (_val >> 8);
_val = AscReadLramWord(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_B_CNTL));
scsiq->cntl = (uchar) _val;
sg_queue_cnt = (uchar) (_val >> 8);
_val = AscReadLramWord(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_B_SENSE_LEN));
scsiq->sense_len = (uchar) _val;
scsiq->extra_bytes = (uchar) (_val >> 8);
scsiq->remain_bytes = AscReadLramWord(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_DW_REMAIN_XFER_CNT));
scsiq->remain_bytes &= max_dma_count;
return (sg_queue_cnt);
}
STATIC int
AscIsrQDone(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc
)
{
uchar               next_qp;
uchar               n_q_used;
uchar               sg_list_qp;
uchar               sg_queue_cnt;
uchar               q_cnt;
uchar               done_q_tail;
uchar               tid_no;
ASC_SCSI_BIT_ID_TYPE scsi_busy;
ASC_SCSI_BIT_ID_TYPE target_id;
PortAddr            iop_base;
ushort              q_addr;
ushort              sg_q_addr;
uchar               cur_target_qng;
ASC_QDONE_INFO      scsiq_buf;
REG ASC_QDONE_INFO *scsiq;
int                 false_overrun;
ASC_ISR_CALLBACK    asc_isr_callback;
iop_base = asc_dvc->iop_base;
asc_isr_callback = (ASC_ISR_CALLBACK) asc_dvc->isr_callback;
n_q_used = 1;
scsiq = (ASC_QDONE_INFO *) & scsiq_buf;
done_q_tail = (uchar) AscGetVarDoneQTail(iop_base);
q_addr = ASC_QNO_TO_QADDR(done_q_tail);
next_qp = AscReadLramByte(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_B_FWD));
if (next_qp != ASC_QLINK_END) {
AscPutVarDoneQTail(iop_base, next_qp);
q_addr = ASC_QNO_TO_QADDR(next_qp);
sg_queue_cnt = _AscCopyLramScsiDoneQ(iop_base, q_addr, scsiq,
asc_dvc->max_dma_count);
AscWriteLramByte(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_B_STATUS),
(uchar) (scsiq->q_status & (uchar) ~ (QS_READY | QS_ABORTED)));
tid_no = ASC_TIX_TO_TID(scsiq->d2.target_ix);
target_id = ASC_TIX_TO_TARGET_ID(scsiq->d2.target_ix);
if ((scsiq->cntl & QC_SG_HEAD) != 0) {
sg_q_addr = q_addr;
sg_list_qp = next_qp;
for (q_cnt = 0; q_cnt < sg_queue_cnt; q_cnt++) {
sg_list_qp = AscReadLramByte(iop_base,
(ushort) (sg_q_addr + (ushort) ASC_SCSIQ_B_FWD));
sg_q_addr = ASC_QNO_TO_QADDR(sg_list_qp);
if (sg_list_qp == ASC_QLINK_END) {
AscSetLibErrorCode(asc_dvc, ASCQ_ERR_SG_Q_LINKS);
scsiq->d3.done_stat = QD_WITH_ERROR;
scsiq->d3.host_stat = QHSTA_D_QDONE_SG_LIST_CORRUPTED;
goto FATAL_ERR_QDONE;
}
AscWriteLramByte(iop_base,
(ushort) (sg_q_addr + (ushort) ASC_SCSIQ_B_STATUS),
QS_FREE);
}
n_q_used = sg_queue_cnt + 1;
AscPutVarDoneQTail(iop_base, sg_list_qp);
}
if (asc_dvc->queue_full_or_busy & target_id) {
cur_target_qng = AscReadLramByte(iop_base,
(ushort) ((ushort) ASC_QADR_BEG + (ushort) scsiq->d2.target_ix));
if (cur_target_qng < asc_dvc->max_dvc_qng[tid_no]) {
scsi_busy = AscReadLramByte(iop_base,
(ushort) ASCV_SCSIBUSY_B);
scsi_busy &= ~target_id;
AscWriteLramByte(iop_base,
(ushort) ASCV_SCSIBUSY_B, scsi_busy);
asc_dvc->queue_full_or_busy &= ~target_id;
}
}
if (asc_dvc->cur_total_qng >= n_q_used) {
asc_dvc->cur_total_qng -= n_q_used;
if (asc_dvc->cur_dvc_qng[tid_no] != 0) {
asc_dvc->cur_dvc_qng[tid_no]--;
}
} else {
AscSetLibErrorCode(asc_dvc, ASCQ_ERR_CUR_QNG);
scsiq->d3.done_stat = QD_WITH_ERROR;
goto FATAL_ERR_QDONE;
}
if ((scsiq->d2.srb_ptr == 0UL) ||
((scsiq->q_status & QS_ABORTED) != 0)) {
return (0x11);
} else if (scsiq->q_status == QS_DONE) {
false_overrun = FALSE;
if (scsiq->extra_bytes != 0) {
scsiq->remain_bytes += (ulong) scsiq->extra_bytes;
}
if (scsiq->d3.done_stat == QD_WITH_ERROR) {
if (scsiq->d3.host_stat == QHSTA_M_DATA_OVER_RUN) {
if ((scsiq->cntl & (QC_DATA_IN | QC_DATA_OUT)) == 0) {
scsiq->d3.done_stat = QD_NO_ERROR;
scsiq->d3.host_stat = QHSTA_NO_ERROR;
} else if (false_overrun) {
scsiq->d3.done_stat = QD_NO_ERROR;
scsiq->d3.host_stat = QHSTA_NO_ERROR;
}
} else if (scsiq->d3.host_stat ==
QHSTA_M_HUNG_REQ_SCSI_BUS_RESET) {
AscStopChip(iop_base);
AscSetChipControl(iop_base,
(uchar) (CC_SCSI_RESET | CC_HALT));
DvcDelayNanoSecond(asc_dvc, 60000);
AscSetChipControl(iop_base, CC_HALT);
AscSetChipStatus(iop_base, CIW_CLR_SCSI_RESET_INT);
AscSetChipStatus(iop_base, 0);
AscSetChipControl(iop_base, 0);
}
}
if ((scsiq->cntl & QC_NO_CALLBACK) == 0) {
(*asc_isr_callback) (asc_dvc, scsiq);
} else {
if ((AscReadLramByte(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_CDB_BEG)) ==
SCSICMD_StartStopUnit)) {
asc_dvc->unit_not_ready &= ~target_id;
if (scsiq->d3.done_stat != QD_NO_ERROR) {
asc_dvc->start_motor &= ~target_id;
}
}
}
return (1);
} else {
AscSetLibErrorCode(asc_dvc, ASCQ_ERR_Q_STATUS);
FATAL_ERR_QDONE:
if ((scsiq->cntl & QC_NO_CALLBACK) == 0) {
(*asc_isr_callback) (asc_dvc, scsiq);
}
return (0x80);
}
}
return (0);
}
STATIC int
AscISR(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc
)
{
ASC_CS_TYPE         chipstat;
PortAddr            iop_base;
ushort              saved_ram_addr;
uchar               ctrl_reg;
uchar               saved_ctrl_reg;
int                 int_pending;
int                 status;
uchar               host_flag;
iop_base = asc_dvc->iop_base;
int_pending = FALSE;
if (((asc_dvc->init_state & ASC_INIT_STATE_END_LOAD_MC) == 0)
|| (asc_dvc->isr_callback == 0)
) {
return (ERR);
}
if (asc_dvc->in_critical_cnt != 0) {
AscSetLibErrorCode(asc_dvc, ASCQ_ERR_ISR_ON_CRITICAL);
return (ERR);
}
if (asc_dvc->is_in_int) {
AscSetLibErrorCode(asc_dvc, ASCQ_ERR_ISR_RE_ENTRY);
return (ERR);
}
asc_dvc->is_in_int = TRUE;
ctrl_reg = AscGetChipControl(iop_base);
saved_ctrl_reg = ctrl_reg & (~(CC_SCSI_RESET | CC_CHIP_RESET |
CC_SINGLE_STEP | CC_DIAG | CC_TEST));
chipstat = AscGetChipStatus(iop_base);
if (chipstat & CSW_SCSI_RESET_LATCH) {
if (!(asc_dvc->bus_type & (ASC_IS_VL | ASC_IS_EISA))) {
int_pending = TRUE;
asc_dvc->sdtr_done = 0;
saved_ctrl_reg &= (uchar) (~CC_HALT);
while (AscGetChipStatus(iop_base) & CSW_SCSI_RESET_ACTIVE) ;
AscSetChipControl(iop_base, (CC_CHIP_RESET | CC_HALT));
AscSetChipControl(iop_base, CC_HALT);
AscSetChipStatus(iop_base, CIW_CLR_SCSI_RESET_INT);
AscSetChipStatus(iop_base, 0);
chipstat = AscGetChipStatus(iop_base);
}
}
saved_ram_addr = AscGetChipLramAddr(iop_base);
host_flag = AscReadLramByte(iop_base,
ASCV_HOST_FLAG_B) & (uchar) (~ASC_HOST_FLAG_IN_ISR);
AscWriteLramByte(iop_base, ASCV_HOST_FLAG_B,
(uchar) (host_flag | (uchar) ASC_HOST_FLAG_IN_ISR));
if ((chipstat & CSW_INT_PENDING)
|| (int_pending)
) {
AscAckInterrupt(iop_base);
int_pending = TRUE;
if ((chipstat & CSW_HALTED) &&
(ctrl_reg & CC_SINGLE_STEP)) {
if (AscIsrChipHalted(asc_dvc) == ERR) {
goto ISR_REPORT_QDONE_FATAL_ERROR;
} else {
saved_ctrl_reg &= (uchar) (~CC_HALT);
}
} else {
ISR_REPORT_QDONE_FATAL_ERROR:
if ((asc_dvc->dvc_cntl & ASC_CNTL_INT_MULTI_Q) != 0) {
while (((status = AscIsrQDone(asc_dvc)) & 0x01) != 0) {
}
} else {
do {
if ((status = AscIsrQDone(asc_dvc)) == 1) {
break;
}
} while (status == 0x11);
}
if ((status & 0x80) != 0)
int_pending = ERR;
}
}
AscWriteLramByte(iop_base, ASCV_HOST_FLAG_B, host_flag);
AscSetChipLramAddr(iop_base, saved_ram_addr);
AscSetChipControl(iop_base, saved_ctrl_reg);
asc_dvc->is_in_int = FALSE;
return (int_pending);
}
STATIC uchar _asc_mcode_buf[] ASC_INITDATA =
{
0x01,  0x03,  0x01,  0x19,  0x0F,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x0F,  0x0F,  0x0F,  0x0F,  0x0F,  0x0F,  0x0F,  0x0F,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x91,  0x10,  0x0A,  0x05,  0x01,  0x00,  0x00,  0x00,  0x00,  0xFF,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0xFF,  0x80,  0xFF,  0xFF,  0x01,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x23,  0x00,  0x24,  0x00,  0x00,  0x00,  0x07,  0x00,  0xFF,  0x00,  0x00,  0x00,  0x00,
0xFF,  0xFF,  0xFF,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0xE2,  0x88,  0x00,  0x00,  0x00,  0x00,
0x80,  0x73,  0x48,  0x04,  0x36,  0x00,  0x00,  0xA2,  0xC2,  0x00,  0x80,  0x73,  0x03,  0x23,  0x36,  0x40,
0xB6,  0x00,  0x36,  0x00,  0x05,  0xD6,  0x0C,  0xD2,  0x12,  0xDA,  0x00,  0xA2,  0xC2,  0x00,  0x92,  0x80,
0x1E,  0x98,  0x50,  0x00,  0xF5,  0x00,  0x48,  0x98,  0xDF,  0x23,  0x36,  0x60,  0xB6,  0x00,  0x92,  0x80,
0x4F,  0x00,  0xF5,  0x00,  0x48,  0x98,  0xEF,  0x23,  0x36,  0x60,  0xB6,  0x00,  0x92,  0x80,  0x80,  0x62,
0x92,  0x80,  0x00,  0x46,  0x17,  0xEE,  0x13,  0xEA,  0x02,  0x01,  0x09,  0xD8,  0xCD,  0x04,  0x4D,  0x00,
0x00,  0xA3,  0xD6,  0x00,  0xA6,  0x97,  0x7F,  0x23,  0x04,  0x61,  0x84,  0x01,  0xE6,  0x84,  0xD2,  0xC1,
0x80,  0x73,  0xCD,  0x04,  0x4D,  0x00,  0x00,  0xA3,  0xE2,  0x01,  0xA6,  0x97,  0xCE,  0x81,  0x00,  0x33,
0x02,  0x00,  0xC0,  0x88,  0x80,  0x73,  0x80,  0x77,  0x00,  0x01,  0x01,  0xA1,  0x02,  0x01,  0x4F,  0x00,
0x84,  0x97,  0x07,  0xA6,  0x0C,  0x01,  0x00,  0x33,  0x03,  0x00,  0xC0,  0x88,  0x03,  0x03,  0x03,  0xDE,
0x00,  0x33,  0x05,  0x00,  0xC0,  0x88,  0xCE,  0x00,  0x69,  0x60,  0xCE,  0x00,  0x02,  0x03,  0x4A,  0x60,
0x00,  0xA2,  0x80,  0x01,  0x80,  0x63,  0x07,  0xA6,  0x2C,  0x01,  0x80,  0x81,  0x03,  0x03,  0x80,  0x63,
0xE2,  0x00,  0x07,  0xA6,  0x3C,  0x01,  0x00,  0x33,  0x04,  0x00,  0xC0,  0x88,  0x03,  0x07,  0x02,  0x01,
0x04,  0xCA,  0x0D,  0x23,  0x68,  0x98,  0x4D,  0x04,  0x04,  0x85,  0x05,  0xD8,  0x0D,  0x23,  0x68,  0x98,
0xCD,  0x04,  0x15,  0x23,  0xF6,  0x88,  0xFB,  0x23,  0x02,  0x61,  0x82,  0x01,  0x80,  0x63,  0x02,  0x03,
0x06,  0xA3,  0x6A,  0x01,  0x00,  0x33,  0x0A,  0x00,  0xC0,  0x88,  0x4E,  0x00,  0x07,  0xA3,  0x76,  0x01,
0x00,  0x33,  0x0B,  0x00,  0xC0,  0x88,  0xCD,  0x04,  0x36,  0x2D,  0x00,  0x33,  0x1A,  0x00,  0xC0,  0x88,
0x50,  0x04,  0x90,  0x81,  0x06,  0xAB,  0x8A,  0x01,  0x90,  0x81,  0x4E,  0x00,  0x07,  0xA3,  0x9A,  0x01,
0x50,  0x00,  0x00,  0xA3,  0x44,  0x01,  0x00,  0x05,  0x84,  0x81,  0x46,  0x97,  0x02,  0x01,  0x05,  0xC6,
0x04,  0x23,  0xA0,  0x01,  0x15,  0x23,  0xA1,  0x01,  0xC6,  0x81,  0xFD,  0x23,  0x02,  0x61,  0x82,  0x01,
0x0A,  0xDA,  0x4A,  0x00,  0x06,  0x61,  0x00,  0xA0,  0xBC,  0x01,  0x80,  0x63,  0xCD,  0x04,  0x36,  0x2D,
0x00,  0x33,  0x1B,  0x00,  0xC0,  0x88,  0x06,  0x23,  0x68,  0x98,  0xCD,  0x04,  0xE6,  0x84,  0x06,  0x01,
0x00,  0xA2,  0xDC,  0x01,  0x57,  0x60,  0x00,  0xA0,  0xE2,  0x01,  0xE6,  0x84,  0x80,  0x23,  0xA0,  0x01,
0xE6,  0x84,  0x80,  0x73,  0x4B,  0x00,  0x06,  0x61,  0x00,  0xA2,  0x08,  0x02,  0x04,  0x01,  0x0C,  0xDE,
0x02,  0x01,  0x03,  0xCC,  0x4F,  0x00,  0x84,  0x97,  0x04,  0x82,  0x08,  0x23,  0x02,  0x41,  0x82,  0x01,
0x4F,  0x00,  0x62,  0x97,  0x48,  0x04,  0x84,  0x80,  0xF0,  0x97,  0x00,  0x46,  0x56,  0x00,  0x03,  0xC0,
0x01,  0x23,  0xE8,  0x00,  0x81,  0x73,  0x06,  0x29,  0x03,  0x42,  0x06,  0xE2,  0x03,  0xEE,  0x67,  0xEB,
0x11,  0x23,  0xF6,  0x88,  0x04,  0x98,  0xF4,  0x80,  0x80,  0x73,  0x80,  0x77,  0x07,  0xA4,  0x32,  0x02,
0x7C,  0x95,  0x06,  0xA6,  0x3C,  0x02,  0x03,  0xA6,  0x4C,  0x04,  0xC0,  0x88,  0x04,  0x01,  0x03,  0xD8,
0xB2,  0x98,  0x6A,  0x96,  0x4E,  0x82,  0xFE,  0x95,  0x80,  0x67,  0x83,  0x03,  0x80,  0x63,  0xB6,  0x2D,
0x02,  0xA6,  0x78,  0x02,  0x07,  0xA6,  0x66,  0x02,  0x06,  0xA6,  0x6A,  0x02,  0x03,  0xA6,  0x6E,  0x02,
0x00,  0x33,  0x10,  0x00,  0xC0,  0x88,  0x7C,  0x95,  0x50,  0x82,  0x60,  0x96,  0x50,  0x82,  0x04,  0x23,
0xA0,  0x01,  0x14,  0x23,  0xA1,  0x01,  0x3C,  0x84,  0x04,  0x01,  0x0C,  0xDC,  0xE0,  0x23,  0x25,  0x61,
0xEF,  0x00,  0x14,  0x01,  0x4F,  0x04,  0xA8,  0x01,  0x6F,  0x00,  0xA5,  0x01,  0x03,  0x23,  0xA4,  0x01,
0x06,  0x23,  0x9C,  0x01,  0x24,  0x2B,  0x1C,  0x01,  0x02,  0xA6,  0xB6,  0x02,  0x07,  0xA6,  0x66,  0x02,
0x06,  0xA6,  0x6A,  0x02,  0x03,  0xA6,  0x20,  0x04,  0x01,  0xA6,  0xC0,  0x02,  0x00,  0xA6,  0xC0,  0x02,
0x00,  0x33,  0x12,  0x00,  0xC0,  0x88,  0x00,  0x0E,  0x80,  0x63,  0x00,  0x43,  0x00,  0xA0,  0x98,  0x02,
0x4D,  0x04,  0x04,  0x01,  0x0B,  0xDC,  0xE7,  0x23,  0x04,  0x61,  0x84,  0x01,  0x10,  0x31,  0x12,  0x35,
0x14,  0x01,  0xEC,  0x00,  0x6C,  0x38,  0x00,  0x3F,  0x00,  0x00,  0xF6,  0x82,  0x18,  0x23,  0x04,  0x61,
0x18,  0xA0,  0xEE,  0x02,  0x04,  0x01,  0x9C,  0xC8,  0x00,  0x33,  0x1F,  0x00,  0xC0,  0x88,  0x08,  0x31,
0x0A,  0x35,  0x0C,  0x39,  0x0E,  0x3D,  0x7E,  0x98,  0xB6,  0x2D,  0x01,  0xA6,  0x20,  0x03,  0x00,  0xA6,
0x20,  0x03,  0x07,  0xA6,  0x18,  0x03,  0x06,  0xA6,  0x1C,  0x03,  0x03,  0xA6,  0x20,  0x04,  0x02,  0xA6,
0x78,  0x02,  0x00,  0x33,  0x33,  0x00,  0xC0,  0x88,  0x7C,  0x95,  0xFA,  0x82,  0x60,  0x96,  0xFA,  0x82,
0x82,  0x98,  0x80,  0x42,  0x7E,  0x98,  0x60,  0xE4,  0x04,  0x01,  0x29,  0xC8,  0x31,  0x05,  0x07,  0x01,
0x00,  0xA2,  0x60,  0x03,  0x00,  0x43,  0x87,  0x01,  0x05,  0x05,  0x86,  0x98,  0x7E,  0x98,  0x00,  0xA6,
0x22,  0x03,  0x07,  0xA6,  0x58,  0x03,  0x03,  0xA6,  0x3C,  0x04,  0x06,  0xA6,  0x5C,  0x03,  0x01,  0xA6,
0x22,  0x03,  0x00,  0x33,  0x25,  0x00,  0xC0,  0x88,  0x7C,  0x95,  0x3E,  0x83,  0x60,  0x96,  0x3E,  0x83,
0x04,  0x01,  0x0C,  0xCE,  0x03,  0xC8,  0x00,  0x33,  0x42,  0x00,  0xC0,  0x88,  0x00,  0x01,  0x05,  0x05,
0xFF,  0xA2,  0x7E,  0x03,  0xB1,  0x01,  0x08,  0x23,  0xB2,  0x01,  0x3A,  0x83,  0x05,  0x05,  0x15,  0x01,
0x00,  0xA2,  0x9E,  0x03,  0xEC,  0x00,  0x6E,  0x00,  0x95,  0x01,  0x6C,  0x38,  0x00,  0x3F,  0x00,  0x00,
0x01,  0xA6,  0x9A,  0x03,  0x00,  0xA6,  0x9A,  0x03,  0x12,  0x84,  0x80,  0x42,  0x7E,  0x98,  0x01,  0xA6,
0xA8,  0x03,  0x00,  0xA6,  0xC0,  0x03,  0x12,  0x84,  0xA6,  0x98,  0x80,  0x42,  0x01,  0xA6,  0xA8,  0x03,
0x07,  0xA6,  0xB6,  0x03,  0xD8,  0x83,  0x7C,  0x95,  0xAC,  0x83,  0x00,  0x33,  0x2F,  0x00,  0xC0,  0x88,
0xA6,  0x98,  0x80,  0x42,  0x00,  0xA6,  0xC0,  0x03,  0x07,  0xA6,  0xCE,  0x03,  0xD8,  0x83,  0x7C,  0x95,
0xC4,  0x83,  0x00,  0x33,  0x26,  0x00,  0xC0,  0x88,  0x38,  0x2B,  0x80,  0x32,  0x80,  0x36,  0x04,  0x23,
0xA0,  0x01,  0x12,  0x23,  0xA1,  0x01,  0x12,  0x84,  0x06,  0xF0,  0x06,  0xA4,  0xF6,  0x03,  0x80,  0x6B,
0x05,  0x23,  0x83,  0x03,  0x80,  0x63,  0x03,  0xA6,  0x10,  0x04,  0x07,  0xA6,  0x08,  0x04,  0x06,  0xA6,
0x0C,  0x04,  0x00,  0x33,  0x17,  0x00,  0xC0,  0x88,  0x7C,  0x95,  0xF6,  0x83,  0x60,  0x96,  0xF6,  0x83,
0x20,  0x84,  0x06,  0xF0,  0x06,  0xA4,  0x20,  0x04,  0x80,  0x6B,  0x05,  0x23,  0x83,  0x03,  0x80,  0x63,
0xB6,  0x2D,  0x03,  0xA6,  0x3C,  0x04,  0x07,  0xA6,  0x34,  0x04,  0x06,  0xA6,  0x38,  0x04,  0x00,  0x33,
0x30,  0x00,  0xC0,  0x88,  0x7C,  0x95,  0x20,  0x84,  0x60,  0x96,  0x20,  0x84,  0x1D,  0x01,  0x06,  0xCC,
0x00,  0x33,  0x00,  0x84,  0xC0,  0x20,  0x00,  0x23,  0xEA,  0x00,  0x81,  0x62,  0xA2,  0x0D,  0x80,  0x63,
0x07,  0xA6,  0x5A,  0x04,  0x00,  0x33,  0x18,  0x00,  0xC0,  0x88,  0x03,  0x03,  0x80,  0x63,  0xA3,  0x01,
0x07,  0xA4,  0x64,  0x04,  0x23,  0x01,  0x00,  0xA2,  0x86,  0x04,  0x0A,  0xA0,  0x76,  0x04,  0xE0,  0x00,
0x00,  0x33,  0x1D,  0x00,  0xC0,  0x88,  0x0B,  0xA0,  0x82,  0x04,  0xE0,  0x00,  0x00,  0x33,  0x1E,  0x00,
0xC0,  0x88,  0x42,  0x23,  0xF6,  0x88,  0x00,  0x23,  0x22,  0xA3,  0xE6,  0x04,  0x08,  0x23,  0x22,  0xA3,
0xA2,  0x04,  0x28,  0x23,  0x22,  0xA3,  0xAE,  0x04,  0x02,  0x23,  0x22,  0xA3,  0xC4,  0x04,  0x42,  0x23,
0xF6,  0x88,  0x4A,  0x00,  0x06,  0x61,  0x00,  0xA0,  0xAE,  0x04,  0x45,  0x23,  0xF6,  0x88,  0x04,  0x98,
0x00,  0xA2,  0xC0,  0x04,  0xB2,  0x98,  0x00,  0x33,  0x00,  0x82,  0xC0,  0x20,  0x81,  0x62,  0xF0,  0x81,
0x47,  0x23,  0xF6,  0x88,  0x04,  0x01,  0x0B,  0xDE,  0x04,  0x98,  0xB2,  0x98,  0x00,  0x33,  0x00,  0x81,
0xC0,  0x20,  0x81,  0x62,  0x14,  0x01,  0x00,  0xA0,  0x08,  0x02,  0x43,  0x23,  0xF6,  0x88,  0x04,  0x23,
0xA0,  0x01,  0x44,  0x23,  0xA1,  0x01,  0x80,  0x73,  0x4D,  0x00,  0x03,  0xA3,  0xF4,  0x04,  0x00,  0x33,
0x27,  0x00,  0xC0,  0x88,  0x04,  0x01,  0x04,  0xDC,  0x02,  0x23,  0xA2,  0x01,  0x04,  0x23,  0xA0,  0x01,
0x04,  0x98,  0x26,  0x95,  0x4B,  0x00,  0xF6,  0x00,  0x4F,  0x04,  0x4F,  0x00,  0x00,  0xA3,  0x22,  0x05,
0x00,  0x05,  0x76,  0x00,  0x06,  0x61,  0x00,  0xA2,  0x1C,  0x05,  0x0A,  0x85,  0x46,  0x97,  0xCD,  0x04,
0x24,  0x85,  0x48,  0x04,  0x84,  0x80,  0x02,  0x01,  0x03,  0xDA,  0x80,  0x23,  0x82,  0x01,  0x34,  0x85,
0x02,  0x23,  0xA0,  0x01,  0x4A,  0x00,  0x06,  0x61,  0x00,  0xA2,  0x40,  0x05,  0x1D,  0x01,  0x04,  0xD6,
0xFF,  0x23,  0x86,  0x41,  0x4B,  0x60,  0xCB,  0x00,  0xFF,  0x23,  0x80,  0x01,  0x49,  0x00,  0x81,  0x01,
0x04,  0x01,  0x02,  0xC8,  0x30,  0x01,  0x80,  0x01,  0xF7,  0x04,  0x03,  0x01,  0x49,  0x04,  0x80,  0x01,
0xC9,  0x00,  0x00,  0x05,  0x00,  0x01,  0xFF,  0xA0,  0x60,  0x05,  0x77,  0x04,  0x01,  0x23,  0xEA,  0x00,
0x5D,  0x00,  0xFE,  0xC7,  0x00,  0x62,  0x00,  0x23,  0xEA,  0x00,  0x00,  0x63,  0x07,  0xA4,  0xF8,  0x05,
0x03,  0x03,  0x02,  0xA0,  0x8E,  0x05,  0xF4,  0x85,  0x00,  0x33,  0x2D,  0x00,  0xC0,  0x88,  0x04,  0xA0,
0xB8,  0x05,  0x80,  0x63,  0x00,  0x23,  0xDF,  0x00,  0x4A,  0x00,  0x06,  0x61,  0x00,  0xA2,  0xA4,  0x05,
0x1D,  0x01,  0x06,  0xD6,  0x02,  0x23,  0x02,  0x41,  0x82,  0x01,  0x50,  0x00,  0x62,  0x97,  0x04,  0x85,
0x04,  0x23,  0x02,  0x41,  0x82,  0x01,  0x04,  0x85,  0x08,  0xA0,  0xBE,  0x05,  0xF4,  0x85,  0x03,  0xA0,
0xC4,  0x05,  0xF4,  0x85,  0x01,  0xA0,  0xCE,  0x05,  0x88,  0x00,  0x80,  0x63,  0xCC,  0x86,  0x07,  0xA0,
0xEE,  0x05,  0x5F,  0x00,  0x00,  0x2B,  0xDF,  0x08,  0x00,  0xA2,  0xE6,  0x05,  0x80,  0x67,  0x80,  0x63,
0x01,  0xA2,  0x7A,  0x06,  0x7C,  0x85,  0x06,  0x23,  0x68,  0x98,  0x48,  0x23,  0xF6,  0x88,  0x07,  0x23,
0x80,  0x00,  0x06,  0x87,  0x80,  0x63,  0x7C,  0x85,  0x00,  0x23,  0xDF,  0x00,  0x00,  0x63,  0x4A,  0x00,
0x06,  0x61,  0x00,  0xA2,  0x36,  0x06,  0x1D,  0x01,  0x16,  0xD4,  0xC0,  0x23,  0x07,  0x41,  0x83,  0x03,
0x80,  0x63,  0x06,  0xA6,  0x1C,  0x06,  0x00,  0x33,  0x37,  0x00,  0xC0,  0x88,  0x1D,  0x01,  0x01,  0xD6,
0x20,  0x23,  0x63,  0x60,  0x83,  0x03,  0x80,  0x63,  0x02,  0x23,  0xDF,  0x00,  0x07,  0xA6,  0x7C,  0x05,
0xEF,  0x04,  0x6F,  0x00,  0x00,  0x63,  0x4B,  0x00,  0x06,  0x41,  0xCB,  0x00,  0x52,  0x00,  0x06,  0x61,
0x00,  0xA2,  0x4E,  0x06,  0x1D,  0x01,  0x03,  0xCA,  0xC0,  0x23,  0x07,  0x41,  0x00,  0x63,  0x1D,  0x01,
0x04,  0xCC,  0x00,  0x33,  0x00,  0x83,  0xC0,  0x20,  0x81,  0x62,  0x80,  0x23,  0x07,  0x41,  0x00,  0x63,
0x80,  0x67,  0x08,  0x23,  0x83,  0x03,  0x80,  0x63,  0x00,  0x63,  0x01,  0x23,  0xDF,  0x00,  0x06,  0xA6,
0x84,  0x06,  0x07,  0xA6,  0x7C,  0x05,  0x80,  0x67,  0x80,  0x63,  0x00,  0x33,  0x00,  0x40,  0xC0,  0x20,
0x81,  0x62,  0x00,  0x63,  0x00,  0x00,  0xFE,  0x95,  0x83,  0x03,  0x80,  0x63,  0x06,  0xA6,  0x94,  0x06,
0x07,  0xA6,  0x7C,  0x05,  0x00,  0x00,  0x01,  0xA0,  0x14,  0x07,  0x00,  0x2B,  0x40,  0x0E,  0x80,  0x63,
0x01,  0x00,  0x06,  0xA6,  0xAA,  0x06,  0x07,  0xA6,  0x7C,  0x05,  0x40,  0x0E,  0x80,  0x63,  0x00,  0x43,
0x00,  0xA0,  0xA2,  0x06,  0x06,  0xA6,  0xBC,  0x06,  0x07,  0xA6,  0x7C,  0x05,  0x80,  0x67,  0x40,  0x0E,
0x80,  0x63,  0x07,  0xA6,  0x7C,  0x05,  0x00,  0x23,  0xDF,  0x00,  0x00,  0x63,  0x07,  0xA6,  0xD6,  0x06,
0x00,  0x33,  0x2A,  0x00,  0xC0,  0x88,  0x03,  0x03,  0x80,  0x63,  0x89,  0x00,  0x0A,  0x2B,  0x07,  0xA6,
0xE8,  0x06,  0x00,  0x33,  0x29,  0x00,  0xC0,  0x88,  0x00,  0x43,  0x00,  0xA2,  0xF4,  0x06,  0xC0,  0x0E,
0x80,  0x63,  0xDE,  0x86,  0xC0,  0x0E,  0x00,  0x33,  0x00,  0x80,  0xC0,  0x20,  0x81,  0x62,  0x04,  0x01,
0x02,  0xDA,  0x80,  0x63,  0x7C,  0x85,  0x80,  0x7B,  0x80,  0x63,  0x06,  0xA6,  0x8C,  0x06,  0x00,  0x33,
0x2C,  0x00,  0xC0,  0x88,  0x0C,  0xA2,  0x2E,  0x07,  0xFE,  0x95,  0x83,  0x03,  0x80,  0x63,  0x06,  0xA6,
0x2C,  0x07,  0x07,  0xA6,  0x7C,  0x05,  0x00,  0x33,  0x3D,  0x00,  0xC0,  0x88,  0x00,  0x00,  0x80,  0x67,
0x83,  0x03,  0x80,  0x63,  0x0C,  0xA0,  0x44,  0x07,  0x07,  0xA6,  0x7C,  0x05,  0xBF,  0x23,  0x04,  0x61,
0x84,  0x01,  0xE6,  0x84,  0x00,  0x63,  0xF0,  0x04,  0x01,  0x01,  0xF1,  0x00,  0x00,  0x01,  0xF2,  0x00,
0x01,  0x05,  0x80,  0x01,  0x72,  0x04,  0x71,  0x00,  0x81,  0x01,  0x70,  0x04,  0x80,  0x05,  0x81,  0x05,
0x00,  0x63,  0xF0,  0x04,  0xF2,  0x00,  0x72,  0x04,  0x01,  0x01,  0xF1,  0x00,  0x70,  0x00,  0x81,  0x01,
0x70,  0x04,  0x71,  0x00,  0x81,  0x01,  0x72,  0x00,  0x80,  0x01,  0x71,  0x04,  0x70,  0x00,  0x80,  0x01,
0x70,  0x04,  0x00,  0x63,  0xF0,  0x04,  0xF2,  0x00,  0x72,  0x04,  0x00,  0x01,  0xF1,  0x00,  0x70,  0x00,
0x80,  0x01,  0x70,  0x04,  0x71,  0x00,  0x80,  0x01,  0x72,  0x00,  0x81,  0x01,  0x71,  0x04,  0x70,  0x00,
0x81,  0x01,  0x70,  0x04,  0x00,  0x63,  0x00,  0x23,  0xB3,  0x01,  0x83,  0x05,  0xA3,  0x01,  0xA2,  0x01,
0xA1,  0x01,  0x01,  0x23,  0xA0,  0x01,  0x00,  0x01,  0xC8,  0x00,  0x03,  0xA1,  0xC4,  0x07,  0x00,  0x33,
0x07,  0x00,  0xC0,  0x88,  0x80,  0x05,  0x81,  0x05,  0x04,  0x01,  0x11,  0xC8,  0x48,  0x00,  0xB0,  0x01,
0xB1,  0x01,  0x08,  0x23,  0xB2,  0x01,  0x05,  0x01,  0x48,  0x04,  0x00,  0x43,  0x00,  0xA2,  0xE4,  0x07,
0x00,  0x05,  0xDA,  0x87,  0x00,  0x01,  0xC8,  0x00,  0xFF,  0x23,  0x80,  0x01,  0x05,  0x05,  0x00,  0x63,
0xF7,  0x04,  0x1A,  0x09,  0xF6,  0x08,  0x6E,  0x04,  0x00,  0x02,  0x80,  0x43,  0x76,  0x08,  0x80,  0x02,
0x77,  0x04,  0x00,  0x63,  0xF7,  0x04,  0x1A,  0x09,  0xF6,  0x08,  0x6E,  0x04,  0x00,  0x02,  0x00,  0xA0,
0x14,  0x08,  0x16,  0x88,  0x00,  0x43,  0x76,  0x08,  0x80,  0x02,  0x77,  0x04,  0x00,  0x63,  0xF3,  0x04,
0x00,  0x23,  0xF4,  0x00,  0x74,  0x00,  0x80,  0x43,  0xF4,  0x00,  0xCF,  0x40,  0x00,  0xA2,  0x44,  0x08,
0x74,  0x04,  0x02,  0x01,  0xF7,  0xC9,  0xF6,  0xD9,  0x00,  0x01,  0x01,  0xA1,  0x24,  0x08,  0x04,  0x98,
0x26,  0x95,  0x24,  0x88,  0x73,  0x04,  0x00,  0x63,  0xF3,  0x04,  0x75,  0x04,  0x5A,  0x88,  0x02,  0x01,
0x04,  0xD8,  0x46,  0x97,  0x04,  0x98,  0x26,  0x95,  0x4A,  0x88,  0x75,  0x00,  0x00,  0xA3,  0x64,  0x08,
0x00,  0x05,  0x4E,  0x88,  0x73,  0x04,  0x00,  0x63,  0x80,  0x7B,  0x80,  0x63,  0x06,  0xA6,  0x76,  0x08,
0x00,  0x33,  0x3E,  0x00,  0xC0,  0x88,  0x80,  0x67,  0x83,  0x03,  0x80,  0x63,  0x00,  0x63,  0x38,  0x2B,
0x9C,  0x88,  0x38,  0x2B,  0x92,  0x88,  0x32,  0x09,  0x31,  0x05,  0x92,  0x98,  0x05,  0x05,  0xB2,  0x09,
0x00,  0x63,  0x00,  0x32,  0x00,  0x36,  0x00,  0x3A,  0x00,  0x3E,  0x00,  0x63,  0x80,  0x32,  0x80,  0x36,
0x80,  0x3A,  0x80,  0x3E,  0x00,  0x63,  0x38,  0x2B,  0x40,  0x32,  0x40,  0x36,  0x40,  0x3A,  0x40,  0x3E,
0x00,  0x63,  0x5A,  0x20,  0xC9,  0x40,  0x00,  0xA0,  0xB2,  0x08,  0x5D,  0x00,  0xFE,  0xC3,  0x00,  0x63,
0x80,  0x73,  0xE6,  0x20,  0x02,  0x23,  0xE8,  0x00,  0x82,  0x73,  0xFF,  0xFD,  0x80,  0x73,  0x13,  0x23,
0xF6,  0x88,  0x66,  0x20,  0xC0,  0x20,  0x04,  0x23,  0xA0,  0x01,  0xA1,  0x23,  0xA1,  0x01,  0x81,  0x62,
0xE0,  0x88,  0x80,  0x73,  0x80,  0x77,  0x68,  0x00,  0x00,  0xA2,  0x80,  0x00,  0x03,  0xC2,  0xF1,  0xC7,
0x41,  0x23,  0xF6,  0x88,  0x11,  0x23,  0xA1,  0x01,  0x04,  0x23,  0xA0,  0x01,  0xE6,  0x84,
};
STATIC ushort _asc_mcode_size ASC_INITDATA = sizeof(_asc_mcode_buf);
STATIC ulong _asc_mcode_chksum ASC_INITDATA = 0x012B5442UL;
#define ASC_SYN_OFFSET_ONE_DISABLE_LIST  16
STATIC uchar _syn_offset_one_disable_cmd[ASC_SYN_OFFSET_ONE_DISABLE_LIST] =
{
SCSICMD_Inquiry,
SCSICMD_RequestSense,
SCSICMD_ReadCapacity,
SCSICMD_ReadTOC,
SCSICMD_ModeSelect6,
SCSICMD_ModeSense6,
SCSICMD_ModeSelect10,
SCSICMD_ModeSense10,
0xFF,
0xFF,
0xFF,
0xFF,
0xFF,
0xFF,
0xFF,
0xFF
};
STATIC int
AscExeScsiQueue(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
REG ASC_SCSI_Q * scsiq
)
{
PortAddr            iop_base;
int                 last_int_level;
int                 sta;
int                 n_q_required;
int                 disable_syn_offset_one_fix;
int                 i;
ulong               addr;
ASC_EXE_CALLBACK    asc_exe_callback;
ushort              sg_entry_cnt = 0;
ushort              sg_entry_cnt_minus_one = 0;
uchar               target_ix;
uchar               tid_no;
uchar               sdtr_data;
uchar               extra_bytes;
uchar               scsi_cmd;
uchar               disable_cmd;
ASC_SG_HEAD *sg_head;
ulong               data_cnt;
iop_base = asc_dvc->iop_base;
sg_head = scsiq->sg_head;
asc_exe_callback = (ASC_EXE_CALLBACK) asc_dvc->exe_callback;
if (asc_dvc->err_code != 0)
return (ERR);
if (scsiq == (ASC_SCSI_Q *) 0L) {
AscSetLibErrorCode(asc_dvc, ASCQ_ERR_SCSIQ_NULL_PTR);
return (ERR);
}
scsiq->q1.q_no = 0;
if ((scsiq->q2.tag_code & ASC_TAG_FLAG_EXTRA_BYTES) == 0) {
scsiq->q1.extra_bytes = 0;
}
sta = 0;
target_ix = scsiq->q2.target_ix;
tid_no = ASC_TIX_TO_TID(target_ix);
n_q_required = 1;
if (scsiq->cdbptr[0] == SCSICMD_RequestSense) {
if ((asc_dvc->init_sdtr & scsiq->q1.target_id) != 0) {
asc_dvc->sdtr_done &= ~scsiq->q1.target_id ;
sdtr_data = AscGetMCodeInitSDTRAtID(iop_base, tid_no);
AscMsgOutSDTR(asc_dvc,
asc_dvc->sdtr_period_tbl[(sdtr_data >> 4) &
(uchar) (asc_dvc->max_sdtr_index - 1)],
(uchar) (sdtr_data & (uchar) ASC_SYN_MAX_OFFSET));
scsiq->q1.cntl |= (QC_MSG_OUT | QC_URGENT);
}
}
last_int_level = DvcEnterCritical();
if (asc_dvc->in_critical_cnt != 0) {
DvcLeaveCritical(last_int_level);
AscSetLibErrorCode(asc_dvc, ASCQ_ERR_CRITICAL_RE_ENTRY);
return (ERR);
}
asc_dvc->in_critical_cnt++;
if ((scsiq->q1.cntl & QC_SG_HEAD) != 0) {
if ((sg_entry_cnt = sg_head->entry_cnt) == 0) {
asc_dvc->in_critical_cnt--;
DvcLeaveCritical(last_int_level);
return (ERR);
}
if (sg_entry_cnt > ASC_MAX_SG_LIST) {
return (ERR);
}
if (sg_entry_cnt == 1) {
scsiq->q1.data_addr = sg_head->sg_list[0].addr;
scsiq->q1.data_cnt = sg_head->sg_list[0].bytes;
scsiq->q1.cntl &= ~(QC_SG_HEAD | QC_SG_SWAP_QUEUE);
}
sg_entry_cnt_minus_one = sg_entry_cnt - 1;
}
scsi_cmd = scsiq->cdbptr[0];
disable_syn_offset_one_fix = FALSE;
if ((asc_dvc->pci_fix_asyn_xfer & scsiq->q1.target_id) &&
!(asc_dvc->pci_fix_asyn_xfer_always & scsiq->q1.target_id)) {
if (scsiq->q1.cntl & QC_SG_HEAD) {
data_cnt = 0;
for (i = 0; i < sg_entry_cnt; i++) {
data_cnt += sg_head->sg_list[i].bytes;
}
} else {
data_cnt = scsiq->q1.data_cnt;
}
if (data_cnt != 0UL) {
if (data_cnt < 512UL) {
disable_syn_offset_one_fix = TRUE;
} else {
for (i = 0; i < ASC_SYN_OFFSET_ONE_DISABLE_LIST; i++) {
disable_cmd = _syn_offset_one_disable_cmd[i];
if (disable_cmd == 0xFF) {
break;
}
if (scsi_cmd == disable_cmd) {
disable_syn_offset_one_fix = TRUE;
break;
}
}
}
}
}
if (disable_syn_offset_one_fix) {
scsiq->q2.tag_code &= ~M2_QTAG_MSG_SIMPLE;
scsiq->q2.tag_code |= (ASC_TAG_FLAG_DISABLE_ASYN_USE_SYN_FIX |
ASC_TAG_FLAG_DISABLE_DISCONNECT);
} else {
scsiq->q2.tag_code &= 0x23;
}
if ((scsiq->q1.cntl & QC_SG_HEAD) != 0) {
if (asc_dvc->bug_fix_cntl) {
if (asc_dvc->bug_fix_cntl & ASC_BUG_FIX_IF_NOT_DWB) {
if ((scsi_cmd == SCSICMD_Read6) ||
(scsi_cmd == SCSICMD_Read10)) {
addr = sg_head->sg_list[sg_entry_cnt_minus_one].addr +
sg_head->sg_list[sg_entry_cnt_minus_one].bytes;
extra_bytes = (uchar) ((ushort) addr & 0x0003);
if ((extra_bytes != 0) &&
((scsiq->q2.tag_code & ASC_TAG_FLAG_EXTRA_BYTES)
== 0)) {
scsiq->q2.tag_code |= ASC_TAG_FLAG_EXTRA_BYTES;
scsiq->q1.extra_bytes = extra_bytes;
sg_head->sg_list[sg_entry_cnt_minus_one].bytes -=
(ulong) extra_bytes;
}
}
}
}
sg_head->entry_to_copy = sg_head->entry_cnt;
n_q_required = AscSgListToQueue(sg_entry_cnt);
if ((AscGetNumOfFreeQueue(asc_dvc, target_ix, n_q_required) >=
(uint) n_q_required) || ((scsiq->q1.cntl & QC_URGENT) != 0)) {
if ((sta = AscSendScsiQueue(asc_dvc, scsiq,
n_q_required)) == 1) {
asc_dvc->in_critical_cnt--;
if (asc_exe_callback != 0) {
(*asc_exe_callback) (asc_dvc, scsiq);
}
DvcLeaveCritical(last_int_level);
return (sta);
}
}
} else {
if (asc_dvc->bug_fix_cntl) {
if (asc_dvc->bug_fix_cntl & ASC_BUG_FIX_IF_NOT_DWB) {
if ((scsi_cmd == SCSICMD_Read6) ||
(scsi_cmd == SCSICMD_Read10)) {
addr = scsiq->q1.data_addr + scsiq->q1.data_cnt;
extra_bytes = (uchar) ((ushort) addr & 0x0003);
if ((extra_bytes != 0) &&
((scsiq->q2.tag_code & ASC_TAG_FLAG_EXTRA_BYTES)
== 0)) {
if (((ushort) scsiq->q1.data_cnt & 0x01FF) == 0) {
scsiq->q2.tag_code |= ASC_TAG_FLAG_EXTRA_BYTES;
scsiq->q1.data_cnt -= (ulong) extra_bytes;
scsiq->q1.extra_bytes = extra_bytes;
}
}
}
}
}
n_q_required = 1;
if ((AscGetNumOfFreeQueue(asc_dvc, target_ix, 1) >= 1) ||
((scsiq->q1.cntl & QC_URGENT) != 0)) {
if ((sta = AscSendScsiQueue(asc_dvc, scsiq,
n_q_required)) == 1) {
asc_dvc->in_critical_cnt--;
if (asc_exe_callback != 0) {
(*asc_exe_callback) (asc_dvc, scsiq);
}
DvcLeaveCritical(last_int_level);
return (sta);
}
}
}
asc_dvc->in_critical_cnt--;
DvcLeaveCritical(last_int_level);
return (sta);
}
STATIC int
AscSendScsiQueue(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
REG ASC_SCSI_Q * scsiq,
uchar n_q_required
)
{
PortAddr            iop_base;
uchar               free_q_head;
uchar               next_qp;
uchar               tid_no;
uchar               target_ix;
int                 sta;
iop_base = asc_dvc->iop_base;
target_ix = scsiq->q2.target_ix;
tid_no = ASC_TIX_TO_TID(target_ix);
sta = 0;
free_q_head = (uchar) AscGetVarFreeQHead(iop_base);
if (n_q_required > 1) {
if ((next_qp = AscAllocMultipleFreeQueue(iop_base,
free_q_head, (uchar) (n_q_required)))
!= (uchar) ASC_QLINK_END) {
asc_dvc->last_q_shortage = 0;
scsiq->sg_head->queue_cnt = n_q_required - 1;
scsiq->q1.q_no = free_q_head;
if ((sta = AscPutReadySgListQueue(asc_dvc, scsiq,
free_q_head)) == 1) {
AscPutVarFreeQHead(iop_base, next_qp);
asc_dvc->cur_total_qng += (uchar) (n_q_required);
asc_dvc->cur_dvc_qng[tid_no]++;
}
return (sta);
}
} else if (n_q_required == 1) {
if ((next_qp = AscAllocFreeQueue(iop_base,
free_q_head)) != ASC_QLINK_END) {
scsiq->q1.q_no = free_q_head;
if ((sta = AscPutReadyQueue(asc_dvc, scsiq,
free_q_head)) == 1) {
AscPutVarFreeQHead(iop_base, next_qp);
asc_dvc->cur_total_qng++;
asc_dvc->cur_dvc_qng[tid_no]++;
}
return (sta);
}
}
return (sta);
}
STATIC int
AscSgListToQueue(
int sg_list
)
{
int                 n_sg_list_qs;
n_sg_list_qs = ((sg_list - 1) / ASC_SG_LIST_PER_Q);
if (((sg_list - 1) % ASC_SG_LIST_PER_Q) != 0)
n_sg_list_qs++;
return (n_sg_list_qs + 1);
}
STATIC uint
AscGetNumOfFreeQueue(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
uchar target_ix,
uchar n_qs
)
{
uint                cur_used_qs;
uint                cur_free_qs;
ASC_SCSI_BIT_ID_TYPE target_id;
uchar               tid_no;
target_id = ASC_TIX_TO_TARGET_ID(target_ix);
tid_no = ASC_TIX_TO_TID(target_ix);
if ((asc_dvc->unit_not_ready & target_id) ||
(asc_dvc->queue_full_or_busy & target_id)) {
return (0);
}
if (n_qs == 1) {
cur_used_qs = (uint) asc_dvc->cur_total_qng +
(uint) asc_dvc->last_q_shortage +
(uint) ASC_MIN_FREE_Q;
} else {
cur_used_qs = (uint) asc_dvc->cur_total_qng +
(uint) ASC_MIN_FREE_Q;
}
if ((uint) (cur_used_qs + n_qs) <= (uint) asc_dvc->max_total_qng) {
cur_free_qs = (uint) asc_dvc->max_total_qng - cur_used_qs;
if (asc_dvc->cur_dvc_qng[tid_no] >=
asc_dvc->max_dvc_qng[tid_no]) {
return (0);
}
return (cur_free_qs);
}
if (n_qs > 1) {
if ((n_qs > asc_dvc->last_q_shortage) && (n_qs <= (asc_dvc->max_total_qng - ASC_MIN_FREE_Q))) {
asc_dvc->last_q_shortage = n_qs;
}
}
return (0);
}
STATIC int
AscPutReadyQueue(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
REG ASC_SCSI_Q * scsiq,
uchar q_no
)
{
ushort              q_addr;
uchar               tid_no;
uchar               sdtr_data;
uchar               syn_period_ix;
uchar               syn_offset;
PortAddr            iop_base;
iop_base = asc_dvc->iop_base;
if (((asc_dvc->init_sdtr & scsiq->q1.target_id) != 0) &&
((asc_dvc->sdtr_done & scsiq->q1.target_id) == 0)) {
tid_no = ASC_TIX_TO_TID(scsiq->q2.target_ix);
sdtr_data = AscGetMCodeInitSDTRAtID(iop_base, tid_no);
syn_period_ix = (sdtr_data >> 4) & (asc_dvc->max_sdtr_index - 1);
syn_offset = sdtr_data & ASC_SYN_MAX_OFFSET;
AscMsgOutSDTR(asc_dvc,
asc_dvc->sdtr_period_tbl[syn_period_ix],
syn_offset);
scsiq->q1.cntl |= QC_MSG_OUT;
}
q_addr = ASC_QNO_TO_QADDR(q_no);
if ((scsiq->q1.target_id & asc_dvc->use_tagged_qng) == 0) {
scsiq->q2.tag_code &= ~M2_QTAG_MSG_SIMPLE;
}
scsiq->q1.status = QS_FREE;
AscMemWordCopyToLram(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_CDB_BEG),
(ushort *) scsiq->cdbptr,
(ushort) ((ushort) scsiq->q2.cdb_len >> 1));
DvcPutScsiQ(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_CPY_BEG),
(ushort *) & scsiq->q1.cntl,
(ushort) ((((sizeof (ASC_SCSIQ_1) + sizeof (ASC_SCSIQ_2)) / 2) - 1)));
AscWriteLramWord(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_B_STATUS),
(ushort) (((ushort) scsiq->q1.q_no << 8) | (ushort) QS_READY));
return (1);
}
STATIC int
AscPutReadySgListQueue(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
REG ASC_SCSI_Q * scsiq,
uchar q_no
)
{
int                 sta;
int                 i;
ASC_SG_HEAD *sg_head;
ASC_SG_LIST_Q       scsi_sg_q;
ulong               saved_data_addr;
ulong               saved_data_cnt;
PortAddr            iop_base;
ushort              sg_list_dwords;
ushort              sg_index;
ushort              sg_entry_cnt;
ushort              q_addr;
uchar               next_qp;
iop_base = asc_dvc->iop_base;
sg_head = scsiq->sg_head;
saved_data_addr = scsiq->q1.data_addr;
saved_data_cnt = scsiq->q1.data_cnt;
scsiq->q1.data_addr = sg_head->sg_list[0].addr;
scsiq->q1.data_cnt = sg_head->sg_list[0].bytes;
sg_entry_cnt = sg_head->entry_cnt - 1;
if (sg_entry_cnt != 0) {
scsiq->q1.cntl |= QC_SG_HEAD;
q_addr = ASC_QNO_TO_QADDR(q_no);
sg_index = 1;
scsiq->q1.sg_queue_cnt = sg_head->queue_cnt;
scsi_sg_q.sg_head_qp = q_no;
scsi_sg_q.cntl = QCSG_SG_XFER_LIST;
for (i = 0; i < sg_head->queue_cnt; i++) {
scsi_sg_q.seq_no = i + 1;
if (sg_entry_cnt > ASC_SG_LIST_PER_Q) {
sg_list_dwords = (uchar) (ASC_SG_LIST_PER_Q * 2);
sg_entry_cnt -= ASC_SG_LIST_PER_Q;
if (i == 0) {
scsi_sg_q.sg_list_cnt = ASC_SG_LIST_PER_Q;
scsi_sg_q.sg_cur_list_cnt = ASC_SG_LIST_PER_Q;
} else {
scsi_sg_q.sg_list_cnt = ASC_SG_LIST_PER_Q - 1;
scsi_sg_q.sg_cur_list_cnt = ASC_SG_LIST_PER_Q - 1;
}
} else {
scsi_sg_q.cntl |= QCSG_SG_XFER_END;
sg_list_dwords = sg_entry_cnt << 1;
if (i == 0) {
scsi_sg_q.sg_list_cnt = sg_entry_cnt;
scsi_sg_q.sg_cur_list_cnt = sg_entry_cnt;
} else {
scsi_sg_q.sg_list_cnt = sg_entry_cnt - 1;
scsi_sg_q.sg_cur_list_cnt = sg_entry_cnt - 1;
}
sg_entry_cnt = 0;
}
next_qp = AscReadLramByte(iop_base,
(ushort) (q_addr + ASC_SCSIQ_B_FWD));
scsi_sg_q.q_no = next_qp;
q_addr = ASC_QNO_TO_QADDR(next_qp);
AscMemWordCopyToLram(iop_base,
(ushort) (q_addr + ASC_SCSIQ_SGHD_CPY_BEG),
(ushort *) & scsi_sg_q,
(ushort) (sizeof (ASC_SG_LIST_Q) >> 1));
AscMemDWordCopyToLram(iop_base,
(ushort) (q_addr + ASC_SGQ_LIST_BEG),
(ulong *) & sg_head->sg_list[sg_index],
(ushort) sg_list_dwords);
sg_index += ASC_SG_LIST_PER_Q;
}
} else {
scsiq->q1.cntl &= ~QC_SG_HEAD;
}
sta = AscPutReadyQueue(asc_dvc, scsiq, q_no);
scsiq->q1.data_addr = saved_data_addr;
scsiq->q1.data_cnt = saved_data_cnt;
return (sta);
}
STATIC int
AscAbortSRB(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
ulong srb_ptr
)
{
int                 sta;
ASC_SCSI_BIT_ID_TYPE saved_unit_not_ready;
PortAddr            iop_base;
iop_base = asc_dvc->iop_base;
sta = ERR;
saved_unit_not_ready = asc_dvc->unit_not_ready;
asc_dvc->unit_not_ready = 0xFF;
AscWaitISRDone(asc_dvc);
if (AscStopQueueExe(iop_base) == 1) {
if (AscRiscHaltedAbortSRB(asc_dvc, srb_ptr) == 1) {
sta = 1;
AscCleanUpBusyQueue(iop_base);
AscStartQueueExe(iop_base);
} else {
sta = 0;
AscStartQueueExe(iop_base);
}
}
asc_dvc->unit_not_ready = saved_unit_not_ready;
return (sta);
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC int
AscResetDevice(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
uchar target_ix
)
{
PortAddr            iop_base;
int                 sta;
uchar               tid_no;
ASC_SCSI_BIT_ID_TYPE target_id;
int                 i;
ASC_SCSI_REQ_Q      scsiq_buf;
ASC_SCSI_REQ_Q *scsiq;
uchar       *buf;
ASC_SCSI_BIT_ID_TYPE saved_unit_not_ready;
iop_base = asc_dvc->iop_base;
tid_no = ASC_TIX_TO_TID(target_ix);
target_id = ASC_TID_TO_TARGET_ID(tid_no);
saved_unit_not_ready = asc_dvc->unit_not_ready;
asc_dvc->unit_not_ready = target_id;
sta = ERR;
AscWaitTixISRDone(asc_dvc, target_ix);
if (AscStopQueueExe(iop_base) == 1) {
if (AscRiscHaltedAbortTIX(asc_dvc, target_ix) == 1) {
AscCleanUpBusyQueue(iop_base);
AscStartQueueExe(iop_base);
AscWaitTixISRDone(asc_dvc, target_ix);
sta = TRUE;
scsiq = (ASC_SCSI_REQ_Q *) & scsiq_buf;
buf = (uchar *) & scsiq_buf;
for (i = 0; i < sizeof (ASC_SCSI_REQ_Q); i++) {
*buf++ = 0x00;
}
scsiq->r1.status = (uchar) QS_READY;
scsiq->r2.cdb_len = 6;
scsiq->r2.tag_code = M2_QTAG_MSG_SIMPLE;
scsiq->r1.target_id = target_id;
scsiq->r2.target_ix = ASC_TIDLUN_TO_IX(tid_no, 0);
scsiq->cdbptr = (uchar *) scsiq->cdb;
scsiq->r1.cntl = QC_NO_CALLBACK | QC_MSG_OUT | QC_URGENT;
AscWriteLramByte(asc_dvc->iop_base, ASCV_MSGOUT_BEG,
M1_BUS_DVC_RESET);
asc_dvc->unit_not_ready &= ~target_id;
asc_dvc->sdtr_done |= target_id;
if (AscExeScsiQueue(asc_dvc, (ASC_SCSI_Q *) scsiq)
== 1) {
asc_dvc->unit_not_ready = target_id;
DvcSleepMilliSecond(1000);
_AscWaitQDone(iop_base, (ASC_SCSI_Q *) scsiq);
if (AscStopQueueExe(iop_base) == 1) {
AscCleanUpDiscQueue(iop_base);
AscStartQueueExe(iop_base);
if (asc_dvc->pci_fix_asyn_xfer & target_id) {
AscSetRunChipSynRegAtID(iop_base, tid_no,
ASYN_SDTR_DATA_FIX_PCI_REV_AB);
}
AscWaitTixISRDone(asc_dvc, target_ix);
}
} else {
sta = 0;
}
asc_dvc->sdtr_done &= ~target_id;
} else {
sta = ERR;
AscStartQueueExe(iop_base);
}
}
asc_dvc->unit_not_ready = saved_unit_not_ready;
return (sta);
}
#endif
STATIC int
AscResetSB(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc
)
{
int                 sta;
int                 i;
PortAddr            iop_base;
iop_base = asc_dvc->iop_base;
asc_dvc->unit_not_ready = 0xFF;
sta = TRUE;
AscWaitISRDone(asc_dvc);
AscStopQueueExe(iop_base);
asc_dvc->sdtr_done = 0;
AscResetChipAndScsiBus(asc_dvc);
DvcSleepMilliSecond((ulong) ((ushort) asc_dvc->scsi_reset_wait * 1000));
AscReInitLram(asc_dvc);
for (i = 0; i <= ASC_MAX_TID; i++) {
asc_dvc->cur_dvc_qng[i] = 0;
if (asc_dvc->pci_fix_asyn_xfer & (ASC_SCSI_BIT_ID_TYPE) (0x01 << i)) {
AscSetChipSynRegAtID(iop_base, i, ASYN_SDTR_DATA_FIX_PCI_REV_AB);
}
}
asc_dvc->err_code = 0;
AscSetPCAddr(iop_base, ASC_MCODE_START_ADDR);
if (AscGetPCAddr(iop_base) != ASC_MCODE_START_ADDR) {
sta = ERR;
}
if (AscStartChip(iop_base) == 0) {
sta = ERR;
}
AscStartQueueExe(iop_base);
asc_dvc->unit_not_ready = 0;
asc_dvc->queue_full_or_busy = 0;
return (sta);
}
STATIC int
AscSetRunChipSynRegAtID(
PortAddr iop_base,
uchar tid_no,
uchar sdtr_data
)
{
int                 sta = FALSE;
if (AscHostReqRiscHalt(iop_base)) {
sta = AscSetChipSynRegAtID(iop_base, tid_no, sdtr_data);
AscStartChip(iop_base);
return (sta);
}
return (sta);
}
STATIC int
AscSetChipSynRegAtID(
PortAddr iop_base,
uchar id,
uchar sdtr_data
)
{
ASC_SCSI_BIT_ID_TYPE org_id;
int                 i;
int                 sta = TRUE;
AscSetBank(iop_base, 1);
org_id = AscReadChipDvcID(iop_base);
for (i = 0; i <= ASC_MAX_TID; i++) {
if (org_id == (0x01 << i))
break;
}
org_id = i;
AscWriteChipDvcID(iop_base, id);
if (AscReadChipDvcID(iop_base) == (0x01 << id)) {
AscSetBank(iop_base, 0);
AscSetChipSyn(iop_base, sdtr_data);
if (AscGetChipSyn(iop_base) != sdtr_data) {
sta = FALSE;
}
} else {
sta = FALSE;
}
AscSetBank(iop_base, 1);
AscWriteChipDvcID(iop_base, org_id);
AscSetBank(iop_base, 0);
return (sta);
}
STATIC int
AscReInitLram(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc
)
{
AscInitLram(asc_dvc);
AscInitQLinkVar(asc_dvc);
return (0);
}
STATIC ushort
AscInitLram(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc
)
{
uchar               i;
ushort              s_addr;
PortAddr            iop_base;
ushort              warn_code;
iop_base = asc_dvc->iop_base;
warn_code = 0;
AscMemWordSetLram(iop_base, ASC_QADR_BEG, 0,
(ushort) (((int) (asc_dvc->max_total_qng + 2 + 1) * 64) >> 1)
);
i = ASC_MIN_ACTIVE_QNO;
s_addr = ASC_QADR_BEG + ASC_QBLK_SIZE;
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_FWD),
(uchar) (i + 1));
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_BWD),
(uchar) (asc_dvc->max_total_qng));
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_QNO),
(uchar) i);
i++;
s_addr += ASC_QBLK_SIZE;
for (; i < asc_dvc->max_total_qng; i++, s_addr += ASC_QBLK_SIZE) {
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_FWD),
(uchar) (i + 1));
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_BWD),
(uchar) (i - 1));
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_QNO),
(uchar) i);
}
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_FWD),
(uchar) ASC_QLINK_END);
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_BWD),
(uchar) (asc_dvc->max_total_qng - 1));
AscWriteLramByte(iop_base, (ushort) (s_addr + ASC_SCSIQ_B_QNO),
(uchar) asc_dvc->max_total_qng);
i++;
s_addr += ASC_QBLK_SIZE;
for (; i <= (uchar) (asc_dvc->max_total_qng + 3);
i++, s_addr += ASC_QBLK_SIZE) {
AscWriteLramByte(iop_base,
(ushort) (s_addr + (ushort) ASC_SCSIQ_B_FWD), i);
AscWriteLramByte(iop_base,
(ushort) (s_addr + (ushort) ASC_SCSIQ_B_BWD), i);
AscWriteLramByte(iop_base,
(ushort) (s_addr + (ushort) ASC_SCSIQ_B_QNO), i);
}
return (warn_code);
}
STATIC ushort
AscInitQLinkVar(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc
)
{
PortAddr            iop_base;
int                 i;
ushort              lram_addr;
iop_base = asc_dvc->iop_base;
AscPutRiscVarFreeQHead(iop_base, 1);
AscPutRiscVarDoneQTail(iop_base, asc_dvc->max_total_qng);
AscPutVarFreeQHead(iop_base, 1);
AscPutVarDoneQTail(iop_base, asc_dvc->max_total_qng);
AscWriteLramByte(iop_base, ASCV_BUSY_QHEAD_B,
(uchar) ((int) asc_dvc->max_total_qng + 1));
AscWriteLramByte(iop_base, ASCV_DISC1_QHEAD_B,
(uchar) ((int) asc_dvc->max_total_qng + 2));
AscWriteLramByte(iop_base, (ushort) ASCV_TOTAL_READY_Q_B,
asc_dvc->max_total_qng);
AscWriteLramWord(iop_base, ASCV_ASCDVC_ERR_CODE_W, 0);
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0);
AscWriteLramByte(iop_base, ASCV_STOP_CODE_B, 0);
AscWriteLramByte(iop_base, ASCV_SCSIBUSY_B, 0);
AscWriteLramByte(iop_base, ASCV_WTM_FLAG_B, 0);
AscPutQDoneInProgress(iop_base, 0);
lram_addr = ASC_QADR_BEG;
for (i = 0; i < 32; i++, lram_addr += 2) {
AscWriteLramWord(iop_base, lram_addr, 0);
}
return (0);
}
STATIC int
AscSetLibErrorCode(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
ushort err_code
)
{
if (asc_dvc->err_code == 0) {
asc_dvc->err_code = err_code;
AscWriteLramWord(asc_dvc->iop_base, ASCV_ASCDVC_ERR_CODE_W,
err_code);
}
return (err_code);
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC int
_AscWaitQDone(
PortAddr iop_base,
REG ASC_SCSI_Q * scsiq
)
{
ushort              q_addr;
uchar               q_status;
int                 count = 0;
while (scsiq->q1.q_no == 0) ;
q_addr = ASC_QNO_TO_QADDR(scsiq->q1.q_no);
do {
q_status = AscReadLramByte(iop_base, q_addr + ASC_SCSIQ_B_STATUS);
DvcSleepMilliSecond(100L);
if (count++ > 30) {
return (0);
}
} while ((q_status & QS_READY) != 0);
return (1);
}
#endif
STATIC uchar
AscMsgOutSDTR(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
uchar sdtr_period,
uchar sdtr_offset
)
{
EXT_MSG             sdtr_buf;
uchar               sdtr_period_index;
PortAddr            iop_base;
iop_base = asc_dvc->iop_base;
sdtr_buf.msg_type = MS_EXTEND;
sdtr_buf.msg_len = MS_SDTR_LEN;
sdtr_buf.msg_req = MS_SDTR_CODE;
sdtr_buf.xfer_period = sdtr_period;
sdtr_offset &= ASC_SYN_MAX_OFFSET;
sdtr_buf.req_ack_offset = sdtr_offset;
if ((sdtr_period_index =
AscGetSynPeriodIndex(asc_dvc, sdtr_period)) <=
asc_dvc->max_sdtr_index) {
AscMemWordCopyToLram(iop_base,
ASCV_MSGOUT_BEG,
(ushort *) & sdtr_buf,
(ushort) (sizeof (EXT_MSG) >> 1));
return ((sdtr_period_index << 4) | sdtr_offset);
} else {
sdtr_buf.req_ack_offset = 0;
AscMemWordCopyToLram(iop_base,
ASCV_MSGOUT_BEG,
(ushort *) & sdtr_buf,
(ushort) (sizeof (EXT_MSG) >> 1));
return (0);
}
}
STATIC uchar
AscCalSDTRData(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
uchar sdtr_period,
uchar syn_offset
)
{
uchar               byte;
uchar               sdtr_period_ix;
sdtr_period_ix = AscGetSynPeriodIndex(asc_dvc, sdtr_period);
if (
(sdtr_period_ix > asc_dvc->max_sdtr_index)
) {
return (0xFF);
}
byte = (sdtr_period_ix << 4) | (syn_offset & ASC_SYN_MAX_OFFSET);
return (byte);
}
STATIC void
AscSetChipSDTR(
PortAddr iop_base,
uchar sdtr_data,
uchar tid_no
)
{
AscSetChipSynRegAtID(iop_base, tid_no, sdtr_data);
AscPutMCodeSDTRDoneAtID(iop_base, tid_no, sdtr_data);
return;
}
STATIC uchar
AscGetSynPeriodIndex(
ASC_DVC_VAR asc_ptr_type * asc_dvc,
ruchar syn_time
)
{
ruchar             *period_table;
int                 max_index;
int                 min_index;
int                 i;
period_table = asc_dvc->sdtr_period_tbl;
max_index = (int) asc_dvc->max_sdtr_index;
min_index = (int)asc_dvc->host_init_sdtr_index ;
if ((syn_time <= period_table[max_index])) {
for (i = min_index; i < (max_index - 1); i++) {
if (syn_time <= period_table[i]) {
return ((uchar) i);
}
}
return ((uchar) max_index);
} else {
return ((uchar) (max_index + 1));
}
}
STATIC uchar
AscAllocFreeQueue(
PortAddr iop_base,
uchar free_q_head
)
{
ushort              q_addr;
uchar               next_qp;
uchar               q_status;
q_addr = ASC_QNO_TO_QADDR(free_q_head);
q_status = (uchar) AscReadLramByte(iop_base,
(ushort) (q_addr + ASC_SCSIQ_B_STATUS));
next_qp = AscReadLramByte(iop_base,
(ushort) (q_addr + ASC_SCSIQ_B_FWD));
if (((q_status & QS_READY) == 0) && (next_qp != ASC_QLINK_END)) {
return (next_qp);
}
return (ASC_QLINK_END);
}
STATIC uchar
AscAllocMultipleFreeQueue(
PortAddr iop_base,
uchar free_q_head,
uchar n_free_q
)
{
uchar               i;
for (i = 0; i < n_free_q; i++) {
if ((free_q_head = AscAllocFreeQueue(iop_base, free_q_head))
== ASC_QLINK_END) {
return (ASC_QLINK_END);
}
}
return (free_q_head);
}
STATIC int
AscRiscHaltedAbortSRB(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
ulong srb_ptr
)
{
PortAddr            iop_base;
ushort              q_addr;
uchar               q_no;
ASC_QDONE_INFO      scsiq_buf;
ASC_QDONE_INFO *scsiq;
ASC_ISR_CALLBACK    asc_isr_callback;
int                 last_int_level;
iop_base = asc_dvc->iop_base;
asc_isr_callback = (ASC_ISR_CALLBACK) asc_dvc->isr_callback;
last_int_level = DvcEnterCritical();
scsiq = (ASC_QDONE_INFO *) & scsiq_buf;
for (q_no = ASC_MIN_ACTIVE_QNO; q_no <= asc_dvc->max_total_qng;
q_no++) {
q_addr = ASC_QNO_TO_QADDR(q_no);
scsiq->d2.srb_ptr = AscReadLramDWord(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_D_SRBPTR));
if (scsiq->d2.srb_ptr == srb_ptr) {
_AscCopyLramScsiDoneQ(iop_base, q_addr, scsiq, asc_dvc->max_dma_count);
if (((scsiq->q_status & QS_READY) != 0)
&& ((scsiq->q_status & QS_ABORTED) == 0)
&& ((scsiq->cntl & QCSG_SG_XFER_LIST) == 0)) {
scsiq->q_status |= QS_ABORTED;
scsiq->d3.done_stat = QD_ABORTED_BY_HOST;
AscWriteLramDWord(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_D_SRBPTR),
0L);
AscWriteLramByte(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_B_STATUS),
scsiq->q_status);
(*asc_isr_callback) (asc_dvc, scsiq);
return (1);
}
}
}
DvcLeaveCritical(last_int_level);
return (0);
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC int
AscRiscHaltedAbortTIX(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
uchar target_ix
)
{
PortAddr            iop_base;
ushort              q_addr;
uchar               q_no;
ASC_QDONE_INFO      scsiq_buf;
ASC_QDONE_INFO *scsiq;
ASC_ISR_CALLBACK    asc_isr_callback;
int                 last_int_level;
iop_base = asc_dvc->iop_base;
asc_isr_callback = (ASC_ISR_CALLBACK) asc_dvc->isr_callback;
last_int_level = DvcEnterCritical();
scsiq = (ASC_QDONE_INFO *) & scsiq_buf;
for (q_no = ASC_MIN_ACTIVE_QNO; q_no <= asc_dvc->max_total_qng;
q_no++) {
q_addr = ASC_QNO_TO_QADDR(q_no);
_AscCopyLramScsiDoneQ(iop_base, q_addr, scsiq, asc_dvc->max_dma_count);
if (((scsiq->q_status & QS_READY) != 0) &&
((scsiq->q_status & QS_ABORTED) == 0) &&
((scsiq->cntl & QCSG_SG_XFER_LIST) == 0)) {
if (scsiq->d2.target_ix == target_ix) {
scsiq->q_status |= QS_ABORTED;
scsiq->d3.done_stat = QD_ABORTED_BY_HOST;
AscWriteLramDWord(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_D_SRBPTR),
0L);
AscWriteLramByte(iop_base,
(ushort) (q_addr + (ushort) ASC_SCSIQ_B_STATUS),
scsiq->q_status);
(*asc_isr_callback) (asc_dvc, scsiq);
}
}
}
DvcLeaveCritical(last_int_level);
return (1);
}
#endif
STATIC int
AscHostReqRiscHalt(
PortAddr iop_base
)
{
int                 count = 0;
int                 sta = 0;
uchar               saved_stop_code;
if (AscIsChipHalted(iop_base))
return (1);
saved_stop_code = AscReadLramByte(iop_base, ASCV_STOP_CODE_B);
AscWriteLramByte(iop_base, ASCV_STOP_CODE_B,
ASC_STOP_HOST_REQ_RISC_HALT | ASC_STOP_REQ_RISC_STOP
);
do {
if (AscIsChipHalted(iop_base)) {
sta = 1;
break;
}
DvcSleepMilliSecond(100);
} while (count++ < 20);
AscWriteLramByte(iop_base, ASCV_STOP_CODE_B, saved_stop_code);
return (sta);
}
STATIC int
AscStopQueueExe(
PortAddr iop_base
)
{
int                 count = 0;
if (AscReadLramByte(iop_base, ASCV_STOP_CODE_B) == 0) {
AscWriteLramByte(iop_base, ASCV_STOP_CODE_B,
ASC_STOP_REQ_RISC_STOP);
do {
if (
AscReadLramByte(iop_base, ASCV_STOP_CODE_B) &
ASC_STOP_ACK_RISC_STOP) {
return (1);
}
DvcSleepMilliSecond(100);
} while (count++ < 20);
}
return (0);
}
STATIC int
AscStartQueueExe(
PortAddr iop_base
)
{
if (AscReadLramByte(iop_base, ASCV_STOP_CODE_B) != 0) {
AscWriteLramByte(iop_base, ASCV_STOP_CODE_B, 0);
}
return (1);
}
STATIC int
AscCleanUpBusyQueue(
PortAddr iop_base
)
{
int                 count;
uchar               stop_code;
count = 0;
if (AscReadLramByte(iop_base, ASCV_STOP_CODE_B) != 0) {
AscWriteLramByte(iop_base, ASCV_STOP_CODE_B,
ASC_STOP_CLEAN_UP_BUSY_Q);
do {
stop_code = AscReadLramByte(iop_base, ASCV_STOP_CODE_B);
if ((stop_code & ASC_STOP_CLEAN_UP_BUSY_Q) == 0)
break;
DvcSleepMilliSecond(100);
} while (count++ < 20);
}
return (1);
}
#if LINUX_VERSION_CODE >= ASC_LINUX_VERSION(1,3,89)
STATIC int
AscCleanUpDiscQueue(
PortAddr iop_base
)
{
int                 count;
uchar               stop_code;
count = 0;
if (AscReadLramByte(iop_base, ASCV_STOP_CODE_B) != 0) {
AscWriteLramByte(iop_base, ASCV_STOP_CODE_B,
ASC_STOP_CLEAN_UP_DISC_Q);
do {
stop_code = AscReadLramByte(iop_base, ASCV_STOP_CODE_B);
if ((stop_code & ASC_STOP_CLEAN_UP_DISC_Q) == 0)
break;
DvcSleepMilliSecond(100);
} while (count++ < 20);
}
return (1);
}
#endif
STATIC int
AscWaitTixISRDone(
ASC_DVC_VAR asc_ptr_type * asc_dvc,
uchar target_ix
)
{
uchar               cur_req;
uchar               tid_no;
int                 i = 0;
tid_no = ASC_TIX_TO_TID(target_ix);
while (i++ < 10) {
if ((cur_req = asc_dvc->cur_dvc_qng[tid_no]) == 0) {
break;
}
DvcSleepMilliSecond(1000L);
if (asc_dvc->cur_dvc_qng[tid_no] == cur_req) {
break;
}
}
return (1);
}
STATIC int
AscWaitISRDone(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc
)
{
int                 tid;
for (tid = 0; tid <= ASC_MAX_TID; tid++) {
AscWaitTixISRDone(asc_dvc, ASC_TID_TO_TIX(tid));
}
return (1);
}
STATIC ulong
AscGetOnePhyAddr(
REG ASC_DVC_VAR asc_ptr_type * asc_dvc,
uchar * buf_addr,
ulong buf_size
)
{
ASC_MIN_SG_HEAD     sg_head;
sg_head.entry_cnt = ASC_MIN_SG_LIST;
if (DvcGetSGList(asc_dvc, (uchar *) buf_addr,
buf_size, (ASC_SG_HEAD *) & sg_head) != buf_size) {
return (0L);
}
if (sg_head.entry_cnt > 1) {
return (0L);
}
return (sg_head.sg_list[0].addr);
}
STATIC void
DvcDelayMicroSecond(ADV_DVC_VAR *asc_dvc, ushort micro_sec)
{
udelay(micro_sec);
}
STATIC void
DvcDelayNanoSecond(ASC_DVC_VAR asc_ptr_type * asc_dvc, ulong nano_sec)
{
udelay((nano_sec + 999)/1000);
}
ASC_INITFUNC(
STATIC ulong
AscGetEisaProductID(
PortAddr iop_base
)
)
{
PortAddr            eisa_iop;
ushort              product_id_high, product_id_low;
ulong               product_id;
eisa_iop = ASC_GET_EISA_SLOT(iop_base) | ASC_EISA_PID_IOP_MASK;
product_id_low = inpw(eisa_iop);
product_id_high = inpw(eisa_iop + 2);
product_id = ((ulong) product_id_high << 16) | (ulong) product_id_low;
return (product_id);
}
ASC_INITFUNC(
STATIC PortAddr
AscSearchIOPortAddrEISA(
PortAddr iop_base
)
)
{
ulong               eisa_product_id;
if (iop_base == 0) {
iop_base = ASC_EISA_MIN_IOP_ADDR;
} else {
if (iop_base == ASC_EISA_MAX_IOP_ADDR)
return (0);
if ((iop_base & 0x0050) == 0x0050) {
iop_base += ASC_EISA_BIG_IOP_GAP;
} else {
iop_base += ASC_EISA_SMALL_IOP_GAP;
}
}
while (iop_base <= ASC_EISA_MAX_IOP_ADDR) {
eisa_product_id = AscGetEisaProductID(iop_base);
if ((eisa_product_id == ASC_EISA_ID_740) ||
(eisa_product_id == ASC_EISA_ID_750)) {
if (AscFindSignature(iop_base)) {
inpw(iop_base + 4);
return (iop_base);
}
}
if (iop_base == ASC_EISA_MAX_IOP_ADDR)
return (0);
if ((iop_base & 0x0050) == 0x0050) {
iop_base += ASC_EISA_BIG_IOP_GAP;
} else {
iop_base += ASC_EISA_SMALL_IOP_GAP;
}
}
return (0);
}
STATIC int
AscStartChip(
PortAddr iop_base
)
{
AscSetChipControl(iop_base, 0);
if ((AscGetChipStatus(iop_base) & CSW_HALTED) != 0) {
return (0);
}
return (1);
}
STATIC int
AscStopChip(
PortAddr iop_base
)
{
uchar               cc_val;
cc_val = AscGetChipControl(iop_base) & (~(CC_SINGLE_STEP | CC_TEST | CC_DIAG));
AscSetChipControl(iop_base, (uchar) (cc_val | CC_HALT));
AscSetChipIH(iop_base, INS_HALT);
AscSetChipIH(iop_base, INS_RFLAG_WTM);
if ((AscGetChipStatus(iop_base) & CSW_HALTED) == 0) {
return (0);
}
return (1);
}
STATIC int
AscIsChipHalted(
PortAddr iop_base
)
{
if ((AscGetChipStatus(iop_base) & CSW_HALTED) != 0) {
if ((AscGetChipControl(iop_base) & CC_HALT) != 0) {
return (1);
}
}
return (0);
}
STATIC void
AscSetChipIH(
PortAddr iop_base,
ushort ins_code
)
{
AscSetBank(iop_base, 1);
AscWriteChipIH(iop_base, ins_code);
AscSetBank(iop_base, 0);
return;
}
STATIC void
AscAckInterrupt(
PortAddr iop_base
)
{
uchar               host_flag;
uchar               risc_flag;
ushort              loop;
loop = 0;
do {
risc_flag = AscReadLramByte(iop_base, ASCV_RISC_FLAG_B);
if (loop++ > 0x7FFF) {
break;
}
} while ((risc_flag & ASC_RISC_FLAG_GEN_INT) != 0);
host_flag = AscReadLramByte(iop_base, ASCV_HOST_FLAG_B) & (~ASC_HOST_FLAG_ACK_INT);
AscWriteLramByte(iop_base, ASCV_HOST_FLAG_B,
(uchar) (host_flag | ASC_HOST_FLAG_ACK_INT));
AscSetChipStatus(iop_base, CIW_INT_ACK);
loop = 0;
while (AscGetChipStatus(iop_base) & CSW_INT_PENDING) {
AscSetChipStatus(iop_base, CIW_INT_ACK);
if (loop++ > 3) {
break;
}
}
AscWriteLramByte(iop_base, ASCV_HOST_FLAG_B, host_flag);
return;
}
STATIC void
AscDisableInterrupt(
PortAddr iop_base
)
{
ushort              cfg;
cfg = AscGetChipCfgLsw(iop_base);
AscSetChipCfgLsw(iop_base, cfg & (~ASC_CFG0_HOST_INT_ON));
return;
}
STATIC void
AscEnableInterrupt(
PortAddr iop_base
)
{
ushort              cfg;
cfg = AscGetChipCfgLsw(iop_base);
AscSetChipCfgLsw(iop_base, cfg | ASC_CFG0_HOST_INT_ON);
return;
}
STATIC void
AscSetBank(
PortAddr iop_base,
uchar bank
)
{
uchar               val;
val = AscGetChipControl(iop_base) &
(~(CC_SINGLE_STEP | CC_TEST | CC_DIAG | CC_SCSI_RESET | CC_CHIP_RESET));
if (bank == 1) {
val |= CC_BANK_ONE;
} else if (bank == 2) {
val |= CC_DIAG | CC_BANK_ONE;
} else {
val &= ~CC_BANK_ONE;
}
AscSetChipControl(iop_base, val);
return;
}
STATIC int
AscResetChipAndScsiBus(
ASC_DVC_VAR *asc_dvc
)
{
PortAddr    iop_base;
iop_base = asc_dvc->iop_base;
while (AscGetChipStatus(iop_base) & CSW_SCSI_RESET_ACTIVE) ;
AscStopChip(iop_base);
AscSetChipControl(iop_base, CC_CHIP_RESET | CC_SCSI_RESET | CC_HALT);
DvcDelayNanoSecond(asc_dvc, 60000);
AscSetChipIH(iop_base, INS_RFLAG_WTM);
AscSetChipIH(iop_base, INS_HALT);
AscSetChipControl(iop_base, CC_CHIP_RESET | CC_HALT);
AscSetChipControl(iop_base, CC_HALT);
DvcSleepMilliSecond(200);
AscSetChipStatus(iop_base, CIW_CLR_SCSI_RESET_INT);
AscSetChipStatus(iop_base, 0);
return (AscIsChipHalted(iop_base));
}
ASC_INITFUNC(
STATIC ulong
AscGetMaxDmaCount(
ushort bus_type
)
)
{
if (bus_type & ASC_IS_ISA)
return (ASC_MAX_ISA_DMA_COUNT);
else if (bus_type & (ASC_IS_EISA | ASC_IS_VL))
return (ASC_MAX_VL_DMA_COUNT);
return (ASC_MAX_PCI_DMA_COUNT);
}
ASC_INITFUNC(
STATIC ushort
AscGetIsaDmaChannel(
PortAddr iop_base
)
)
{
ushort              channel;
channel = AscGetChipCfgLsw(iop_base) & 0x0003;
if (channel == 0x03)
return (0);
else if (channel == 0x00)
return (7);
return (channel + 4);
}
ASC_INITFUNC(
STATIC ushort
AscSetIsaDmaChannel(
PortAddr iop_base,
ushort dma_channel
)
)
{
ushort              cfg_lsw;
uchar               value;
if ((dma_channel >= 5) && (dma_channel <= 7)) {
if (dma_channel == 7)
value = 0x00;
else
value = dma_channel - 4;
cfg_lsw = AscGetChipCfgLsw(iop_base) & 0xFFFC;
cfg_lsw |= value;
AscSetChipCfgLsw(iop_base, cfg_lsw);
return (AscGetIsaDmaChannel(iop_base));
}
return (0);
}
ASC_INITFUNC(
STATIC uchar
AscSetIsaDmaSpeed(
PortAddr iop_base,
uchar speed_value
)
)
{
speed_value &= 0x07;
AscSetBank(iop_base, 1);
AscWriteChipDmaSpeed(iop_base, speed_value);
AscSetBank(iop_base, 0);
return (AscGetIsaDmaSpeed(iop_base));
}
ASC_INITFUNC(
STATIC uchar
AscGetIsaDmaSpeed(
PortAddr iop_base
)
)
{
uchar               speed_value;
AscSetBank(iop_base, 1);
speed_value = AscReadChipDmaSpeed(iop_base);
speed_value &= 0x07;
AscSetBank(iop_base, 0);
return (speed_value);
}
ASC_INITFUNC(
STATIC ushort
AscReadPCIConfigWord(
ASC_DVC_VAR asc_ptr_type *asc_dvc,
ushort pci_config_offset)
)
{
uchar       lsb, msb;
lsb = DvcReadPCIConfigByte(asc_dvc, pci_config_offset);
msb = DvcReadPCIConfigByte(asc_dvc, pci_config_offset + 1);
return ((ushort) ((msb << 8) | lsb));
}
ASC_INITFUNC(
STATIC ushort
AscInitGetConfig(
ASC_DVC_VAR asc_ptr_type * asc_dvc
)
)
{
ushort              warn_code;
PortAddr            iop_base;
ushort              PCIDeviceID;
ushort              PCIVendorID;
uchar               PCIRevisionID;
uchar               prevCmdRegBits;
warn_code = 0;
iop_base = asc_dvc->iop_base;
asc_dvc->init_state = ASC_INIT_STATE_BEG_GET_CFG;
if (asc_dvc->err_code != 0) {
return (UW_ERR);
}
if (asc_dvc->bus_type == ASC_IS_PCI) {
PCIVendorID = AscReadPCIConfigWord(asc_dvc,
AscPCIConfigVendorIDRegister);
PCIDeviceID = AscReadPCIConfigWord(asc_dvc,
AscPCIConfigDeviceIDRegister);
PCIRevisionID = DvcReadPCIConfigByte(asc_dvc,
AscPCIConfigRevisionIDRegister);
if (PCIVendorID != ASC_PCI_VENDORID) {
warn_code |= ASC_WARN_SET_PCI_CONFIG_SPACE;
}
prevCmdRegBits = DvcReadPCIConfigByte(asc_dvc,
AscPCIConfigCommandRegister);
if ((prevCmdRegBits & AscPCICmdRegBits_IOMemBusMaster) !=
AscPCICmdRegBits_IOMemBusMaster) {
DvcWritePCIConfigByte(asc_dvc,
AscPCIConfigCommandRegister,
(prevCmdRegBits |
AscPCICmdRegBits_IOMemBusMaster));
if ((DvcReadPCIConfigByte(asc_dvc,
AscPCIConfigCommandRegister)
& AscPCICmdRegBits_IOMemBusMaster)
!= AscPCICmdRegBits_IOMemBusMaster) {
warn_code |= ASC_WARN_SET_PCI_CONFIG_SPACE;
}
}
if ((PCIDeviceID == ASC_PCI_DEVICEID_1200A) ||
(PCIDeviceID == ASC_PCI_DEVICEID_1200B)) {
DvcWritePCIConfigByte(asc_dvc,
AscPCIConfigLatencyTimer, 0x00);
if (DvcReadPCIConfigByte(asc_dvc, AscPCIConfigLatencyTimer)
!= 0x00) {
warn_code |= ASC_WARN_SET_PCI_CONFIG_SPACE;
}
} else if (PCIDeviceID == ASC_PCI_DEVICEID_ULTRA) {
if (DvcReadPCIConfigByte(asc_dvc,
AscPCIConfigLatencyTimer) < 0x20) {
DvcWritePCIConfigByte(asc_dvc,
AscPCIConfigLatencyTimer, 0x20);
if (DvcReadPCIConfigByte(asc_dvc,
AscPCIConfigLatencyTimer) < 0x20) {
warn_code |= ASC_WARN_SET_PCI_CONFIG_SPACE;
}
}
}
}
if (AscFindSignature(iop_base)) {
warn_code |= AscInitAscDvcVar(asc_dvc);
warn_code |= AscInitFromEEP(asc_dvc);
asc_dvc->init_state |= ASC_INIT_STATE_END_GET_CFG;
if (asc_dvc->scsi_reset_wait > ASC_MAX_SCSI_RESET_WAIT) {
asc_dvc->scsi_reset_wait = ASC_MAX_SCSI_RESET_WAIT;
}
} else {
asc_dvc->err_code = ASC_IERR_BAD_SIGNATURE;
}
return(warn_code);
}
ASC_INITFUNC(
STATIC ushort
AscInitSetConfig(
ASC_DVC_VAR asc_ptr_type * asc_dvc
)
)
{
ushort              warn_code = 0;
asc_dvc->init_state |= ASC_INIT_STATE_BEG_SET_CFG;
if (asc_dvc->err_code != 0)
return (UW_ERR);
if (AscFindSignature(asc_dvc->iop_base)) {
warn_code |= AscInitFromAscDvcVar(asc_dvc);
asc_dvc->init_state |= ASC_INIT_STATE_END_SET_CFG;
} else {
asc_dvc->err_code = ASC_IERR_BAD_SIGNATURE;
}
return (warn_code);
}
ASC_INITFUNC(
STATIC ushort
AscInitFromAscDvcVar(
ASC_DVC_VAR asc_ptr_type * asc_dvc
)
)
{
PortAddr            iop_base;
ushort              cfg_msw;
ushort              warn_code;
ushort              pci_device_id;
iop_base = asc_dvc->iop_base;
pci_device_id = asc_dvc->cfg->pci_device_id;
warn_code = 0;
cfg_msw = AscGetChipCfgMsw(iop_base);
if ((cfg_msw & ASC_CFG_MSW_CLR_MASK) != 0) {
cfg_msw &= (~(ASC_CFG_MSW_CLR_MASK));
warn_code |= ASC_WARN_CFG_MSW_RECOVER;
AscSetChipCfgMsw(iop_base, cfg_msw);
}
if ((asc_dvc->cfg->cmd_qng_enabled & asc_dvc->cfg->disc_enable) !=
asc_dvc->cfg->cmd_qng_enabled) {
asc_dvc->cfg->disc_enable = asc_dvc->cfg->cmd_qng_enabled;
warn_code |= ASC_WARN_CMD_QNG_CONFLICT;
}
if (AscGetChipStatus(iop_base) & CSW_AUTO_CONFIG) {
warn_code |= ASC_WARN_AUTO_CONFIG;
}
if ((asc_dvc->bus_type & (ASC_IS_ISA | ASC_IS_VL)) != 0) {
if (AscSetChipIRQ(iop_base, asc_dvc->irq_no, asc_dvc->bus_type)
!= asc_dvc->irq_no) {
asc_dvc->err_code |= ASC_IERR_SET_IRQ_NO;
}
}
if (asc_dvc->bus_type & ASC_IS_PCI) {
cfg_msw &= 0xFFC0;
AscSetChipCfgMsw(iop_base, cfg_msw);
if ((asc_dvc->bus_type & ASC_IS_PCI_ULTRA) == ASC_IS_PCI_ULTRA) {
} else {
if ((pci_device_id == ASC_PCI_DEVICE_ID_REV_A) ||
(pci_device_id == ASC_PCI_DEVICE_ID_REV_B)) {
asc_dvc->bug_fix_cntl |= ASC_BUG_FIX_IF_NOT_DWB;
asc_dvc->bug_fix_cntl |= ASC_BUG_FIX_ASYN_USE_SYN;
}
}
} else if (asc_dvc->bus_type == ASC_IS_ISAPNP) {
if (AscGetChipVersion(iop_base, asc_dvc->bus_type)
== ASC_CHIP_VER_ASYN_BUG) {
asc_dvc->bug_fix_cntl |= ASC_BUG_FIX_ASYN_USE_SYN;
}
}
if (AscSetChipScsiID(iop_base, asc_dvc->cfg->chip_scsi_id) !=
asc_dvc->cfg->chip_scsi_id) {
asc_dvc->err_code |= ASC_IERR_SET_SCSI_ID;
}
if (asc_dvc->bus_type & ASC_IS_ISA) {
AscSetIsaDmaChannel(iop_base, asc_dvc->cfg->isa_dma_channel);
AscSetIsaDmaSpeed(iop_base, asc_dvc->cfg->isa_dma_speed);
}
return (warn_code);
}
ASC_INITFUNC(
STATIC ushort
AscInitAsc1000Driver(
ASC_DVC_VAR asc_ptr_type * asc_dvc
)
)
{
ushort              warn_code;
PortAddr            iop_base;
extern ushort       _asc_mcode_size;
extern ulong        _asc_mcode_chksum;
extern uchar        _asc_mcode_buf[];
iop_base = asc_dvc->iop_base;
warn_code = 0;
if ((asc_dvc->dvc_cntl & ASC_CNTL_RESET_SCSI) &&
!(asc_dvc->init_state & ASC_INIT_RESET_SCSI_DONE)) {
AscResetChipAndScsiBus(asc_dvc);
DvcSleepMilliSecond((ulong) ((ushort) asc_dvc->scsi_reset_wait * 1000));
}
asc_dvc->init_state |= ASC_INIT_STATE_BEG_LOAD_MC;
if (asc_dvc->err_code != 0)
return (UW_ERR);
if (!AscFindSignature(asc_dvc->iop_base)) {
asc_dvc->err_code = ASC_IERR_BAD_SIGNATURE;
return (warn_code);
}
AscDisableInterrupt(iop_base);
warn_code |= AscInitLram(asc_dvc);
if (asc_dvc->err_code != 0)
return (UW_ERR);
if (AscLoadMicroCode(iop_base, 0, (ushort *) _asc_mcode_buf,
_asc_mcode_size) != _asc_mcode_chksum) {
asc_dvc->err_code |= ASC_IERR_MCODE_CHKSUM;
return (warn_code);
}
warn_code |= AscInitMicroCodeVar(asc_dvc);
asc_dvc->init_state |= ASC_INIT_STATE_END_LOAD_MC;
AscEnableInterrupt(iop_base);
return (warn_code);
}
ASC_INITFUNC(
STATIC ushort
AscInitAscDvcVar(
ASC_DVC_VAR asc_ptr_type * asc_dvc
)
)
{
int                 i;
PortAddr            iop_base;
ushort              warn_code;
uchar               chip_version;
iop_base = asc_dvc->iop_base;
warn_code = 0;
asc_dvc->err_code = 0;
if ((asc_dvc->bus_type &
(ASC_IS_ISA | ASC_IS_PCI | ASC_IS_EISA | ASC_IS_VL)) == 0) {
asc_dvc->err_code |= ASC_IERR_NO_BUS_TYPE;
}
AscSetChipControl(iop_base, CC_HALT);
AscSetChipStatus(iop_base, 0);
asc_dvc->bug_fix_cntl = 0;
asc_dvc->pci_fix_asyn_xfer = 0;
asc_dvc->pci_fix_asyn_xfer_always = 0;
asc_dvc->init_state = 0;
asc_dvc->sdtr_done = 0;
asc_dvc->cur_total_qng = 0;
asc_dvc->is_in_int = 0;
asc_dvc->in_critical_cnt = 0;
asc_dvc->last_q_shortage = 0;
asc_dvc->use_tagged_qng = 0;
asc_dvc->no_scam = 0;
asc_dvc->unit_not_ready = 0;
asc_dvc->queue_full_or_busy = 0;
asc_dvc->redo_scam = 0 ;
asc_dvc->res2 = 0 ;
asc_dvc->host_init_sdtr_index = 0 ;
asc_dvc->res7 = 0 ;
asc_dvc->res8 = 0 ;
asc_dvc->cfg->can_tagged_qng = 0 ;
asc_dvc->cfg->cmd_qng_enabled = 0;
asc_dvc->dvc_cntl = ASC_DEF_DVC_CNTL;
asc_dvc->init_sdtr = 0;
asc_dvc->max_total_qng = ASC_DEF_MAX_TOTAL_QNG;
asc_dvc->scsi_reset_wait = 3;
asc_dvc->start_motor = ASC_SCSI_WIDTH_BIT_SET;
asc_dvc->max_dma_count = AscGetMaxDmaCount(asc_dvc->bus_type);
asc_dvc->cfg->sdtr_enable = ASC_SCSI_WIDTH_BIT_SET;
asc_dvc->cfg->disc_enable = ASC_SCSI_WIDTH_BIT_SET;
asc_dvc->cfg->chip_scsi_id = ASC_DEF_CHIP_SCSI_ID;
asc_dvc->cfg->lib_serial_no = ASC_LIB_SERIAL_NUMBER;
asc_dvc->cfg->lib_version = (ASC_LIB_VERSION_MAJOR << 8) |
ASC_LIB_VERSION_MINOR;
chip_version = AscGetChipVersion(iop_base, asc_dvc->bus_type);
asc_dvc->cfg->chip_version = chip_version;
asc_dvc->sdtr_period_tbl[0] = SYN_XFER_NS_0;
asc_dvc->sdtr_period_tbl[1] = SYN_XFER_NS_1;
asc_dvc->sdtr_period_tbl[2] = SYN_XFER_NS_2;
asc_dvc->sdtr_period_tbl[3] = SYN_XFER_NS_3;
asc_dvc->sdtr_period_tbl[4] = SYN_XFER_NS_4;
asc_dvc->sdtr_period_tbl[5] = SYN_XFER_NS_5;
asc_dvc->sdtr_period_tbl[6] = SYN_XFER_NS_6;
asc_dvc->sdtr_period_tbl[7] = SYN_XFER_NS_7;
asc_dvc->max_sdtr_index = 7;
if ((asc_dvc->bus_type & ASC_IS_PCI) &&
(chip_version >= ASC_CHIP_VER_PCI_ULTRA_3150)) {
asc_dvc->bus_type = ASC_IS_PCI_ULTRA;
asc_dvc->sdtr_period_tbl[0] = SYN_ULTRA_XFER_NS_0;
asc_dvc->sdtr_period_tbl[1] = SYN_ULTRA_XFER_NS_1;
asc_dvc->sdtr_period_tbl[2] = SYN_ULTRA_XFER_NS_2;
asc_dvc->sdtr_period_tbl[3] = SYN_ULTRA_XFER_NS_3;
asc_dvc->sdtr_period_tbl[4] = SYN_ULTRA_XFER_NS_4;
asc_dvc->sdtr_period_tbl[5] = SYN_ULTRA_XFER_NS_5;
asc_dvc->sdtr_period_tbl[6] = SYN_ULTRA_XFER_NS_6;
asc_dvc->sdtr_period_tbl[7] = SYN_ULTRA_XFER_NS_7;
asc_dvc->sdtr_period_tbl[8] = SYN_ULTRA_XFER_NS_8;
asc_dvc->sdtr_period_tbl[9] = SYN_ULTRA_XFER_NS_9;
asc_dvc->sdtr_period_tbl[10] = SYN_ULTRA_XFER_NS_10;
asc_dvc->sdtr_period_tbl[11] = SYN_ULTRA_XFER_NS_11;
asc_dvc->sdtr_period_tbl[12] = SYN_ULTRA_XFER_NS_12;
asc_dvc->sdtr_period_tbl[13] = SYN_ULTRA_XFER_NS_13;
asc_dvc->sdtr_period_tbl[14] = SYN_ULTRA_XFER_NS_14;
asc_dvc->sdtr_period_tbl[15] = SYN_ULTRA_XFER_NS_15;
asc_dvc->max_sdtr_index = 15;
if (chip_version == ASC_CHIP_VER_PCI_ULTRA_3150)
{
AscSetExtraControl(iop_base,
(SEC_ACTIVE_NEGATE | SEC_SLEW_RATE));
} else if (chip_version >= ASC_CHIP_VER_PCI_ULTRA_3050) {
AscSetExtraControl(iop_base,
(SEC_ACTIVE_NEGATE | SEC_ENABLE_FILTER));
}
}
if (asc_dvc->bus_type == ASC_IS_PCI) {
AscSetExtraControl(iop_base, (SEC_ACTIVE_NEGATE | SEC_SLEW_RATE));
}
asc_dvc->cfg->isa_dma_speed = ASC_DEF_ISA_DMA_SPEED;
if (AscGetChipBusType(iop_base) == ASC_IS_ISAPNP) {
AscSetChipIFC(iop_base, IFC_INIT_DEFAULT);
asc_dvc->bus_type = ASC_IS_ISAPNP;
}
if ((asc_dvc->bus_type & ASC_IS_ISA) != 0) {
asc_dvc->cfg->isa_dma_channel = (uchar) AscGetIsaDmaChannel(iop_base);
}
for (i = 0; i <= ASC_MAX_TID; i++) {
asc_dvc->cur_dvc_qng[i] = 0;
asc_dvc->max_dvc_qng[i] = ASC_MAX_SCSI1_QNG;
asc_dvc->scsiq_busy_head[i] = (ASC_SCSI_Q *) 0L;
asc_dvc->scsiq_busy_tail[i] = (ASC_SCSI_Q *) 0L;
asc_dvc->cfg->max_tag_qng[i] = ASC_MAX_INRAM_TAG_QNG;
}
return (warn_code);
}
ASC_INITFUNC(
STATIC ushort
AscInitFromEEP(
ASC_DVC_VAR asc_ptr_type * asc_dvc
)
)
{
ASCEEP_CONFIG       eep_config_buf;
ASCEEP_CONFIG       *eep_config;
PortAddr            iop_base;
ushort              chksum;
ushort              warn_code;
ushort              cfg_msw, cfg_lsw;
int                 i;
int                 write_eep = 0;
iop_base = asc_dvc->iop_base;
warn_code = 0;
AscWriteLramWord(iop_base, ASCV_HALTCODE_W, 0x00FE);
AscStopQueueExe(iop_base);
if ((AscStopChip(iop_base) == FALSE) ||
(AscGetChipScsiCtrl(iop_base) != 0)) {
asc_dvc->init_state |= ASC_INIT_RESET_SCSI_DONE;
AscResetChipAndScsiBus(asc_dvc);
DvcSleepMilliSecond((ulong) ((ushort) asc_dvc->scsi_reset_wait * 1000));
}
if (AscIsChipHalted(iop_base) == FALSE) {
asc_dvc->err_code |= ASC_IERR_START_STOP_CHIP;
return (warn_code);
}
AscSetPCAddr(iop_base, ASC_MCODE_START_ADDR);
if (AscGetPCAddr(iop_base) != ASC_MCODE_START_ADDR) {
asc_dvc->err_code |= ASC_IERR_SET_PC_ADDR;
return (warn_code);
}
eep_config = (ASCEEP_CONFIG *) & eep_config_buf;
cfg_msw = AscGetChipCfgMsw(iop_base);
cfg_lsw = AscGetChipCfgLsw(iop_base);
if ((cfg_msw & ASC_CFG_MSW_CLR_MASK) != 0) {
cfg_msw &= (~(ASC_CFG_MSW_CLR_MASK));
warn_code |= ASC_WARN_CFG_MSW_RECOVER;
AscSetChipCfgMsw(iop_base, cfg_msw);
}
chksum = AscGetEEPConfig(iop_base, eep_config, asc_dvc->bus_type);
if (chksum == 0) {
chksum = 0xaa55;
}
if (AscGetChipStatus(iop_base) & CSW_AUTO_CONFIG) {
warn_code |= ASC_WARN_AUTO_CONFIG;
if (asc_dvc->cfg->chip_version == 3) {
if (eep_config->cfg_lsw != cfg_lsw) {
warn_code |= ASC_WARN_EEPROM_RECOVER;
eep_config->cfg_lsw = AscGetChipCfgLsw(iop_base);
}
if (eep_config->cfg_msw != cfg_msw) {
warn_code |= ASC_WARN_EEPROM_RECOVER;
eep_config->cfg_msw = AscGetChipCfgMsw(iop_base);
}
}
}
eep_config->cfg_msw &= ~ASC_CFG_MSW_CLR_MASK;
eep_config->cfg_lsw |= ASC_CFG0_HOST_INT_ON;
if (chksum != eep_config->chksum) {
if (AscGetChipVersion(iop_base, asc_dvc->bus_type) ==
ASC_CHIP_VER_PCI_ULTRA_3050 )
{
eep_config->init_sdtr = 0xFF;
eep_config->disc_enable = 0xFF;
eep_config->start_motor = 0xFF;
eep_config->use_cmd_qng = 0;
eep_config->max_total_qng = 0xF0;
eep_config->max_tag_qng = 0x20;
eep_config->cntl = 0xBFFF;
eep_config->chip_scsi_id = 7;
eep_config->no_scam = 0;
eep_config->adapter_info[0] = 0;
eep_config->adapter_info[1] = 0;
eep_config->adapter_info[2] = 0;
eep_config->adapter_info[3] = 0;
eep_config->adapter_info[4] = 0;
eep_config->adapter_info[5] = 0xBB;
} else {
write_eep = 1 ;
warn_code |= ASC_WARN_EEPROM_CHKSUM ;
}
}
asc_dvc->cfg->sdtr_enable = eep_config->init_sdtr ;
asc_dvc->cfg->disc_enable = eep_config->disc_enable;
asc_dvc->cfg->cmd_qng_enabled = eep_config->use_cmd_qng;
asc_dvc->cfg->isa_dma_speed = eep_config->isa_dma_speed;
asc_dvc->start_motor = eep_config->start_motor;
asc_dvc->dvc_cntl = eep_config->cntl;
asc_dvc->no_scam = eep_config->no_scam;
asc_dvc->cfg->adapter_info[0] = eep_config->adapter_info[0];
asc_dvc->cfg->adapter_info[1] = eep_config->adapter_info[1];
asc_dvc->cfg->adapter_info[2] = eep_config->adapter_info[2];
asc_dvc->cfg->adapter_info[3] = eep_config->adapter_info[3];
asc_dvc->cfg->adapter_info[4] = eep_config->adapter_info[4];
asc_dvc->cfg->adapter_info[5] = eep_config->adapter_info[5];
if (!AscTestExternalLram(asc_dvc)) {
if (((asc_dvc->bus_type & ASC_IS_PCI_ULTRA) == ASC_IS_PCI_ULTRA)) {
eep_config->max_total_qng = ASC_MAX_PCI_ULTRA_INRAM_TOTAL_QNG;
eep_config->max_tag_qng = ASC_MAX_PCI_ULTRA_INRAM_TAG_QNG;
} else {
eep_config->cfg_msw |= 0x0800;
cfg_msw |= 0x0800;
AscSetChipCfgMsw(iop_base, cfg_msw);
eep_config->max_total_qng = ASC_MAX_PCI_INRAM_TOTAL_QNG;
eep_config->max_tag_qng = ASC_MAX_INRAM_TAG_QNG;
}
} else {
}
if (eep_config->max_total_qng < ASC_MIN_TOTAL_QNG) {
eep_config->max_total_qng = ASC_MIN_TOTAL_QNG;
}
if (eep_config->max_total_qng > ASC_MAX_TOTAL_QNG) {
eep_config->max_total_qng = ASC_MAX_TOTAL_QNG;
}
if (eep_config->max_tag_qng > eep_config->max_total_qng) {
eep_config->max_tag_qng = eep_config->max_total_qng;
}
if (eep_config->max_tag_qng < ASC_MIN_TAG_Q_PER_DVC) {
eep_config->max_tag_qng = ASC_MIN_TAG_Q_PER_DVC;
}
asc_dvc->max_total_qng = eep_config->max_total_qng;
if ((eep_config->use_cmd_qng & eep_config->disc_enable) !=
eep_config->use_cmd_qng) {
eep_config->disc_enable = eep_config->use_cmd_qng;
warn_code |= ASC_WARN_CMD_QNG_CONFLICT;
}
if (asc_dvc->bus_type & (ASC_IS_ISA | ASC_IS_VL | ASC_IS_EISA)) {
asc_dvc->irq_no = AscGetChipIRQ(iop_base, asc_dvc->bus_type);
}
eep_config->chip_scsi_id &= ASC_MAX_TID;
asc_dvc->cfg->chip_scsi_id = eep_config->chip_scsi_id;
if (((asc_dvc->bus_type & ASC_IS_PCI_ULTRA) == ASC_IS_PCI_ULTRA) &&
!(asc_dvc->dvc_cntl & ASC_CNTL_SDTR_ENABLE_ULTRA)) {
asc_dvc->host_init_sdtr_index = ASC_SDTR_ULTRA_PCI_10MB_INDEX;
}
for (i = 0; i <= ASC_MAX_TID; i++) {
asc_dvc->dos_int13_table[i] = eep_config->dos_int13_table[i];
asc_dvc->cfg->max_tag_qng[i] = eep_config->max_tag_qng;
asc_dvc->cfg->sdtr_period_offset[i] =
(uchar) (ASC_DEF_SDTR_OFFSET |
(asc_dvc->host_init_sdtr_index << 4));
}
eep_config->cfg_msw = AscGetChipCfgMsw(iop_base);
if (write_eep) {
(void) AscSetEEPConfig(iop_base, eep_config, asc_dvc->bus_type);
}
return (warn_code);
}
ASC_INITFUNC(
STATIC ushort
AscInitMicroCodeVar(
ASC_DVC_VAR asc_ptr_type * asc_dvc
)
)
{
int                 i;
ushort              warn_code;
PortAddr            iop_base;
ulong               phy_addr;
iop_base = asc_dvc->iop_base;
warn_code = 0;
for (i = 0; i <= ASC_MAX_TID; i++) {
AscPutMCodeInitSDTRAtID(iop_base, i,
asc_dvc->cfg->sdtr_period_offset[i]
);
}
AscInitQLinkVar(asc_dvc);
AscWriteLramByte(iop_base, ASCV_DISC_ENABLE_B,
asc_dvc->cfg->disc_enable);
AscWriteLramByte(iop_base, ASCV_HOSTSCSI_ID_B,
ASC_TID_TO_TARGET_ID(asc_dvc->cfg->chip_scsi_id));
if ((phy_addr = AscGetOnePhyAddr(asc_dvc,
(uchar *) asc_dvc->cfg->overrun_buf,
ASC_OVERRUN_BSIZE)) == 0L) {
asc_dvc->err_code |= ASC_IERR_GET_PHY_ADDR;
} else {
phy_addr = (phy_addr & 0xFFFFFFF8UL) + 8;
AscWriteLramDWord(iop_base, ASCV_OVERRUN_PADDR_D, phy_addr);
AscWriteLramDWord(iop_base, ASCV_OVERRUN_BSIZE_D,
ASC_OVERRUN_BSIZE - 8);
}
asc_dvc->cfg->mcode_date = AscReadLramWord(iop_base,
(ushort) ASCV_MC_DATE_W);
asc_dvc->cfg->mcode_version = AscReadLramWord(iop_base,
(ushort) ASCV_MC_VER_W);
AscSetPCAddr(iop_base, ASC_MCODE_START_ADDR);
if (AscGetPCAddr(iop_base) != ASC_MCODE_START_ADDR) {
asc_dvc->err_code |= ASC_IERR_SET_PC_ADDR;
return (warn_code);
}
if (AscStartChip(iop_base) != 1) {
asc_dvc->err_code |= ASC_IERR_START_STOP_CHIP;
return (warn_code);
}
return (warn_code);
}
ASC_INITFUNC(
STATIC int
AscTestExternalLram(
ASC_DVC_VAR asc_ptr_type * asc_dvc
)
)
{
PortAddr            iop_base;
ushort              q_addr;
ushort              saved_word;
int                 sta;
iop_base = asc_dvc->iop_base;
sta = 0;
q_addr = ASC_QNO_TO_QADDR(241);
saved_word = AscReadLramWord(iop_base, q_addr);
AscSetChipLramAddr(iop_base, q_addr);
AscSetChipLramData(iop_base, 0x55AA);
DvcSleepMilliSecond(10);
AscSetChipLramAddr(iop_base, q_addr);
if (AscGetChipLramData(iop_base) == 0x55AA) {
sta = 1;
AscWriteLramWord(iop_base, q_addr, saved_word);
}
return (sta);
}
ASC_INITFUNC(
STATIC int
AscWriteEEPCmdReg(
PortAddr iop_base,
uchar cmd_reg
)
)
{
uchar               read_back;
int                 retry;
retry = 0;
while (TRUE) {
AscSetChipEEPCmd(iop_base, cmd_reg);
DvcSleepMilliSecond(1);
read_back = AscGetChipEEPCmd(iop_base);
if (read_back == cmd_reg) {
return (1);
}
if (retry++ > ASC_EEP_MAX_RETRY) {
return (0);
}
}
}
ASC_INITFUNC(
STATIC int
AscWriteEEPDataReg(
PortAddr iop_base,
ushort data_reg
)
)
{
ushort              read_back;
int                 retry;
retry = 0;
while (TRUE) {
AscSetChipEEPData(iop_base, data_reg);
DvcSleepMilliSecond(1);
read_back = AscGetChipEEPData(iop_base);
if (read_back == data_reg) {
return (1);
}
if (retry++ > ASC_EEP_MAX_RETRY) {
return (0);
}
}
}
ASC_INITFUNC(
STATIC void
AscWaitEEPRead(
void
)
)
{
DvcSleepMilliSecond(1);
return;
}
ASC_INITFUNC(
STATIC void
AscWaitEEPWrite(
void
)
)
{
DvcSleepMilliSecond(20);
return;
}
ASC_INITFUNC(
STATIC ushort
AscReadEEPWord(
PortAddr iop_base,
uchar addr
)
)
{
ushort              read_wval;
uchar               cmd_reg;
AscWriteEEPCmdReg(iop_base, ASC_EEP_CMD_WRITE_DISABLE);
AscWaitEEPRead();
cmd_reg = addr | ASC_EEP_CMD_READ;
AscWriteEEPCmdReg(iop_base, cmd_reg);
AscWaitEEPRead();
read_wval = AscGetChipEEPData(iop_base);
AscWaitEEPRead();
return (read_wval);
}
ASC_INITFUNC(
STATIC ushort
AscWriteEEPWord(
PortAddr iop_base,
uchar addr,
ushort word_val
)
)
{
ushort              read_wval;
read_wval = AscReadEEPWord(iop_base, addr);
if (read_wval != word_val) {
AscWriteEEPCmdReg(iop_base, ASC_EEP_CMD_WRITE_ABLE);
AscWaitEEPRead();
AscWriteEEPDataReg(iop_base, word_val);
AscWaitEEPRead();
AscWriteEEPCmdReg(iop_base,
(uchar) ((uchar) ASC_EEP_CMD_WRITE | addr));
AscWaitEEPWrite();
AscWriteEEPCmdReg(iop_base, ASC_EEP_CMD_WRITE_DISABLE);
AscWaitEEPRead();
return (AscReadEEPWord(iop_base, addr));
}
return (read_wval);
}
ASC_INITFUNC(
STATIC ushort
AscGetEEPConfig(
PortAddr iop_base,
ASCEEP_CONFIG * cfg_buf, ushort bus_type
)
)
{
ushort              wval;
ushort              sum;
ushort      *wbuf;
int                 cfg_beg;
int                 cfg_end;
int                 s_addr;
int                 isa_pnp_wsize;
wbuf = (ushort *) cfg_buf;
sum = 0;
isa_pnp_wsize = 0;
for (s_addr = 0; s_addr < (2 + isa_pnp_wsize); s_addr++, wbuf++) {
wval = AscReadEEPWord(iop_base, (uchar) s_addr);
sum += wval;
*wbuf = wval;
}
if (bus_type & ASC_IS_VL) {
cfg_beg = ASC_EEP_DVC_CFG_BEG_VL;
cfg_end = ASC_EEP_MAX_DVC_ADDR_VL;
} else {
cfg_beg = ASC_EEP_DVC_CFG_BEG;
cfg_end = ASC_EEP_MAX_DVC_ADDR;
}
for (s_addr = cfg_beg; s_addr <= (cfg_end - 1);
s_addr++, wbuf++) {
wval = AscReadEEPWord(iop_base, (uchar) s_addr);
sum += wval;
*wbuf = wval;
}
*wbuf = AscReadEEPWord(iop_base, (uchar) s_addr);
return (sum);
}
ASC_INITFUNC(
STATIC int
AscSetEEPConfigOnce(
PortAddr iop_base,
ASCEEP_CONFIG * cfg_buf, ushort bus_type
)
)
{
int                 n_error;
ushort      *wbuf;
ushort              sum;
int                 s_addr;
int                 cfg_beg;
int                 cfg_end;
wbuf = (ushort *) cfg_buf;
n_error = 0;
sum = 0;
for (s_addr = 0; s_addr < 2; s_addr++, wbuf++) {
sum += *wbuf;
if (*wbuf != AscWriteEEPWord(iop_base, (uchar) s_addr, *wbuf)) {
n_error++;
}
}
if (bus_type & ASC_IS_VL) {
cfg_beg = ASC_EEP_DVC_CFG_BEG_VL;
cfg_end = ASC_EEP_MAX_DVC_ADDR_VL;
} else {
cfg_beg = ASC_EEP_DVC_CFG_BEG;
cfg_end = ASC_EEP_MAX_DVC_ADDR;
}
for (s_addr = cfg_beg; s_addr <= (cfg_end - 1);
s_addr++, wbuf++) {
sum += *wbuf;
if (*wbuf != AscWriteEEPWord(iop_base, (uchar) s_addr, *wbuf)) {
n_error++;
}
}
*wbuf = sum;
if (sum != AscWriteEEPWord(iop_base, (uchar) s_addr, sum)) {
n_error++;
}
wbuf = (ushort *) cfg_buf;
for (s_addr = 0; s_addr < 2; s_addr++, wbuf++) {
if (*wbuf != AscReadEEPWord(iop_base, (uchar) s_addr)) {
n_error++;
}
}
for (s_addr = cfg_beg; s_addr <= cfg_end;
s_addr++, wbuf++) {
if (*wbuf != AscReadEEPWord(iop_base, (uchar) s_addr)) {
n_error++;
}
}
return (n_error);
}
ASC_INITFUNC(
STATIC int
AscSetEEPConfig(
PortAddr iop_base,
ASCEEP_CONFIG * cfg_buf, ushort bus_type
)
)
{
int            retry;
int            n_error;
retry = 0;
while (TRUE) {
if ((n_error = AscSetEEPConfigOnce(iop_base, cfg_buf,
bus_type)) == 0) {
break;
}
if (++retry > ASC_EEP_MAX_RETRY) {
break;
}
}
return (n_error);
}
STATIC void
AscAsyncFix(
ASC_DVC_VAR asc_ptr_type *asc_dvc,
uchar tid_no,
ASC_SCSI_INQUIRY *inq)
{
uchar                dvc_type;
ASC_SCSI_BIT_ID_TYPE tid_bits;
dvc_type = inq->byte0.peri_dvc_type;
tid_bits = ASC_TIX_TO_TARGET_ID(tid_no);
if (asc_dvc->bug_fix_cntl & ASC_BUG_FIX_ASYN_USE_SYN) {
if (!(asc_dvc->init_sdtr & tid_bits)) {
if ((dvc_type == SCSI_TYPE_CDROM) &&
(AscCompareString((uchar *) inq->vendor_id,
(uchar *) "HP ", 3) == 0)) {
asc_dvc->pci_fix_asyn_xfer_always |= tid_bits;
}
asc_dvc->pci_fix_asyn_xfer |= tid_bits;
if ((dvc_type == SCSI_TYPE_PROC) ||
(dvc_type == SCSI_TYPE_SCANNER)) {
asc_dvc->pci_fix_asyn_xfer &= ~tid_bits;
}
if ((dvc_type == SCSI_TYPE_SASD) &&
(AscCompareString((uchar *) inq->vendor_id,
(uchar *) "TANDBERG", 8) == 0) &&
(AscCompareString((uchar *) inq->product_id,
(uchar *) " TDC 36", 7) == 0)) {
asc_dvc->pci_fix_asyn_xfer &= ~tid_bits;
}
if ((dvc_type == SCSI_TYPE_SASD) &&
(AscCompareString((uchar *) inq->vendor_id,
(uchar *) "WANGTEK ", 8) == 0)) {
asc_dvc->pci_fix_asyn_xfer &= ~tid_bits;
}
if ((dvc_type == SCSI_TYPE_CDROM) &&
(AscCompareString((uchar *) inq->vendor_id,
(uchar *) "NEC     ", 8) == 0) &&
(AscCompareString((uchar *) inq->product_id,
(uchar *) "CD-ROM DRIVE    ", 16) == 0)) {
asc_dvc->pci_fix_asyn_xfer &= ~tid_bits;
}
if ((dvc_type == SCSI_TYPE_CDROM) &&
(AscCompareString((uchar *) inq->vendor_id,
(uchar *) "YAMAHA", 6) == 0) &&
(AscCompareString((uchar *) inq->product_id,
(uchar *) "CDR400", 6) == 0)) {
asc_dvc->pci_fix_asyn_xfer &= ~tid_bits;
}
if (asc_dvc->pci_fix_asyn_xfer & tid_bits) {
AscSetRunChipSynRegAtID(asc_dvc->iop_base, tid_no,
ASYN_SDTR_DATA_FIX_PCI_REV_AB);
}
}
}
return;
}
STATIC int
AscTagQueuingSafe(ASC_SCSI_INQUIRY *inq)
{
if ((inq->add_len >= 32) &&
(AscCompareString((uchar *) inq->vendor_id,
(uchar *) "QUANTUM XP34301", 15) == 0) &&
(AscCompareString((uchar *) inq->product_rev_level,
(uchar *) "1071", 4) == 0))
{
return 0;
}
return 1;
}
STATIC void
AscInquiryHandling(ASC_DVC_VAR asc_ptr_type *asc_dvc,
uchar tid_no, ASC_SCSI_INQUIRY *inq)
{
ASC_SCSI_BIT_ID_TYPE tid_bit = ASC_TIX_TO_TARGET_ID(tid_no);
ASC_SCSI_BIT_ID_TYPE orig_init_sdtr, orig_use_tagged_qng;
orig_init_sdtr = asc_dvc->init_sdtr;
orig_use_tagged_qng = asc_dvc->use_tagged_qng;
asc_dvc->init_sdtr &= ~tid_bit;
asc_dvc->cfg->can_tagged_qng &= ~tid_bit;
asc_dvc->use_tagged_qng &= ~tid_bit;
if (inq->byte3.rsp_data_fmt >= 2 || inq->byte2.ansi_apr_ver >= 2) {
if ((asc_dvc->cfg->sdtr_enable & tid_bit) && inq->byte7.Sync) {
asc_dvc->init_sdtr |= tid_bit;
}
if ((asc_dvc->cfg->cmd_qng_enabled & tid_bit) && inq->byte7.CmdQue) {
if (AscTagQueuingSafe(inq)) {
asc_dvc->use_tagged_qng |= tid_bit;
asc_dvc->cfg->can_tagged_qng |= tid_bit;
}
}
}
if (orig_use_tagged_qng != asc_dvc->use_tagged_qng) {
AscWriteLramByte(asc_dvc->iop_base, ASCV_DISC_ENABLE_B,
asc_dvc->cfg->disc_enable);
AscWriteLramByte(asc_dvc->iop_base, ASCV_USE_TAGGED_QNG_B,
asc_dvc->use_tagged_qng);
AscWriteLramByte(asc_dvc->iop_base, ASCV_CAN_TAGGED_QNG_B,
asc_dvc->cfg->can_tagged_qng);
asc_dvc->max_dvc_qng[tid_no] =
asc_dvc->cfg->max_tag_qng[tid_no];
AscWriteLramByte(asc_dvc->iop_base,
(ushort) (ASCV_MAX_DVC_QNG_BEG + tid_no),
asc_dvc->max_dvc_qng[tid_no]);
}
if (orig_init_sdtr != asc_dvc->init_sdtr) {
AscAsyncFix(asc_dvc, tid_no, inq);
}
return;
}
STATIC int
AscCompareString(
ruchar * str1,
ruchar * str2,
int len
)
{
int                 i;
int                 diff;
for (i = 0; i < len; i++) {
diff = (int) (str1[i] - str2[i]);
if (diff != 0)
return (diff);
}
return (0);
}
STATIC uchar
AscReadLramByte(
PortAddr iop_base,
ushort addr
)
{
uchar               byte_data;
ushort              word_data;
if (isodd_word(addr)) {
AscSetChipLramAddr(iop_base, addr - 1);
word_data = AscGetChipLramData(iop_base);
byte_data = (uchar) ((word_data >> 8) & 0xFF);
} else {
AscSetChipLramAddr(iop_base, addr);
word_data = AscGetChipLramData(iop_base);
byte_data = (uchar) (word_data & 0xFF);
}
return (byte_data);
}
STATIC ushort
AscReadLramWord(
PortAddr iop_base,
ushort addr
)
{
ushort              word_data;
AscSetChipLramAddr(iop_base, addr);
word_data = AscGetChipLramData(iop_base);
return (word_data);
}
STATIC ulong
AscReadLramDWord(
PortAddr iop_base,
ushort addr
)
{
ushort              val_low, val_high;
ulong               dword_data;
AscSetChipLramAddr(iop_base, addr);
val_low = AscGetChipLramData(iop_base);
val_high = AscGetChipLramData(iop_base);
dword_data = ((ulong) val_high << 16) | (ulong) val_low;
return (dword_data);
}
STATIC void
AscWriteLramWord(
PortAddr iop_base,
ushort addr,
ushort word_val
)
{
AscSetChipLramAddr(iop_base, addr);
AscSetChipLramData(iop_base, word_val);
return;
}
STATIC void
AscWriteLramDWord(
PortAddr iop_base,
ushort addr,
ulong dword_val
)
{
ushort              word_val;
AscSetChipLramAddr(iop_base, addr);
word_val = (ushort) dword_val;
AscSetChipLramData(iop_base, word_val);
word_val = (ushort) (dword_val >> 16);
AscSetChipLramData(iop_base, word_val);
return;
}
STATIC void
AscWriteLramByte(
PortAddr iop_base,
ushort addr,
uchar byte_val
)
{
ushort              word_data;
if (isodd_word(addr)) {
addr--;
word_data = AscReadLramWord(iop_base, addr);
word_data &= 0x00FF;
word_data |= (((ushort) byte_val << 8) & 0xFF00);
} else {
word_data = AscReadLramWord(iop_base, addr);
word_data &= 0xFF00;
word_data |= ((ushort) byte_val & 0x00FF);
}
AscWriteLramWord(iop_base, addr, word_data);
return;
}
STATIC void
AscMemWordCopyToLram(
PortAddr iop_base,
ushort s_addr,
ushort * s_buffer,
int words
)
{
AscSetChipLramAddr(iop_base, s_addr);
DvcOutPortWords(iop_base + IOP_RAM_DATA, s_buffer, words);
return;
}
STATIC void
AscMemDWordCopyToLram(
PortAddr iop_base,
ushort s_addr,
ulong * s_buffer,
int dwords
)
{
AscSetChipLramAddr(iop_base, s_addr);
DvcOutPortDWords(iop_base + IOP_RAM_DATA, s_buffer, dwords);
return;
}
STATIC void
AscMemWordCopyFromLram(
PortAddr iop_base,
ushort s_addr,
ushort * d_buffer,
int words
)
{
AscSetChipLramAddr(iop_base, s_addr);
DvcInPortWords(iop_base + IOP_RAM_DATA, d_buffer, words);
return;
}
STATIC ulong
AscMemSumLramWord(
PortAddr iop_base,
ushort s_addr,
rint words
)
{
ulong               sum;
int                 i;
sum = 0L;
for (i = 0; i < words; i++, s_addr += 2) {
sum += AscReadLramWord(iop_base, s_addr);
}
return (sum);
}
STATIC void
AscMemWordSetLram(
PortAddr iop_base,
ushort s_addr,
ushort set_wval,
rint words
)
{
rint             i;
AscSetChipLramAddr(iop_base, s_addr);
for (i = 0; i < words; i++) {
AscSetChipLramData(iop_base, set_wval);
}
return;
}
STATIC unsigned char _adv_mcode_buf[] ASC_INITDATA = {
0x9C,  0xF0,  0x80,  0x01,  0x00,  0xF0,  0x44,  0x0A,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x72,  0x01,  0xD6,  0x11,  0x00,  0x00,  0x70,  0x01,
0x30,  0x01,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x90,  0x10,  0x2D,  0x03,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x48,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,  0x01,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x78,  0x56,  0x34,  0x12,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x04,  0xF7,  0x70,  0x01,  0x0C,  0x1C,  0x06,  0xF7,  0x02,  0x00,  0x00,  0xF2,  0xD6,  0x0A,
0x04,  0xF7,  0x70,  0x01,  0x06,  0xF7,  0x02,  0x00,  0x3E,  0x57,  0x3C,  0x56,  0x0C,  0x1C,  0x00,  0xFC,
0xA6,  0x00,  0x01,  0x58,  0xAA,  0x13,  0x20,  0xF0,  0xA6,  0x03,  0x06,  0xEC,  0xB9,  0x00,  0x0E,  0x47,
0x03,  0xE6,  0x10,  0x00,  0xCE,  0x45,  0x02,  0x13,  0x3E,  0x57,  0x06,  0xEA,  0xB9,  0x00,  0x47,  0x4B,
0x03,  0xF6,  0xE0,  0x00,  0x00,  0xF2,  0x68,  0x0A,  0x01,  0x48,  0x4E,  0x12,  0x03,  0xF6,  0xC0,  0x00,
0x00,  0xF2,  0x68,  0x0A,  0x41,  0x58,  0x03,  0xF6,  0xD0,  0x00,  0x00,  0xF2,  0x68,  0x0A,  0x49,  0x44,
0x59,  0xF0,  0x0A,  0x02,  0x03,  0xF6,  0xE0,  0x00,  0x00,  0xF2,  0x68,  0x0A,  0x44,  0x58,  0x00,  0xF2,
0xE2,  0x0D,  0x02,  0xCC,  0x4A,  0xE4,  0x01,  0x00,  0x55,  0xF0,  0x08,  0x03,  0x45,  0xF4,  0x02,  0x00,
0x83,  0x5A,  0x04,  0xCC,  0x01,  0x4A,  0x12,  0x12,  0x00,  0xF2,  0xE2,  0x0D,  0x00,  0xCD,  0x48,  0xE4,
0x01,  0x00,  0xE9,  0x13,  0x00,  0xF2,  0xC6,  0x0F,  0xFA,  0x10,  0x0E,  0x47,  0x03,  0xE6,  0x10,  0x00,
0xCE,  0x45,  0x02,  0x13,  0x3E,  0x57,  0xCE,  0x47,  0x97,  0x13,  0x04,  0xEC,  0xB4,  0x00,  0x00,  0xF2,
0xE2,  0x0D,  0x00,  0xCD,  0x48,  0xE4,  0x00,  0x00,  0x12,  0x12,  0x3E,  0x57,  0x06,  0xCC,  0x45,  0xF4,
0x02,  0x00,  0x83,  0x5A,  0x00,  0xCC,  0x00,  0xEA,  0xB4,  0x00,  0x92,  0x10,  0x00,  0xF0,  0x8C,  0x01,
0x43,  0xF0,  0x5C,  0x02,  0x44,  0xF0,  0x60,  0x02,  0x45,  0xF0,  0x64,  0x02,  0x46,  0xF0,  0x68,  0x02,
0x47,  0xF0,  0x6E,  0x02,  0x48,  0xF0,  0x9E,  0x02,  0xB9,  0x54,  0x62,  0x10,  0x00,  0x1C,  0x5A,  0x10,
0x02,  0x1C,  0x56,  0x10,  0x1E,  0x1C,  0x52,  0x10,  0x00,  0xF2,  0x1E,  0x11,  0x50,  0x10,  0x06,  0xFC,
0xA8,  0x00,  0x03,  0xF6,  0xBE,  0x00,  0x00,  0xF2,  0x4E,  0x0A,  0x8C,  0x10,  0x01,  0xF6,  0x01,  0x00,
0x01,  0xFA,  0xA8,  0x00,  0x00,  0xF2,  0x2C,  0x0B,  0x06,  0x10,  0xB9,  0x54,  0x01,  0xFA,  0xA8,  0x00,
0x03,  0xF6,  0xBE,  0x00,  0x00,  0xF2,  0x58,  0x0A,  0x01,  0xFC,  0xA8,  0x00,  0x20,  0x10,  0x58,  0x1C,
0x00,  0xF2,  0x1C,  0x0B,  0x5A,  0x1C,  0x01,  0xF6,  0x01,  0x00,  0x38,  0x54,  0x00,  0xFA,  0xA6,  0x00,
0x01,  0xFA,  0xA8,  0x00,  0x20,  0x1C,  0x00,  0xF0,  0x72,  0x01,  0x01,  0xF6,  0x01,  0x00,  0x38,  0x54,
0x00,  0xFA,  0xA6,  0x00,  0x01,  0xFA,  0xA8,  0x00,  0x20,  0x1C,  0x00,  0xF0,  0x80,  0x01,  0x03,  0xF6,
0xE0,  0x00,  0x00,  0xF2,  0x68,  0x0A,  0x01,  0x48,  0x0A,  0x13,  0x00,  0xF2,  0x38,  0x10,  0x00,  0xF2,
0x54,  0x0F,  0x24,  0x10,  0x03,  0xF6,  0xC0,  0x00,  0x00,  0xF2,  0x68,  0x0A,  0x02,  0xF6,  0xD0,  0x00,
0x02,  0x57,  0x03,  0x59,  0x01,  0xCC,  0x49,  0x44,  0x5B,  0xF0,  0x04,  0x03,  0x00,  0xF2,  0x9C,  0x0F,
0x00,  0xF0,  0x80,  0x01,  0x00,  0xF2,  0x14,  0x10,  0x0C,  0x1C,  0x02,  0x4B,  0xBF,  0x57,  0x9E,  0x43,
0x77,  0x57,  0x07,  0x4B,  0x20,  0xF0,  0xA6,  0x03,  0x40,  0x1C,  0x1E,  0xF0,  0x30,  0x03,  0x26,  0xF0,
0x2C,  0x03,  0xA0,  0xF0,  0x1A,  0x03,  0x11,  0xF0,  0xA6,  0x03,  0x12,  0x10,  0x9F,  0xF0,  0x3E,  0x03,
0x46,  0x1C,  0x82,  0xE7,  0x05,  0x00,  0x9E,  0xE7,  0x11,  0x00,  0x00,  0xF0,  0x06,  0x0A,  0x0C,  0x1C,
0x48,  0x1C,  0x46,  0x1C,  0x38,  0x54,  0x00,  0xEC,  0xBA,  0x00,  0x08,  0x44,  0x00,  0xEA,  0xBA,  0x00,
0x03,  0xF6,  0xC0,  0x00,  0x00,  0xF2,  0x68,  0x0A,  0x08,  0x44,  0x00,  0x4C,  0x82,  0xE7,  0x02,  0x00,
0x00,  0xF2,  0x12,  0x11,  0x00,  0xF2,  0x12,  0x11,  0x85,  0xF0,  0x70,  0x03,  0x00,  0xF2,  0x60,  0x0B,
0x06,  0xF0,  0x80,  0x03,  0x09,  0xF0,  0x24,  0x09,  0x1E,  0xF0,  0xFC,  0x09,  0x00,  0xF0,  0x02,  0x0A,
0x00,  0xFC,  0xBE,  0x00,  0x98,  0x57,  0x55,  0xF0,  0xAC,  0x04,  0x01,  0xE6,  0x0C,  0x00,  0x00,  0xF2,
0x4E,  0x0D,  0x00,  0xF2,  0x12,  0x11,  0x00,  0xF2,  0xBC,  0x11,  0x00,  0xF2,  0xC8,  0x11,  0x01,  0xF0,
0x7C,  0x02,  0x00,  0xF0,  0x8A,  0x02,  0x46,  0x1C,  0x0C,  0x1C,  0x67,  0x1B,  0xBF,  0x57,  0x77,  0x57,
0x02,  0x4B,  0x48,  0x1C,  0x32,  0x1C,  0x00,  0xF2,  0x92,  0x0D,  0x30,  0x1C,  0x96,  0xF0,  0xBC,  0x03,
0xB1,  0xF0,  0xC0,  0x03,  0x1E,  0xF0,  0xFC,  0x09,  0x85,  0xF0,  0x02,  0x0A,  0x00,  0xFC,  0xBE,  0x00,
0x98,  0x57,  0x14,  0x12,  0x01,  0xE6,  0x0C,  0x00,  0x00,  0xF2,  0x4E,  0x0D,  0x00,  0xF2,  0x12,  0x11,
0x01,  0xF0,  0x7C,  0x02,  0x00,  0xF0,  0x8A,  0x02,  0x03,  0xF6,  0xE0,  0x00,  0x00,  0xF2,  0x68,  0x0A,
0x01,  0x48,  0x55,  0xF0,  0x98,  0x04,  0x03,  0x82,  0x03,  0xFC,  0xA0,  0x00,  0x9B,  0x57,  0x40,  0x12,
0x69,  0x18,  0x00,  0xF2,  0x12,  0x11,  0x85,  0xF0,  0x42,  0x04,  0x69,  0x08,  0x00,  0xF2,  0x12,  0x11,
0x85,  0xF0,  0x02,  0x0A,  0x68,  0x08,  0x4C,  0x44,  0x28,  0x12,  0x44,  0x48,  0x03,  0xF6,  0xE0,  0x00,
0x00,  0xF2,  0x68,  0x0A,  0x45,  0x58,  0x00,  0xF2,  0xF6,  0x0D,  0x00,  0xCC,  0x01,  0x48,  0x55,  0xF0,
0x98,  0x04,  0x4C,  0x44,  0xEF,  0x13,  0x00,  0xF2,  0xC6,  0x0F,  0x00,  0xF2,  0x14,  0x10,  0x08,  0x10,
0x68,  0x18,  0x45,  0x5A,  0x00,  0xF2,  0xF6,  0x0D,  0x04,  0x80,  0x18,  0xE4,  0x10,  0x00,  0x28,  0x12,
0x01,  0xE6,  0x06,  0x00,  0x04,  0x80,  0x18,  0xE4,  0x01,  0x00,  0x04,  0x12,  0x01,  0xE6,  0x0D,  0x00,
0x00,  0xF2,  0x4E,  0x0D,  0x00,  0xF2,  0x12,  0x11,  0x04,  0xE6,  0x02,  0x00,  0x9E,  0xE7,  0x15,  0x00,
0x01,  0xF0,  0x1C,  0x0A,  0x00,  0xF0,  0x02,  0x0A,  0x69,  0x08,  0x05,  0x80,  0x48,  0xE4,  0x00,  0x00,
0x0C,  0x12,  0x00,  0xE6,  0x11,  0x00,  0x00,  0xEA,  0xB8,  0x00,  0x00,  0xF2,  0xB6,  0x10,  0x82,  0xE7,
0x02,  0x00,  0x1C,  0x90,  0x40,  0x5C,  0x00,  0x16,  0x01,  0xE6,  0x06,  0x00,  0x00,  0xF2,  0x4E,  0x0D,
0x01,  0xF0,  0x80,  0x01,  0x1E,  0xF0,  0x80,  0x01,  0x00,  0xF0,  0xA0,  0x04,  0x42,  0x5B,  0x06,  0xF7,
0x03,  0x00,  0x46,  0x59,  0xBF,  0x57,  0x77,  0x57,  0x01,  0xE6,  0x80,  0x00,  0x07,  0x80,  0x31,  0x44,
0x04,  0x80,  0x18,  0xE4,  0x20,  0x00,  0x56,  0x13,  0x20,  0x80,  0x48,  0xE4,  0x03,  0x00,  0x4E,  0x12,
0x00,  0xFC,  0xA2,  0x00,  0x98,  0x57,  0x55,  0xF0,  0x1C,  0x05,  0x31,  0xE4,  0x40,  0x00,  0x00,  0xFC,
0xA0,  0x00,  0x98,  0x57,  0x36,  0x12,  0x4C,  0x1C,  0x00,  0xF2,  0x12,  0x11,  0x89,  0x48,  0x00,  0xF2,
0x12,  0x11,  0x86,  0xF0,  0x2E,  0x05,  0x82,  0xE7,  0x06,  0x00,  0x1B,  0x80,  0x48,  0xE4,  0x22,  0x00,
0x5B,  0xF0,  0x0C,  0x05,  0x48,  0xE4,  0x20,  0x00,  0x59,  0xF0,  0x10,  0x05,  0x00,  0xE6,  0x20,  0x00,
0x09,  0x48,  0x00,  0xF2,  0x12,  0x11,  0x86,  0xF0,  0x2E,  0x05,  0x83,  0x80,  0x04,  0x10,  0x00,  0xF2,
0xA2,  0x0D,  0x00,  0xE6,  0x01,  0x00,  0x00,  0xEA,  0x26,  0x01,  0x01,  0xEA,  0x27,  0x01,  0x04,  0x80,
0x18,  0xE4,  0x10,  0x00,  0x36,  0x12,  0xB9,  0x54,  0x00,  0xF2,  0xF6,  0x0E,  0x01,  0xE6,  0x06,  0x00,
0x04,  0x80,  0x18,  0xE4,  0x01,  0x00,  0x04,  0x12,  0x01,  0xE6,  0x0D,  0x00,  0x00,  0xF2,  0x4E,  0x0D,
0x00,  0xF2,  0x12,  0x11,  0x00,  0xF2,  0xBC,  0x11,  0x00,  0xF2,  0xC8,  0x11,  0x04,  0xE6,  0x02,  0x00,
0x9E,  0xE7,  0x15,  0x00,  0x01,  0xF0,  0x1C,  0x0A,  0x00,  0xF0,  0x02,  0x0A,  0x00,  0xFC,  0x20,  0x01,
0x98,  0x57,  0x34,  0x12,  0x00,  0xFC,  0x24,  0x01,  0x98,  0x57,  0x2C,  0x13,  0xB9,  0x54,  0x00,  0xF2,
0xF6,  0x0E,  0x86,  0xF0,  0xA8,  0x05,  0x03,  0xF6,  0x01,  0x00,  0x00,  0xF2,  0x8C,  0x0E,  0x85,  0xF0,
0x9E,  0x05,  0x82,  0xE7,  0x03,  0x00,  0x00,  0xF2,  0x60,  0x0B,  0x82,  0xE7,  0x02,  0x00,  0x00,  0xFC,
0x24,  0x01,  0xB0,  0x57,  0x00,  0xFA,  0x24,  0x01,  0x00,  0xFC,  0x9E,  0x00,  0x98,  0x57,  0x5A,  0x12,
0x00,  0xFC,  0xB6,  0x00,  0x98,  0x57,  0x52,  0x13,  0x03,  0xE6,  0x0C,  0x00,  0x00,  0xFC,  0x9C,  0x00,
0x98,  0x57,  0x04,  0x13,  0x03,  0xE6,  0x19,  0x00,  0x05,  0xE6,  0x08,  0x00,  0x00,  0xF6,  0x00,  0x01,
0x00,  0x57,  0x00,  0x57,  0x03,  0x58,  0x00,  0xDC,  0x18,  0xF4,  0x00,  0x80,  0x04,  0x13,  0x05,  0xE6,
0x0F,  0x00,  0xB9,  0x54,  0x00,  0xF2,  0xF6,  0x0E,  0x86,  0xF0,  0x0A,  0x06,  0x00,  0xF2,  0xBA,  0x0E,
0x85,  0xF0,  0x00,  0x06,  0x82,  0xE7,  0x03,  0x00,  0x00,  0xF2,  0x60,  0x0B,  0x82,  0xE7,  0x02,  0x00,
0x00,  0xFC,  0xB6,  0x00,  0xB0,  0x57,  0x00,  0xFA,  0xB6,  0x00,  0x01,  0xF6,  0x01,  0x00,  0x00,  0xF2,
0xF6,  0x0E,  0x9C,  0x32,  0x4E,  0x1C,  0x32,  0x1C,  0x00,  0xF2,  0x92,  0x0D,  0x30,  0x1C,  0x82,  0xE7,
0x04,  0x00,  0xB1,  0xF0,  0x22,  0x06,  0x0A,  0xF0,  0x3E,  0x06,  0x05,  0xF0,  0xD6,  0x06,  0x06,  0xF0,
0xDC,  0x06,  0x09,  0xF0,  0x24,  0x09,  0x1E,  0xF0,  0xFC,  0x09,  0x00,  0xF0,  0x02,  0x0A,  0x04,  0x80,
0x18,  0xE4,  0x20,  0x00,  0x30,  0x12,  0x09,  0xE7,  0x03,  0x00,  0x00,  0xF2,  0x12,  0x11,  0x21,  0x80,
0x18,  0xE4,  0xE0,  0x00,  0x09,  0x48,  0x00,  0xF2,  0x12,  0x11,  0x09,  0xE7,  0x00,  0x00,  0x00,  0xF2,
0x12,  0x11,  0x09,  0xE7,  0x00,  0x00,  0x00,  0xF2,  0x12,  0x11,  0x99,  0xA4,  0x00,  0xF2,  0x12,  0x11,
0x09,  0xE7,  0x00,  0x00,  0x9A,  0x10,  0x04,  0x80,  0x18,  0xE4,  0x02,  0x00,  0x34,  0x12,  0x09,  0xE7,
0x1B,  0x00,  0x00,  0xF2,  0x12,  0x11,  0x21,  0x80,  0x18,  0xE4,  0xE0,  0x00,  0x09,  0x48,  0x00,  0xF2,
0x12,  0x11,  0x09,  0xE7,  0x00,  0x00,  0x00,  0xF2,  0x12,  0x11,  0x09,  0xE7,  0x00,  0x00,  0x00,  0xF2,
0x12,  0x11,  0x09,  0xE7,  0x01,  0x00,  0x00,  0xF2,  0x12,  0x11,  0x09,  0xE7,  0x00,  0x00,  0x00,  0xF0,
0x0C,  0x09,  0xBB,  0x55,  0x9A,  0x81,  0x03,  0xF7,  0x20,  0x00,  0x09,  0x6F,  0x93,  0x45,  0x55,  0xF0,
0xE2,  0x06,  0xB1,  0xF0,  0xC2,  0x06,  0x0A,  0xF0,  0xBA,  0x06,  0x09,  0xF0,  0x24,  0x09,  0x1E,  0xF0,
0xFC,  0x09,  0x00,  0xF0,  0x02,  0x0A,  0x00,  0xF2,  0x60,  0x0B,  0x47,  0x10,  0x09,  0xE7,  0x08,  0x00,
0x41,  0x10,  0x05,  0x80,  0x48,  0xE4,  0x00,  0x00,  0x1E,  0x12,  0x00,  0xE6,  0x11,  0x00,  0x00,  0xEA,
0xB8,  0x00,  0x00,  0xF2,  0xB6,  0x10,  0x2C,  0x90,  0xAE,  0x90,  0x08,  0x50,  0x8A,  0x50,  0x38,  0x54,
0x1F,  0x40,  0x00,  0xF2,  0xB4,  0x0D,  0x08,  0x10,  0x08,  0x90,  0x8A,  0x90,  0x30,  0x50,  0xB2,  0x50,
0x9C,  0x32,  0x0C,  0x92,  0x8E,  0x92,  0x38,  0x54,  0x04,  0x80,  0x30,  0xE4,  0x08,  0x00,  0x04,  0x40,
0x0C,  0x1C,  0x00,  0xF6,  0x03,  0x00,  0xB1,  0xF0,  0x26,  0x07,  0x9E,  0xF0,  0x3A,  0x07,  0x01,  0x48,
0x55,  0xF0,  0xFC,  0x09,  0x0C,  0x1C,  0x10,  0x44,  0xED,  0x10,  0x0B,  0xF0,  0x5E,  0x07,  0x0C,  0xF0,
0x62,  0x07,  0x05,  0xF0,  0x52,  0x07,  0x06,  0xF0,  0x58,  0x07,  0x09,  0xF0,  0x24,  0x09,  0x00,  0xF0,
0x02,  0x0A,  0x00,  0xF2,  0x60,  0x0B,  0xCF,  0x10,  0x09,  0xE7,  0x08,  0x00,  0xC9,  0x10,  0x2E,  0x1C,
0x02,  0x10,  0x2C,  0x1C,  0xAA,  0xF0,  0x64,  0x07,  0xAC,  0xF0,  0x72,  0x07,  0x40,  0x10,  0x34,  0x1C,
0xF3,  0x10,  0xAD,  0xF0,  0x7C,  0x07,  0xC8,  0x10,  0x36,  0x1C,  0xE9,  0x10,  0x2B,  0xF0,  0x82,  0x08,
0x6B,  0x18,  0x18,  0xF4,  0x00,  0xFE,  0x20,  0x12,  0x01,  0x58,  0xD2,  0xF0,  0x82,  0x08,  0x76,  0x18,
0x18,  0xF4,  0x03,  0x00,  0xEC,  0x12,  0x00,  0xFC,  0x22,  0x01,  0x18,  0xF4,  0x01,  0x00,  0xE2,  0x12,
0x0B,  0xF0,  0x64,  0x07,  0x0C,  0xF0,  0x64,  0x07,  0x36,  0x1C,  0x34,  0x1C,  0xB7,  0x10,  0x38,  0x54,
0xB9,  0x54,  0x84,  0x80,  0x19,  0xE4,  0x20,  0x00,  0xB2,  0x13,  0x85,  0x80,  0x81,  0x48,  0x66,  0x12,
0x04,  0x80,  0x18,  0xE4,  0x08,  0x00,  0x58,  0x13,  0x1F,  0x80,  0x08,  0x44,  0xC8,  0x44,  0x9F,  0x12,
0x1F,  0x40,  0x34,  0x91,  0xB6,  0x91,  0x44,  0x55,  0xE5,  0x55,  0x02,  0xEC,  0xB8,  0x00,  0x02,  0x49,
0xBB,  0x55,  0x82,  0x81,  0xC0,  0x55,  0x48,  0xF4,  0x0F,  0x00,  0x5A,  0xF0,  0x1A,  0x08,  0x4A,  0xE4,
0x17,  0x00,  0xD5,  0xF0,  0xFA,  0x07,  0x02,  0xF6,  0x0F,  0x00,  0x02,  0xF4,  0x02,  0x00,  0x02,  0xEA,
0xB8,  0x00,  0x04,  0x91,  0x86,  0x91,  0x02,  0x4B,  0x2C,  0x90,  0x08,  0x50,  0x2E,  0x90,  0x0A,  0x50,
0x2C,  0x51,  0xAE,  0x51,  0x00,  0xF2,  0xB6,  0x10,  0x38,  0x54,  0x00,  0xF2,  0xB4,  0x0D,  0x56,  0x10,
0x34,  0x91,  0xB6,  0x91,  0x0C,  0x10,  0x04,  0x80,  0x18,  0xE4,  0x08,  0x00,  0x41,  0x12,  0x0C,  0x91,
0x8E,  0x91,  0x04,  0x80,  0x18,  0xE4,  0xF7,  0x00,  0x04,  0x40,  0x30,  0x90,  0xB2,  0x90,  0x36,  0x10,
0x02,  0x80,  0x48,  0xE4,  0x10,  0x00,  0x31,  0x12,  0x82,  0xE7,  0x10,  0x00,  0x84,  0x80,  0x19,  0xE4,
0x20,  0x00,  0x10,  0x13,  0x0C,  0x90,  0x8E,  0x90,  0x5D,  0xF0,  0x78,  0x07,  0x0C,  0x58,  0x8D,  0x58,
0x00,  0xF0,  0x64,  0x07,  0x38,  0x54,  0xB9,  0x54,  0x19,  0x80,  0xF1,  0x10,  0x3A,  0x55,  0x19,  0x81,
0xBB,  0x55,  0x10,  0x90,  0x92,  0x90,  0x10,  0x58,  0x91,  0x58,  0x14,  0x59,  0x95,  0x59,  0x00,  0xF0,
0x64,  0x07,  0x04,  0x80,  0x18,  0xE4,  0x20,  0x00,  0x06,  0x12,  0x6C,  0x19,  0x19,  0x41,  0x7C,  0x10,
0x6C,  0x19,  0x0C,  0x51,  0xED,  0x19,  0x8E,  0x51,  0x6B,  0x18,  0x18,  0xF4,  0x00,  0xFF,  0x02,  0x13,
0x6A,  0x10,  0x01,  0x58,  0xD2,  0xF0,  0xC0,  0x08,  0x76,  0x18,  0x18,  0xF4,  0x03,  0x00,  0x0A,  0x12,
0x00,  0xFC,  0x22,  0x01,  0x18,  0xF4,  0x01,  0x00,  0x06,  0x13,  0x9E,  0xE7,  0x16,  0x00,  0x4C,  0x10,
0xD1,  0xF0,  0xCA,  0x08,  0x9E,  0xE7,  0x17,  0x00,  0x42,  0x10,  0xD0,  0xF0,  0xD4,  0x08,  0x9E,  0xE7,
0x19,  0x00,  0x38,  0x10,  0xCF,  0xF0,  0xDE,  0x08,  0x9E,  0xE7,  0x20,  0x00,  0x2E,  0x10,  0xCE,  0xF0,
0xE8,  0x08,  0x9E,  0xE7,  0x21,  0x00,  0x24,  0x10,  0xCD,  0xF0,  0xF2,  0x08,  0x9E,  0xE7,  0x22,  0x00,
0x1A,  0x10,  0xCC,  0xF0,  0x04,  0x09,  0x84,  0x80,  0x19,  0xE4,  0x04,  0x00,  0x06,  0x12,  0x9E,  0xE7,
0x12,  0x00,  0x08,  0x10,  0xCB,  0xF0,  0x0C,  0x09,  0x9E,  0xE7,  0x24,  0x00,  0xB1,  0xF0,  0x0C,  0x09,
0x05,  0xF0,  0x1E,  0x09,  0x09,  0xF0,  0x24,  0x09,  0x1E,  0xF0,  0xFC,  0x09,  0xE4,  0x10,  0x00,  0xF2,
0x60,  0x0B,  0xE9,  0x10,  0x9C,  0x32,  0x82,  0xE7,  0x20,  0x00,  0x32,  0x1C,  0xE9,  0x09,  0x00,  0xF2,
0x12,  0x11,  0x85,  0xF0,  0x02,  0x0A,  0x69,  0x08,  0x01,  0xF0,  0x44,  0x09,  0x1E,  0xF0,  0xFC,  0x09,
0x00,  0xF0,  0x38,  0x09,  0x30,  0x44,  0x06,  0x12,  0x9E,  0xE7,  0x42,  0x00,  0xB8,  0x10,  0x04,  0xF6,
0x01,  0x00,  0xB3,  0x45,  0x74,  0x12,  0x04,  0x80,  0x18,  0xE4,  0x20,  0x00,  0x22,  0x13,  0x4B,  0xE4,
0x02,  0x00,  0x36,  0x12,  0x4B,  0xE4,  0x28,  0x00,  0xAC,  0x13,  0x00,  0xF2,  0xBC,  0x11,  0x00,  0xF2,
0xC8,  0x11,  0x03,  0xF6,  0xD0,  0x00,  0xFA,  0x14,  0x82,  0xE7,  0x01,  0x00,  0x00,  0xF0,  0x80,  0x01,
0x9E,  0xE7,  0x44,  0x00,  0x4B,  0xE4,  0x02,  0x00,  0x06,  0x12,  0x03,  0xE6,  0x02,  0x00,  0x76,  0x10,
0x00,  0xF2,  0xA2,  0x0D,  0x03,  0xE6,  0x02,  0x00,  0x6C,  0x10,  0x00,  0xF2,  0xA2,  0x0D,  0x19,  0x82,
0x34,  0x46,  0x0A,  0x13,  0x03,  0xE6,  0x02,  0x00,  0x9E,  0xE7,  0x43,  0x00,  0x68,  0x10,  0x04,  0x80,
0x30,  0xE4,  0x20,  0x00,  0x04,  0x40,  0x00,  0xF2,  0xBC,  0x11,  0x00,  0xF2,  0xC8,  0x11,  0x82,  0xE7,
0x01,  0x00,  0x06,  0xF7,  0x02,  0x00,  0x00,  0xF0,  0x08,  0x03,  0x04,  0x80,  0x18,  0xE4,  0x20,  0x00,
0x06,  0x12,  0x03,  0xE6,  0x02,  0x00,  0x3E,  0x10,  0x04,  0x80,  0x18,  0xE4,  0x02,  0x00,  0x3A,  0x12,
0x04,  0x80,  0x18,  0xE4,  0xFD,  0x00,  0x04,  0x40,  0x1C,  0x1C,  0x9D,  0xF0,  0xEA,  0x09,  0x1C,  0x1C,
0x9D,  0xF0,  0xF0,  0x09,  0xC1,  0x10,  0x9E,  0xE7,  0x13,  0x00,  0x0A,  0x10,  0x9E,  0xE7,  0x41,  0x00,
0x04,  0x10,  0x9E,  0xE7,  0x24,  0x00,  0x00,  0xFC,  0xBE,  0x00,  0x98,  0x57,  0xD5,  0xF0,  0x8A,  0x02,
0x04,  0xE6,  0x04,  0x00,  0x06,  0x10,  0x04,  0xE6,  0x04,  0x00,  0x9D,  0x41,  0x1C,  0x42,  0x9F,  0xE7,
0x00,  0x00,  0x06,  0xF7,  0x02,  0x00,  0x03,  0xF6,  0xE0,  0x00,  0x3C,  0x14,  0x44,  0x58,  0x45,  0x58,
0x00,  0xF2,  0xF6,  0x0D,  0x00,  0xF2,  0x7E,  0x10,  0x00,  0xF2,  0xC6,  0x0F,  0x3C,  0x14,  0x1E,  0x1C,
0x00,  0xF0,  0x80,  0x01,  0x12,  0x1C,  0x22,  0x1C,  0xD2,  0x14,  0x00,  0xF0,  0x72,  0x01,  0x83,  0x59,
0x03,  0xDC,  0x73,  0x57,  0x80,  0x5D,  0x00,  0x16,  0x83,  0x59,  0x03,  0xDC,  0x38,  0x54,  0x70,  0x57,
0x33,  0x54,  0x3B,  0x54,  0x80,  0x5D,  0x00,  0x16,  0x03,  0x57,  0x83,  0x59,  0x38,  0x54,  0x00,  0xCC,
0x00,  0x16,  0x03,  0x57,  0x83,  0x59,  0x00,  0x4C,  0x00,  0x16,  0x02,  0x80,  0x48,  0xE4,  0x01,  0x00,
0x0E,  0x12,  0x48,  0xE4,  0x05,  0x00,  0x08,  0x12,  0x00,  0xF2,  0xBC,  0x11,  0x00,  0xF2,  0xC8,  0x11,
0xC1,  0x5A,  0x3A,  0x55,  0x02,  0xEC,  0xB5,  0x00,  0x45,  0x59,  0x00,  0xF2,  0xF6,  0x0D,  0x83,  0x58,
0x30,  0xE7,  0x00,  0x00,  0x10,  0x4D,  0x30,  0xE7,  0x40,  0x00,  0x10,  0x4F,  0x38,  0x90,  0xBA,  0x90,
0x10,  0x5C,  0x80,  0x5C,  0x83,  0x5A,  0x10,  0x4E,  0x04,  0xEA,  0xB5,  0x00,  0x43,  0x5B,  0x03,  0xF4,
0xE0,  0x00,  0x83,  0x59,  0x04,  0xCC,  0x01,  0x4A,  0x0A,  0x12,  0x45,  0x5A,  0x00,  0xF2,  0xF6,  0x0D,
0x00,  0xF2,  0x38,  0x10,  0x00,  0x16,  0x08,  0x1C,  0x00,  0xFC,  0xAC,  0x00,  0x06,  0x58,  0x67,  0x18,
0x18,  0xF4,  0x8F,  0xE1,  0x01,  0xFC,  0xAE,  0x00,  0x19,  0xF4,  0x70,  0x1E,  0xB0,  0x54,  0x07,  0x58,
0x00,  0xFC,  0xB0,  0x00,  0x08,  0x58,  0x00,  0xFC,  0xB2,  0x00,  0x09,  0x58,  0x0A,  0x1C,  0x00,  0xE6,
0x0F,  0x00,  0x00,  0xEA,  0xB9,  0x00,  0x38,  0x54,  0x00,  0xFA,  0x24,  0x01,  0x00,  0xFA,  0xB6,  0x00,
0x18,  0x1C,  0x14,  0x1C,  0x10,  0x1C,  0x32,  0x1C,  0x12,  0x1C,  0x00,  0x16,  0x3E,  0x57,  0x0C,  0x14,
0x0E,  0x47,  0x07,  0xE6,  0x10,  0x00,  0xCE,  0x47,  0xF5,  0x13,  0x00,  0x16,  0x00,  0xF2,  0xA2,  0x0D,
0x02,  0x4B,  0x03,  0xF6,  0xE0,  0x00,  0x00,  0xF2,  0x68,  0x0A,  0x01,  0x48,  0x20,  0x12,  0x44,  0x58,
0x45,  0x58,  0x9E,  0xE7,  0x15,  0x00,  0x9C,  0xE7,  0x04,  0x00,  0x00,  0xF2,  0xF6,  0x0D,  0x00,  0xF2,
0x7E,  0x10,  0x00,  0xF2,  0xC6,  0x0F,  0x00,  0xF2,  0x7A,  0x0A,  0x1E,  0x1C,  0xD5,  0x10,  0x00,  0x16,
0x69,  0x08,  0x48,  0xE4,  0x04,  0x00,  0x64,  0x12,  0x48,  0xE4,  0x02,  0x00,  0x20,  0x12,  0x48,  0xE4,
0x03,  0x00,  0x1A,  0x12,  0x48,  0xE4,  0x08,  0x00,  0x14,  0x12,  0x48,  0xE4,  0x01,  0x00,  0xF0,  0x12,
0x48,  0xE4,  0x07,  0x00,  0x12,  0x12,  0x01,  0xE6,  0x07,  0x00,  0x00,  0xF2,  0x4E,  0x0D,  0x00,  0xF2,
0x12,  0x11,  0x05,  0xF0,  0x60,  0x0B,  0x00,  0x16,  0x00,  0xE6,  0x01,  0x00,  0x00,  0xEA,  0x99,  0x00,
0x02,  0x80,  0x48,  0xE4,  0x03,  0x00,  0xE7,  0x12,  0x48,  0xE4,  0x06,  0x00,  0xE1,  0x12,  0x01,  0xE6,
0x06,  0x00,  0x00,  0xF2,  0x4E,  0x0D,  0x00,  0xF2,  0x12,  0x11,  0x04,  0xE6,  0x02,  0x00,  0x9E,  0xE7,
0x15,  0x00,  0x01,  0xF0,  0x1C,  0x0A,  0x00,  0xF0,  0x02,  0x0A,  0x00,  0x16,  0x02,  0x80,  0x48,  0xE4,
0x10,  0x00,  0x1C,  0x12,  0x82,  0xE7,  0x08,  0x00,  0x3C,  0x56,  0x03,  0x82,  0x00,  0xF2,  0xE2,  0x0D,
0x30,  0xE7,  0x08,  0x00,  0x04,  0xF7,  0x70,  0x01,  0x06,  0xF7,  0x02,  0x00,  0x00,  0xF0,  0x80,  0x01,
0x6C,  0x19,  0xED,  0x19,  0x5D,  0xF0,  0xD4,  0x0B,  0x44,  0x55,  0xE5,  0x55,  0x59,  0xF0,  0x52,  0x0C,
0x04,  0x55,  0xA5,  0x55,  0x1F,  0x80,  0x01,  0xEC,  0xB8,  0x00,  0x82,  0x48,  0x82,  0x80,  0x49,  0x44,
0x2E,  0x13,  0x01,  0xEC,  0xB8,  0x00,  0x41,  0xE4,  0x02,  0x00,  0x01,  0xEA,  0xB8,  0x00,  0x49,  0xE4,
0x11,  0x00,  0x59,  0xF0,  0x2E,  0x0C,  0x01,  0xE6,  0x17,  0x00,  0x01,  0xEA,  0xB8,  0x00,  0x02,  0x4B,
0x88,  0x90,  0xAC,  0x50,  0x8A,  0x90,  0xAE,  0x50,  0x01,  0xEC,  0xB8,  0x00,  0x82,  0x48,  0x82,  0x80,
0x10,  0x44,  0x02,  0x4B,  0x1F,  0x40,  0xC0,  0x44,  0x00,  0xF2,  0xB4,  0x0D,  0x04,  0x55,  0xA5,  0x55,
0x9F,  0x10,  0x0C,  0x51,  0x8E,  0x51,  0x30,  0x90,  0xB2,  0x90,  0x00,  0x56,  0xA1,  0x56,  0x30,  0x50,
0xB2,  0x50,  0x34,  0x90,  0xB6,  0x90,  0x40,  0x56,  0xE1,  0x56,  0x34,  0x50,  0xB6,  0x50,  0x65,  0x10,
0xB1,  0xF0,  0x70,  0x0C,  0x85,  0xF0,  0xCA,  0x0B,  0xE9,  0x09,  0x4B,  0xE4,  0x03,  0x00,  0x78,  0x12,
0x4B,  0xE4,  0x02,  0x00,  0x01,  0x13,  0xB1,  0xF0,  0x86,  0x0C,  0x85,  0xF0,  0xCA,  0x0B,  0x69,  0x08,
0x48,  0xE4,  0x03,  0x00,  0xD5,  0xF0,  0x86,  0x0B,  0x00,  0xF2,  0x12,  0x11,  0x85,  0xF0,  0xCA,  0x0B,
0xE8,  0x09,  0x3C,  0x56,  0x00,  0xFC,  0x20,  0x01,  0x98,  0x57,  0x02,  0x13,  0xBB,  0x45,  0x4B,  0xE4,
0x00,  0x00,  0x08,  0x12,  0x03,  0xE6,  0x01,  0x00,  0x04,  0xF6,  0x00,  0x80,  0xA8,  0x14,  0xD2,  0x14,
0x30,  0x1C,  0x02,  0x80,  0x48,  0xE4,  0x03,  0x00,  0x10,  0x13,  0x00,  0xFC,  0xB6,  0x00,  0x98,  0x57,
0x02,  0x13,  0x4C,  0x1C,  0x3E,  0x1C,  0x00,  0xF0,  0x8E,  0x0B,  0x00,  0xFC,  0x24,  0x01,  0xB0,  0x57,
0x00,  0xFA,  0x24,  0x01,  0x4C,  0x1C,  0x3E,  0x1C,  0x00,  0xF2,  0x12,  0x11,  0x86,  0xF0,  0x8E,  0x0B,
0x00,  0xF2,  0x8C,  0x0E,  0x00,  0xF0,  0x8E,  0x0B,  0xB1,  0xF0,  0xF8,  0x0C,  0x85,  0xF0,  0x86,  0x0B,
0x69,  0x08,  0x48,  0xE4,  0x01,  0x00,  0xD5,  0xF0,  0x86,  0x0B,  0xFC,  0x14,  0x42,  0x58,  0x6C,  0x14,
0x80,  0x14,  0x30,  0x1C,  0x4A,  0xF4,  0x02,  0x00,  0x55,  0xF0,  0x86,  0x0B,  0x4A,  0xF4,  0x01,  0x00,
0x0E,  0x12,  0x02,  0x80,  0x48,  0xE4,  0x03,  0x00,  0x06,  0x13,  0x3E,  0x1C,  0x00,  0xF0,  0x8E,  0x0B,
0x00,  0xFC,  0xB6,  0x00,  0xB0,  0x57,  0x00,  0xFA,  0xB6,  0x00,  0x4C,  0x1C,  0x3E,  0x1C,  0x00,  0xF2,
0x12,  0x11,  0x86,  0xF0,  0x8E,  0x0B,  0x00,  0xF2,  0xBA,  0x0E,  0x00,  0xF0,  0x8E,  0x0B,  0x4C,  0x1C,
0xB1,  0xF0,  0x50,  0x0D,  0x85,  0xF0,  0x5C,  0x0D,  0x69,  0x08,  0xF3,  0x10,  0x86,  0xF0,  0x64,  0x0D,
0x4E,  0x1C,  0x89,  0x48,  0x00,  0x16,  0x00,  0xF6,  0x00,  0x01,  0x00,  0x57,  0x00,  0x57,  0x03,  0x58,
0x00,  0xDC,  0x18,  0xF4,  0xFF,  0x7F,  0x30,  0x56,  0x00,  0x5C,  0x00,  0x16,  0x00,  0xF6,  0x00,  0x01,
0x00,  0x57,  0x00,  0x57,  0x03,  0x58,  0x00,  0xDC,  0x18,  0xF4,  0x00,  0x80,  0x30,  0x56,  0x00,  0x5C,
0x00,  0x16,  0x00,  0xF6,  0x00,  0x01,  0x00,  0x57,  0x00,  0x57,  0x03,  0x58,  0x00,  0xDC,  0x0B,  0x58,
0x00,  0x16,  0x03,  0xF6,  0x24,  0x01,  0x00,  0xF2,  0x58,  0x0A,  0x03,  0xF6,  0xB6,  0x00,  0x00,  0xF2,
0x58,  0x0A,  0x00,  0x16,  0x02,  0xEC,  0xB8,  0x00,  0x02,  0x49,  0x18,  0xF4,  0xFF,  0x00,  0x00,  0x54,
0x00,  0x54,  0x00,  0x54,  0x00,  0xF4,  0x08,  0x00,  0xE1,  0x18,  0x80,  0x54,  0x03,  0x58,  0x00,  0xDD,
0x01,  0xDD,  0x02,  0xDD,  0x03,  0xDC,  0x02,  0x4B,  0x30,  0x50,  0xB2,  0x50,  0x34,  0x51,  0xB6,  0x51,
0x00,  0x16,  0x45,  0x5A,  0x1D,  0xF4,  0xFF,  0x00,  0x85,  0x56,  0x85,  0x56,  0x85,  0x56,  0x05,  0xF4,
0x02,  0x12,  0x83,  0x5A,  0x00,  0x16,  0x1D,  0xF4,  0xFF,  0x00,  0x85,  0x56,  0x85,  0x56,  0x85,  0x56,
0x05,  0xF4,  0x00,  0x12,  0x83,  0x5A,  0x00,  0x16,  0x38,  0x54,  0xBB,  0x55,  0x3C,  0x56,  0xBD,  0x56,
0x00,  0xF2,  0x12,  0x11,  0x85,  0xF0,  0x82,  0x0E,  0xE9,  0x09,  0xC1,  0x59,  0x00,  0xF2,  0x12,  0x11,
0x85,  0xF0,  0x82,  0x0E,  0xE8,  0x0A,  0x83,  0x55,  0x83,  0x55,  0x4B,  0xF4,  0x90,  0x01,  0x5C,  0xF0,
0x36,  0x0E,  0xBD,  0x56,  0x40,  0x10,  0x4B,  0xF4,  0x30,  0x00,  0x59,  0xF0,  0x48,  0x0E,  0x01,  0xF6,
0x0C,  0x00,  0x00,  0xF6,  0x01,  0x00,  0x2E,  0x10,  0x02,  0xFC,  0x9C,  0x00,  0x9A,  0x57,  0x14,  0x13,
0x4B,  0xF4,  0x64,  0x00,  0x59,  0xF0,  0x64,  0x0E,  0x03,  0xF6,  0x64,  0x00,  0x01,  0xF6,  0x19,  0x00,
0x00,  0xF6,  0x01,  0x00,  0x43,  0xF4,  0x33,  0x00,  0x56,  0xF0,  0x76,  0x0E,  0x04,  0xF4,  0x00,  0x01,
0x43,  0xF4,  0x19,  0x00,  0xF3,  0x10,  0xB4,  0x56,  0xC3,  0x58,  0x02,  0xFC,  0x9E,  0x00,  0x9A,  0x57,
0x08,  0x13,  0x3C,  0x56,  0x00,  0xF6,  0x02,  0x00,  0x00,  0x16,  0x00,  0x16,  0x09,  0xE7,  0x01,  0x00,
0x00,  0xF2,  0x12,  0x11,  0x86,  0xF0,  0xB8,  0x0E,  0x09,  0xE7,  0x02,  0x00,  0x00,  0xF2,  0x12,  0x11,
0x86,  0xF0,  0xB8,  0x0E,  0x09,  0xE7,  0x03,  0x00,  0x00,  0xF2,  0x12,  0x11,  0x86,  0xF0,  0xB8,  0x0E,
0x4E,  0x1C,  0x89,  0x49,  0x00,  0xF2,  0x12,  0x11,  0x00,  0x16,  0x09,  0xE7,  0x01,  0x00,  0x00,  0xF2,
0x12,  0x11,  0x86,  0xF0,  0xF2,  0x0E,  0x09,  0xE7,  0x03,  0x00,  0x00,  0xF2,  0x12,  0x11,  0x86,  0xF0,
0xF2,  0x0E,  0x09,  0xE7,  0x01,  0x00,  0x00,  0xF2,  0x12,  0x11,  0x86,  0xF0,  0xF2,  0x0E,  0x89,  0x49,
0x00,  0xF2,  0x12,  0x11,  0x86,  0xF0,  0xF2,  0x0E,  0x4E,  0x1C,  0x89,  0x4A,  0x00,  0xF2,  0x12,  0x11,
0x00,  0x16,  0x3C,  0x56,  0x00,  0x16,  0x00,  0xEC,  0x26,  0x01,  0x48,  0xE4,  0x01,  0x00,  0x1E,  0x13,
0x38,  0x44,  0x00,  0xEA,  0x26,  0x01,  0x49,  0xF4,  0x00,  0x00,  0x04,  0x12,  0x4E,  0x1C,  0x02,  0x10,
0x4C,  0x1C,  0x01,  0xEC,  0x27,  0x01,  0x89,  0x48,  0x00,  0xF2,  0x12,  0x11,  0x02,  0x14,  0x00,  0x16,
0x85,  0xF0,  0x52,  0x0F,  0x38,  0x54,  0x00,  0xEA,  0x99,  0x00,  0x00,  0xF2,  0x60,  0x0B,  0x02,  0x80,
0x48,  0xE4,  0x06,  0x00,  0x1C,  0x13,  0x00,  0xEC,  0x99,  0x00,  0x48,  0xE4,  0x01,  0x00,  0x0A,  0x12,
0x04,  0x80,  0x30,  0xE4,  0x01,  0x00,  0x04,  0x40,  0x08,  0x10,  0x04,  0x80,  0x18,  0xE4,  0xFE,  0x00,
0x04,  0x40,  0x00,  0x16,  0x02,  0xF6,  0xE0,  0x00,  0x02,  0x57,  0x03,  0x59,  0x01,  0xCC,  0x81,  0x48,
0x22,  0x12,  0x00,  0x4E,  0x83,  0x5A,  0x90,  0x4C,  0x20,  0xE7,  0x00,  0x00,  0xC3,  0x58,  0x1B,  0xF4,
0xFF,  0x00,  0x83,  0x55,  0x83,  0x55,  0x83,  0x55,  0x03,  0xF4,  0x00,  0x12,  0x8B,  0x55,  0x83,  0x59,
0x00,  0x4E,  0x00,  0x16,  0x00,  0x4E,  0x02,  0xF6,  0xF0,  0x00,  0x02,  0x57,  0x03,  0x59,  0x00,  0x4E,
0x83,  0x5A,  0x30,  0xE7,  0x00,  0x00,  0x20,  0xE7,  0x00,  0x00,  0x00,  0x16,  0x02,  0xF6,  0xF0,  0x00,
0x02,  0x57,  0x03,  0x59,  0x01,  0xCC,  0x00,  0x4E,  0x83,  0x5A,  0x30,  0xE7,  0x00,  0x00,  0x80,  0x4C,
0xC3,  0x58,  0x1B,  0xF4,  0xFF,  0x00,  0x83,  0x55,  0x83,  0x55,  0x83,  0x55,  0x03,  0xF4,  0x00,  0x12,
0x83,  0x59,  0x00,  0x4E,  0x00,  0x16,  0x03,  0xF6,  0xE0,  0x00,  0x03,  0x57,  0x83,  0x59,  0x3A,  0x55,
0x02,  0xCC,  0x45,  0x5A,  0x00,  0xF2,  0xF6,  0x0D,  0xC0,  0x5A,  0x40,  0x5C,  0x38,  0x54,  0x00,  0xCD,
0x01,  0xCC,  0x4A,  0x46,  0x0A,  0x13,  0x83,  0x59,  0x00,  0x4C,  0x01,  0x48,  0x16,  0x13,  0x0C,  0x10,
0xC5,  0x58,  0x00,  0xF2,  0xF6,  0x0D,  0x00,  0x4C,  0x01,  0x48,  0x08,  0x13,  0x05,  0xF6,  0xF0,  0x00,
0x05,  0x57,  0x08,  0x10,  0x45,  0x58,  0x00,  0xF2,  0xF6,  0x0D,  0x8D,  0x56,  0x83,  0x5A,  0x80,  0x4C,
0x05,  0x17,  0x00,  0x16,  0x02,  0x4B,  0x06,  0xF7,  0x04,  0x00,  0x62,  0x0B,  0x03,  0x82,  0x00,  0xF2,
0xE2,  0x0D,  0x02,  0x80,  0x00,  0x4C,  0x45,  0xF4,  0x02,  0x00,  0x52,  0x14,  0x06,  0xF7,  0x02,  0x00,
0x06,  0x14,  0x00,  0xF2,  0x54,  0x0F,  0x00,  0x16,  0x02,  0x4B,  0x01,  0xF6,  0xFF,  0x00,  0x38,  0x1C,
0x05,  0xF4,  0x04,  0x00,  0x83,  0x5A,  0x18,  0xDF,  0x19,  0xDF,  0x1D,  0xF7,  0x3C,  0x00,  0xB8,  0xF0,
0x4E,  0x10,  0x9C,  0x14,  0x01,  0x48,  0x1C,  0x13,  0x0E,  0xF7,  0x3C,  0x00,  0x03,  0xF7,  0x04,  0x00,
0xAF,  0x19,  0x03,  0x42,  0x45,  0xF4,  0x02,  0x00,  0x83,  0x5A,  0x02,  0xCC,  0x02,  0x41,  0x45,  0xF4,
0x02,  0x00,  0x00,  0x16,  0x91,  0x44,  0xD5,  0xF0,  0x3E,  0x10,  0x00,  0xF0,  0x9E,  0x02,  0x01,  0xF6,
0xFF,  0x00,  0x38,  0x1C,  0x05,  0xF4,  0x04,  0x00,  0x83,  0x5A,  0x18,  0xDF,  0x19,  0xDF,  0x0E,  0xF7,
0x3C,  0x00,  0x03,  0xF7,  0x04,  0x00,  0x0F,  0x79,  0x1C,  0xF7,  0x3C,  0x00,  0xB8,  0xF0,  0x9C,  0x10,
0x4E,  0x14,  0x01,  0x48,  0x06,  0x13,  0x45,  0xF4,  0x04,  0x00,  0x00,  0x16,  0x91,  0x44,  0xD5,  0xF0,
0x82,  0x10,  0x00,  0xF0,  0x9E,  0x02,  0x02,  0xF6,  0xFF,  0x00,  0x38,  0x1C,  0x2C,  0xBC,  0xAE,  0xBC,
0xE2,  0x08,  0x00,  0xEC,  0xB8,  0x00,  0x02,  0x48,  0x1D,  0xF7,  0x80,  0x00,  0xB8,  0xF0,  0xCC,  0x10,
0x1E,  0x14,  0x01,  0x48,  0x0E,  0x13,  0x0E,  0xF7,  0x80,  0x00,  0x38,  0x54,  0x03,  0x58,  0xAF,  0x19,
0x82,  0x48,  0x00,  0x16,  0x82,  0x48,  0x12,  0x45,  0xD5,  0xF0,  0xBA,  0x10,  0x00,  0xF0,  0x9E,  0x02,
0x39,  0xF0,  0xF8,  0x10,  0x38,  0x44,  0x00,  0x16,  0x7E,  0x18,  0x18,  0xF4,  0x03,  0x00,  0x04,  0x13,
0x61,  0x18,  0x00,  0x16,  0x38,  0x1C,  0x00,  0xFC,  0x22,  0x01,  0x18,  0xF4,  0x01,  0x00,  0xF1,  0x12,
0xE3,  0x10,  0x30,  0x44,  0x30,  0x44,  0x30,  0x44,  0xB1,  0xF0,  0x18,  0x11,  0x00,  0x16,  0x3E,  0x57,
0x03,  0xF6,  0xE0,  0x00,  0x03,  0x57,  0x83,  0x59,  0x04,  0xCC,  0x01,  0x4A,  0x6A,  0x12,  0x45,  0x5A,
0x00,  0xF2,  0xF6,  0x0D,  0x02,  0x4B,  0x70,  0x14,  0x34,  0x13,  0x02,  0x80,  0x48,  0xE4,  0x08,  0x00,
0x18,  0x12,  0x9C,  0xE7,  0x02,  0x00,  0x9E,  0xE7,  0x15,  0x00,  0x00,  0xF2,  0xC6,  0x0F,  0x00,  0xF2,
0x7A,  0x0A,  0x1E,  0x1C,  0x01,  0xF6,  0x01,  0x00,  0x00,  0x16,  0x30,  0xE4,  0x10,  0x00,  0x04,  0x40,
0x00,  0xF2,  0xE2,  0x0D,  0x20,  0xE7,  0x01,  0x00,  0x01,  0xF6,  0x01,  0x00,  0x00,  0x16,  0x04,  0xDC,
0x01,  0x4A,  0x24,  0x12,  0x45,  0x5A,  0x00,  0xF2,  0xF6,  0x0D,  0x43,  0x5B,  0x06,  0xEC,  0x98,  0x00,
0x00,  0xF2,  0x38,  0x10,  0xC6,  0x59,  0x20,  0x14,  0x0A,  0x13,  0x00,  0xF2,  0xC6,  0x0F,  0x00,  0xF2,
0x14,  0x10,  0xA7,  0x10,  0x83,  0x5A,  0xD7,  0x10,  0x0E,  0x47,  0x07,  0xE6,  0x10,  0x00,  0xCE,  0x47,
0x5A,  0xF0,  0x20,  0x11,  0xB9,  0x54,  0x00,  0x16,  0x14,  0x90,  0x96,  0x90,  0x02,  0xFC,  0xA8,  0x00,
0x03,  0xFC,  0xAA,  0x00,  0x48,  0x55,  0x02,  0x13,  0xC9,  0x55,  0x00,  0x16,  0x00,  0xEC,  0xBA,  0x00,
0x10,  0x44,  0x00,  0xEA,  0xBA,  0x00,  0x00,  0x16,  0x03,  0xF6,  0xC0,  0x00,  0x00,  0xF2,  0x68,  0x0A,
0x10,  0x44,  0x00,  0x4C,  0x00,  0x16
};
unsigned short _adv_mcode_size ASC_INITDATA =
sizeof(_adv_mcode_buf);
unsigned long  _adv_mcode_chksum ASC_INITDATA = 0x03494981UL;
STATIC ADVEEP_CONFIG
Default_EEPROM_Config ASC_INITDATA = {
ADV_EEPROM_BIOS_ENABLE,
0x0000,
0xFFFF,
0xFFFF,
0xFFFF,
0xFFFF,
0xFFFF,
0xFFFF,
0,
7,
0,
3,
0,
0,
0,
0xFFEF,
0xFFFF,
0,
ASC_DEF_MAX_HOST_QNG,
ASC_DEF_MAX_DVC_QNG,
0,
0,
0,
0,
0,
0,
{ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 },
0,
0,
0,
0,
0,
0,
0
};
ASC_INITFUNC(
int
AdvInitGetConfig(ADV_DVC_VAR *asc_dvc)
)
{
ushort      warn_code;
AdvPortAddr iop_base;
uchar       pci_cmd_reg;
int         status;
warn_code = 0;
asc_dvc->err_code = 0;
iop_base = asc_dvc->iop_base;
if (((pci_cmd_reg = DvcAdvReadPCIConfigByte(asc_dvc,
AscPCIConfigCommandRegister))
& AscPCICmdRegBits_BusMastering)
!= AscPCICmdRegBits_BusMastering)
{
pci_cmd_reg |= AscPCICmdRegBits_BusMastering;
DvcAdvWritePCIConfigByte(asc_dvc,
AscPCIConfigCommandRegister, pci_cmd_reg);
if (((DvcAdvReadPCIConfigByte(asc_dvc, AscPCIConfigCommandRegister))
& AscPCICmdRegBits_BusMastering)
!= AscPCICmdRegBits_BusMastering)
{
warn_code |= ASC_WARN_SET_PCI_CONFIG_SPACE;
}
}
if (DvcAdvReadPCIConfigByte(asc_dvc, AscPCIConfigLatencyTimer) < 0x20) {
DvcAdvWritePCIConfigByte(asc_dvc, AscPCIConfigLatencyTimer, 0x20);
if (DvcAdvReadPCIConfigByte(asc_dvc, AscPCIConfigLatencyTimer) < 0x20)
{
warn_code |= ASC_WARN_SET_PCI_CONFIG_SPACE;
}
}
asc_dvc->cfg->control_flag = 0;
if (((DvcAdvReadPCIConfigByte(asc_dvc, AscPCIConfigCommandRegister)
& AscPCICmdRegBits_ParErrRespCtrl)) == 0)
{
asc_dvc->cfg->control_flag |= CONTROL_FLAG_IGNORE_PERR;
}
asc_dvc->cur_host_qng = 0;
asc_dvc->cfg->lib_version = (ADV_LIB_VERSION_MAJOR << 8) |
ADV_LIB_VERSION_MINOR;
asc_dvc->cfg->chip_version =
AdvGetChipVersion(iop_base, asc_dvc->bus_type);
if (AdvFindSignature(iop_base) == 0)
{
asc_dvc->err_code = ASC_IERR_BAD_SIGNATURE;
return ADV_ERROR;
}
else {
AdvResetChip(asc_dvc);
if ((status = AdvInitFromEEP(asc_dvc)) == ADV_ERROR)
{
return ADV_ERROR;
}
warn_code |= status;
if (asc_dvc->bios_ctrl & BIOS_CTRL_RESET_SCSI_BUS)
{
AdvResetSCSIBus(asc_dvc);
}
}
return warn_code;
}
ASC_INITFUNC(
int
AdvInitAsc3550Driver(ADV_DVC_VAR *asc_dvc)
)
{
AdvPortAddr iop_base;
ushort      warn_code;
ulong       sum;
int         begin_addr;
int         end_addr;
int         code_sum;
int         word;
int         rql_addr;
int         i;
ushort      scsi_cfg1;
uchar       biosmem[ASC_MC_BIOSLEN];
if (asc_dvc->err_code != 0)
{
return ADV_ERROR;
}
warn_code = 0;
iop_base = asc_dvc->iop_base;
for (i = 0; i < ASC_MC_BIOSLEN; i++)
{
AdvReadByteLram(iop_base, ASC_MC_BIOSMEM + i, biosmem[i]);
}
AdvWriteWordRegister(iop_base, IOPW_RAM_ADDR, 0);
for (word = 0; word < _adv_mcode_size; word += 2)
{
AdvWriteWordAutoIncLram(iop_base,
*((ushort *) (&_adv_mcode_buf[word])));
}
for (; word < ADV_CONDOR_MEMSIZE; word += 2)
{
AdvWriteWordAutoIncLram(iop_base, 0);
}
sum = 0;
AdvWriteWordRegister(iop_base, IOPW_RAM_ADDR, 0);
for (word = 0; word < _adv_mcode_size; word += 2)
{
sum += AdvReadWordAutoIncLram(iop_base);
}
if (sum != _adv_mcode_chksum)
{
asc_dvc->err_code |= ASC_IERR_MCODE_CHKSUM;
return ADV_ERROR;
}
for (i = 0; i < ASC_MC_BIOSLEN; i++)
{
AdvWriteByteLram(iop_base, ASC_MC_BIOSMEM + i, biosmem[i]);
}
AdvReadWordLram(iop_base, ASC_MC_CODE_BEGIN_ADDR, begin_addr);
AdvReadWordLram(iop_base, ASC_MC_CODE_END_ADDR, end_addr);
code_sum = 0;
for (word = begin_addr; word < end_addr; word += 2)
{
code_sum += *((ushort *) (&_adv_mcode_buf[word]));
}
AdvWriteWordLram(iop_base, ASC_MC_CODE_CHK_SUM, code_sum);
AdvReadWordLram(iop_base, ASC_MC_VERSION_DATE, asc_dvc->cfg->mcode_date);
AdvReadWordLram(iop_base, ASC_MC_VERSION_NUM, asc_dvc->cfg->mcode_version);
AdvWriteWordLram(iop_base, ASC_MC_ADAPTER_SCSI_ID,
asc_dvc->chip_scsi_id);
if (asc_dvc->cfg->control_flag & CONTROL_FLAG_IGNORE_PERR)
{
AdvReadWordLram(iop_base, ASC_MC_CONTROL_FLAG, word);
word |= CONTROL_FLAG_IGNORE_PERR;
AdvWriteWordLram(iop_base, ASC_MC_CONTROL_FLAG, word);
}
AdvWriteWordLram(iop_base, ASC_MC_ULTRA_ABLE, asc_dvc->ultra_able);
AdvWriteWordLram(iop_base, ASC_MC_DISC_ENABLE, asc_dvc->cfg->disc_enable);
AdvWriteWordLram(iop_base, ASC_MC_DEFAULT_SCSI_CFG0,
PARITY_EN | SEL_TMO_LONG | OUR_ID_EN | asc_dvc->chip_scsi_id);
scsi_cfg1 = AdvReadWordRegister(iop_base, IOPW_SCSI_CFG1);
if ((scsi_cfg1 & CABLE_ILLEGAL_A) == 0 ||
(scsi_cfg1 & CABLE_ILLEGAL_B) == 0)
{
asc_dvc->err_code |= ASC_IERR_ILLEGAL_CONNECTION;
return ADV_ERROR;
}
if ((AdvReadWordRegister(iop_base, IOPW_SCSI_CTRL) & 0x3F07) == 0x3F07)
{
asc_dvc->err_code |= ASC_IERR_REVERSED_CABLE;
return ADV_ERROR;
}
if ((scsi_cfg1 & DIFF_MODE) && (scsi_cfg1 & DIFF_SENSE) == 0)
{
asc_dvc->err_code |= ASC_IERR_SINGLE_END_DEVICE;
return ADV_ERROR;
}
if (asc_dvc->cfg->termination == 0)
{
asc_dvc->cfg->termination |= TERM_CTL_SEL;
switch(scsi_cfg1 & CABLE_DETECT)
{
case 0x3: case 0x7: case 0xB: case 0xD: case 0xE: case 0xF:
asc_dvc->cfg->termination |= (TERM_CTL_H | TERM_CTL_L);
break;
case 0x1: case 0x5: case 0x9: case 0xA: case 0xC:
asc_dvc->cfg->termination |= TERM_CTL_H;
break;
case 0x2: case 0x6:
break;
}
}
scsi_cfg1 &= ~TERM_CTL;
scsi_cfg1 |= (TERM_CTL_SEL | (~asc_dvc->cfg->termination & TERM_CTL));
AdvWriteWordLram(iop_base, ASC_MC_DEFAULT_SCSI_CFG1,
FLTR_11_TO_20NS | scsi_cfg1);
AdvWriteWordLram(iop_base, ASC_MC_DEFAULT_SEL_MASK,
ADV_TID_TO_TIDMASK(asc_dvc->chip_scsi_id));
for (i = 1, rql_addr = ASC_MC_RISC_Q_LIST_BASE + ASC_MC_RISC_Q_LIST_SIZE;
i < ASC_MC_RISC_Q_TOTAL_CNT;
i++, rql_addr += ASC_MC_RISC_Q_LIST_SIZE)
{
AdvWriteWordLram(iop_base, rql_addr, ((i + 1) + ((i - 1) << 8)));
AdvWriteByteLram(iop_base, rql_addr + RQL_STATE, ASC_MC_QS_FREE);
}
AdvWriteByteLram(iop_base, ASC_MC_HOST_NEXT_READY, ASC_MC_RISC_Q_FIRST);
AdvWriteByteLram(iop_base, ASC_MC_HOST_NEXT_DONE, ASC_MC_RISC_Q_LAST);
AdvWriteByteLram(iop_base, ASC_MC_RISC_NEXT_READY, ASC_MC_RISC_Q_FIRST);
AdvWriteByteLram(iop_base, ASC_MC_RISC_NEXT_DONE, ASC_MC_RISC_Q_LAST);
AdvWriteWordLram(iop_base, rql_addr, (ASC_MC_NULL_Q + ((i - 1) << 8)));
AdvWriteByteLram(iop_base, rql_addr + RQL_STATE, ASC_MC_QS_FREE);
AdvWriteByteRegister(iop_base, IOPB_INTR_ENABLES,
(ADV_INTR_ENABLE_HOST_INTR | ADV_INTR_ENABLE_GLOBAL_INTR));
AdvReadWordLram(iop_base, ASC_MC_CODE_BEGIN_ADDR, word);
AdvWriteWordRegister(iop_base, IOPW_PC, word);
AdvWriteWordRegister(iop_base, IOPW_RISC_CSR, ADV_RISC_CSR_RUN);
return warn_code;
}
ASC_INITFUNC(
STATIC int
AdvInitFromEEP(ADV_DVC_VAR *asc_dvc)
)
{
AdvPortAddr         iop_base;
ushort              warn_code;
ADVEEP_CONFIG       eep_config;
int                 i;
iop_base = asc_dvc->iop_base;
warn_code = 0;
if (AdvGetEEPConfig(iop_base, &eep_config) != eep_config.check_sum)
{
warn_code |= ASC_WARN_EEPROM_CHKSUM;
for (i = 0; i < sizeof(ADVEEP_CONFIG); i++)
{
*((uchar *) &eep_config + i) =
*((uchar *) &Default_EEPROM_Config + i);
}
eep_config.serial_number_word3 =
AdvReadEEPWord(iop_base, ASC_EEP_DVC_CFG_END - 1);
eep_config.serial_number_word2 =
AdvReadEEPWord(iop_base, ASC_EEP_DVC_CFG_END - 2);
eep_config.serial_number_word1 =
AdvReadEEPWord(iop_base, ASC_EEP_DVC_CFG_END - 3);
AdvSetEEPConfig(iop_base, &eep_config);
}
asc_dvc->wdtr_able = eep_config.wdtr_able;
asc_dvc->sdtr_able = eep_config.sdtr_able;
asc_dvc->ultra_able = eep_config.ultra_able;
asc_dvc->tagqng_able = eep_config.tagqng_able;
asc_dvc->cfg->disc_enable = eep_config.disc_enable;
asc_dvc->max_host_qng = eep_config.max_host_qng;
asc_dvc->max_dvc_qng = eep_config.max_dvc_qng;
asc_dvc->chip_scsi_id = (eep_config.adapter_scsi_id & ADV_MAX_TID);
asc_dvc->start_motor = eep_config.start_motor;
asc_dvc->scsi_reset_wait = eep_config.scsi_reset_delay;
asc_dvc->cfg->bios_boot_wait = eep_config.bios_boot_delay;
asc_dvc->bios_ctrl = eep_config.bios_ctrl;
asc_dvc->no_scam = eep_config.scam_tolerant;
asc_dvc->cfg->serial1 = eep_config.serial_number_word1;
asc_dvc->cfg->serial2 = eep_config.serial_number_word2;
asc_dvc->cfg->serial3 = eep_config.serial_number_word3;
if (eep_config.max_host_qng > ASC_DEF_MAX_HOST_QNG)
{
eep_config.max_host_qng = ASC_DEF_MAX_HOST_QNG;
} else if (eep_config.max_host_qng < ASC_DEF_MIN_HOST_QNG)
{
if (eep_config.max_host_qng == 0)
{
eep_config.max_host_qng = ASC_DEF_MAX_HOST_QNG;
} else
{
eep_config.max_host_qng = ASC_DEF_MIN_HOST_QNG;
}
}
if (eep_config.max_dvc_qng > ASC_DEF_MAX_DVC_QNG)
{
eep_config.max_dvc_qng = ASC_DEF_MAX_DVC_QNG;
} else if (eep_config.max_dvc_qng < ASC_DEF_MIN_DVC_QNG)
{
if (eep_config.max_dvc_qng == 0)
{
eep_config.max_dvc_qng = ASC_DEF_MAX_DVC_QNG;
} else
{
eep_config.max_dvc_qng = ASC_DEF_MIN_DVC_QNG;
}
}
if (eep_config.max_dvc_qng > eep_config.max_host_qng)
{
eep_config.max_dvc_qng = eep_config.max_host_qng;
}
asc_dvc->max_host_qng = eep_config.max_host_qng;
asc_dvc->max_dvc_qng = eep_config.max_dvc_qng;
if (eep_config.termination == 0)
{
asc_dvc->cfg->termination = 0;
} else
{
if (eep_config.termination == 1)
{
asc_dvc->cfg->termination = TERM_CTL_SEL;
} else if (eep_config.termination == 2)
{
asc_dvc->cfg->termination = TERM_CTL_SEL | TERM_CTL_H;
} else if (eep_config.termination == 3)
{
asc_dvc->cfg->termination = TERM_CTL_SEL | TERM_CTL_H | TERM_CTL_L;
} else
{
asc_dvc->cfg->termination = 0;
warn_code |= ASC_WARN_EEPROM_TERMINATION;
}
}
return warn_code;
}
ASC_INITFUNC(
STATIC ushort
AdvGetEEPConfig(AdvPortAddr iop_base, ADVEEP_CONFIG *cfg_buf)
)
{
ushort              wval, chksum;
ushort              *wbuf;
int                 eep_addr;
wbuf = (ushort *) cfg_buf;
chksum = 0;
for (eep_addr = ASC_EEP_DVC_CFG_BEGIN;
eep_addr < ASC_EEP_DVC_CFG_END;
eep_addr++, wbuf++)
{
wval = AdvReadEEPWord(iop_base, eep_addr);
chksum += wval;
*wbuf = wval;
}
*wbuf = AdvReadEEPWord(iop_base, eep_addr);
wbuf++;
for (eep_addr = ASC_EEP_DVC_CTL_BEGIN;
eep_addr < ASC_EEP_MAX_WORD_ADDR;
eep_addr++, wbuf++)
{
*wbuf = AdvReadEEPWord(iop_base, eep_addr);
}
return chksum;
}
ASC_INITFUNC(
STATIC ushort
AdvReadEEPWord(AdvPortAddr iop_base, int eep_word_addr)
)
{
AdvWriteWordRegister(iop_base, IOPW_EE_CMD,
ASC_EEP_CMD_READ | eep_word_addr);
AdvWaitEEPCmd(iop_base);
return AdvReadWordRegister(iop_base, IOPW_EE_DATA);
}
ASC_INITFUNC(
STATIC void
AdvWaitEEPCmd(AdvPortAddr iop_base)
)
{
int eep_delay_ms;
for (eep_delay_ms = 0; eep_delay_ms < ASC_EEP_DELAY_MS; eep_delay_ms++)
{
if (AdvReadWordRegister(iop_base, IOPW_EE_CMD) & ASC_EEP_CMD_DONE)
{
break;
}
DvcSleepMilliSecond(1);
}
if ((AdvReadWordRegister(iop_base, IOPW_EE_CMD) & ASC_EEP_CMD_DONE) == 0)
{
ADV_ASSERT(0);
}
return;
}
ASC_INITFUNC(
STATIC void
AdvSetEEPConfig(AdvPortAddr iop_base, ADVEEP_CONFIG *cfg_buf)
)
{
ushort       *wbuf;
ushort       addr, chksum;
wbuf = (ushort *) cfg_buf;
chksum = 0;
AdvWriteWordRegister(iop_base, IOPW_EE_CMD, ASC_EEP_CMD_WRITE_ABLE);
AdvWaitEEPCmd(iop_base);
for (addr = ASC_EEP_DVC_CFG_BEGIN;
addr < ASC_EEP_DVC_CFG_END; addr++, wbuf++)
{
chksum += *wbuf;
AdvWriteWordRegister(iop_base, IOPW_EE_DATA, *wbuf);
AdvWriteWordRegister(iop_base, IOPW_EE_CMD, ASC_EEP_CMD_WRITE | addr);
AdvWaitEEPCmd(iop_base);
DvcSleepMilliSecond(ASC_EEP_DELAY_MS);
}
AdvWriteWordRegister(iop_base, IOPW_EE_DATA, chksum);
AdvWriteWordRegister(iop_base, IOPW_EE_CMD, ASC_EEP_CMD_WRITE | addr);
AdvWaitEEPCmd(iop_base);
wbuf++;
for (addr = ASC_EEP_DVC_CTL_BEGIN;
addr < ASC_EEP_MAX_WORD_ADDR; addr++, wbuf++)
{
AdvWriteWordRegister(iop_base, IOPW_EE_DATA, *wbuf);
AdvWriteWordRegister(iop_base, IOPW_EE_CMD, ASC_EEP_CMD_WRITE | addr);
AdvWaitEEPCmd(iop_base);
}
AdvWriteWordRegister(iop_base, IOPW_EE_CMD, ASC_EEP_CMD_WRITE_DISABLE);
AdvWaitEEPCmd(iop_base);
return;
}
ASC_INITFUNC(
STATIC void
AdvResetChip(ADV_DVC_VAR *asc_dvc)
)
{
AdvPortAddr    iop_base;
ushort         word;
uchar          byte;
iop_base = asc_dvc->iop_base;
AdvWriteWordRegister(iop_base, IOPW_CTRL_REG, ADV_CTRL_REG_CMD_RESET);
DvcSleepMilliSecond(100);
AdvWriteWordRegister(iop_base, IOPW_CTRL_REG, ADV_CTRL_REG_CMD_WR_IO_REG);
byte = AdvReadByteRegister(iop_base, IOPB_MEM_CFG);
byte |= RAM_SZ_8KB;
AdvWriteByteRegister(iop_base, IOPB_MEM_CFG, byte);
word = AdvReadWordRegister(iop_base, IOPW_SCSI_CFG1);
word &= ~BIG_ENDIAN;
AdvWriteWordRegister(iop_base, IOPW_SCSI_CFG1, word);
AdvWriteByteRegister(iop_base, IOPB_DMA_CFG0,
START_CTL_EMFU | READ_CMD_MRM);
}
STATIC int
AdvExeScsiQueue(ADV_DVC_VAR *asc_dvc,
ADV_SCSI_REQ_Q *scsiq)
{
if (scsiq == (ADV_SCSI_REQ_Q *) 0L)
{
ADV_ASSERT(0);
return ADV_ERROR;
}
return AdvSendScsiCmd(asc_dvc, scsiq);
}
STATIC int
AdvResetSB(ADV_DVC_VAR *asc_dvc)
{
int         status;
status = AdvSendIdleCmd(asc_dvc, (ushort) IDLE_CMD_SCSI_RESET, 0L, 0);
AdvResetSCSIBus(asc_dvc);
return status;
}
STATIC void
AdvResetSCSIBus(ADV_DVC_VAR *asc_dvc)
{
AdvPortAddr    iop_base;
ushort         scsi_ctrl;
iop_base = asc_dvc->iop_base;
scsi_ctrl = AdvReadWordRegister(iop_base, IOPW_SCSI_CTRL);
AdvWriteWordRegister(iop_base, IOPW_SCSI_CTRL,
scsi_ctrl | ADV_SCSI_CTRL_RSTOUT);
DvcDelayMicroSecond(asc_dvc, (ushort) ASC_SCSI_RESET_HOLD_TIME_US);
AdvWriteWordRegister(iop_base, IOPW_SCSI_CTRL,
scsi_ctrl & ~ADV_SCSI_CTRL_RSTOUT);
DvcSleepMilliSecond((ulong) asc_dvc->scsi_reset_wait * 1000);
}
STATIC int
AdvISR(ADV_DVC_VAR *asc_dvc)
{
AdvPortAddr                 iop_base;
uchar                       int_stat;
ushort                      next_done_loc, target_bit;
int                         completed_q;
long                        flags;
ADV_SCSI_REQ_Q              *scsiq;
ASC_REQ_SENSE               *sense_data;
int                         ret;
flags = DvcEnterCritical();
iop_base = asc_dvc->iop_base;
if (AdvIsIntPending(iop_base))
{
ret = ADV_TRUE;
} else
{
ret = ADV_FALSE;
}
int_stat = AdvReadByteRegister(iop_base, IOPB_INTR_STATUS_REG);
if (int_stat & ADV_INTR_STATUS_INTRB)
{
asc_dvc->idle_cmd_done = ADV_TRUE;
}
if (int_stat & ADV_INTR_STATUS_INTRC)
{
if (asc_dvc->sbreset_callback != 0)
{
(*(ADV_SBRESET_CALLBACK) asc_dvc->sbreset_callback)(asc_dvc);
}
}
AdvReadByteLram(iop_base, ASC_MC_HOST_NEXT_DONE, next_done_loc);
next_done_loc = ASC_MC_RISC_Q_LIST_BASE +
(next_done_loc * ASC_MC_RISC_Q_LIST_SIZE) + RQL_FWD;
AdvReadByteLram(iop_base, next_done_loc, completed_q);
while (completed_q != ASC_MC_NULL_Q)
{
AdvWriteByteLram(iop_base, ASC_MC_HOST_NEXT_DONE, completed_q);
next_done_loc = ASC_MC_RISC_Q_LIST_BASE +
(completed_q * ASC_MC_RISC_Q_LIST_SIZE);
{
ushort lsw, msw;
AdvReadWordLram(iop_base, next_done_loc + RQL_PHYADDR, lsw);
AdvReadWordLram(iop_base, next_done_loc + RQL_PHYADDR + 2, msw);
scsiq = (ADV_SCSI_REQ_Q *) (((ulong) msw << 16) | lsw);
}
ADV_ASSERT(scsiq != NULL);
target_bit = ADV_TID_TO_TIDMASK(scsiq->target_id);
scsiq->cntl = 0;
if ((scsiq->done_status == QD_WITH_ERROR) &&
(scsiq->scsi_status == SS_CHK_CONDITION) &&
(sense_data = (ASC_REQ_SENSE *) scsiq->vsense_addr) != 0 &&
(scsiq->orig_sense_len - scsiq->sense_len) >= ASC_MIN_SENSE_LEN)
{
}
else if (scsiq->done_status == QD_NO_ERROR &&
scsiq->cdb[0] == SCSICMD_Inquiry &&
scsiq->target_lun == 0)
{
AdvInquiryHandling(asc_dvc, scsiq);
}
AdvWriteByteLram(iop_base, next_done_loc + RQL_STATE, ASC_MC_QS_FREE);
AdvReadByteLram(iop_base, next_done_loc + RQL_FWD, completed_q);
ADV_ASSERT(asc_dvc->cur_host_qng > 0);
asc_dvc->cur_host_qng--;
scsiq->a_flag |= ADV_SCSIQ_DONE;
(*(ADV_ISR_CALLBACK) asc_dvc->isr_callback)(asc_dvc, scsiq);
(void) DvcEnterCritical();
}
DvcLeaveCritical(flags);
return ret;
}
STATIC int
AdvSendIdleCmd(ADV_DVC_VAR *asc_dvc,
ushort idle_cmd,
ulong idle_cmd_parameter,
int flags)
{
int         last_int_level;
ulong       i;
AdvPortAddr iop_base;
int         ret;
asc_dvc->idle_cmd_done = 0;
last_int_level = DvcEnterCritical();
iop_base = asc_dvc->iop_base;
AdvWriteDWordLram(iop_base, ASC_MC_IDLE_PARA_STAT, idle_cmd_parameter);
AdvWriteWordLram(iop_base, ASC_MC_IDLE_CMD, idle_cmd);
DvcLeaveCritical(last_int_level);
if (flags & ADV_NOWAIT)
{
return ADV_TRUE;
}
for (i = 0; i < SCSI_WAIT_10_SEC * SCSI_MS_PER_SEC; i++)
{
if (asc_dvc->idle_cmd_done)
{
break;
}
DvcSleepMilliSecond(1);
(void) AdvISR(asc_dvc);
}
last_int_level = DvcEnterCritical();
if (asc_dvc->idle_cmd_done == ADV_FALSE)
{
ADV_ASSERT(0);
return ADV_FALSE;
} else
{
AdvReadWordLram(iop_base, ASC_MC_IDLE_PARA_STAT, ret);
return ret;
}
}
STATIC int
AdvSendScsiCmd(
ADV_DVC_VAR *asc_dvc,
ADV_SCSI_REQ_Q  *scsiq)
{
ushort                 next_ready_loc;
uchar                  next_ready_loc_fwd;
int                    last_int_level;
AdvPortAddr            iop_base;
long                   req_size;
ulong                  q_phy_addr;
if (scsiq->target_id == asc_dvc->chip_scsi_id ||
scsiq->target_id > ADV_MAX_TID)
{
scsiq->host_status = QHSTA_M_INVALID_DEVICE;
scsiq->done_status = QD_WITH_ERROR;
return ADV_ERROR;
}
iop_base = asc_dvc->iop_base;
last_int_level = DvcEnterCritical();
if (asc_dvc->cur_host_qng >= asc_dvc->max_host_qng)
{
DvcLeaveCritical(last_int_level);
return ADV_BUSY;
} else
{
ADV_ASSERT(asc_dvc->cur_host_qng < ASC_MC_RISC_Q_TOTAL_CNT);
asc_dvc->cur_host_qng++;
}
scsiq->a_flag &= ~ADV_SCSIQ_DONE;
scsiq->orig_sense_len = scsiq->sense_len;
AdvReadByteLram(iop_base, ASC_MC_HOST_NEXT_READY, next_ready_loc);
next_ready_loc = ASC_MC_RISC_Q_LIST_BASE +
(next_ready_loc * ASC_MC_RISC_Q_LIST_SIZE);
req_size = sizeof(ADV_SCSI_REQ_Q);
q_phy_addr = DvcGetPhyAddr(asc_dvc, scsiq,
(uchar *) scsiq, &req_size,
ADV_IS_SCSIQ_FLAG);
ADV_ASSERT(ADV_DWALIGN(q_phy_addr) == q_phy_addr);
ADV_ASSERT(req_size >= sizeof(ADV_SCSI_REQ_Q));
scsiq->scsiq_ptr = (ADV_SCSI_REQ_Q *) scsiq;
AdvWriteDWordLram(iop_base, next_ready_loc + RQL_PHYADDR, q_phy_addr);
AdvWriteByteLram(iop_base, next_ready_loc + RQL_TID, scsiq->target_id);
AdvReadByteLram(iop_base, next_ready_loc + RQL_FWD, next_ready_loc_fwd);
AdvWriteByteLram(iop_base, ASC_MC_HOST_NEXT_READY, next_ready_loc_fwd);
AdvWriteByteLram(iop_base, next_ready_loc + RQL_STATE, ASC_MC_QS_READY);
DvcLeaveCritical(last_int_level);
return ADV_SUCCESS;
}
STATIC void
AdvInquiryHandling(
ADV_DVC_VAR          *asc_dvc,
ADV_SCSI_REQ_Q       *scsiq)
{
AdvPortAddr          iop_base;
uchar                tid;
ASC_SCSI_INQUIRY     *inq;
ushort               tidmask;
ushort               cfg_word;
if (scsiq->cdb[4] < 8 || (scsiq->cdb[4] - scsiq->data_cnt) < 8)
{
return;
}
iop_base = asc_dvc->iop_base;
tid = scsiq->target_id;
inq = (ASC_SCSI_INQUIRY *) scsiq->vdata_addr;
if (inq->byte3.rsp_data_fmt < 2 && inq->byte2.ansi_apr_ver < 2)
{
return;
} else
{
tidmask = ADV_TID_TO_TIDMASK(tid);
if ((asc_dvc->wdtr_able & tidmask) && inq->byte7.WBus16)
{
AdvReadWordLram(iop_base, ASC_MC_WDTR_ABLE, cfg_word);
if ((cfg_word & tidmask) == 0)
{
cfg_word |= tidmask;
AdvWriteWordLram(iop_base, ASC_MC_WDTR_ABLE, cfg_word);
AdvReadWordLram(iop_base, ASC_MC_WDTR_DONE, cfg_word);
cfg_word &= ~tidmask;
AdvWriteWordLram(iop_base, ASC_MC_WDTR_DONE, cfg_word);
}
}
if ((asc_dvc->sdtr_able & tidmask) && inq->byte7.Sync)
{
AdvReadWordLram(iop_base, ASC_MC_SDTR_ABLE, cfg_word);
if ((cfg_word & tidmask) == 0)
{
cfg_word |= tidmask;
AdvWriteWordLram(iop_base, ASC_MC_SDTR_ABLE, cfg_word);
AdvReadWordLram(iop_base, ASC_MC_SDTR_DONE, cfg_word);
cfg_word &= ~tidmask;
AdvWriteWordLram(iop_base, ASC_MC_SDTR_DONE, cfg_word);
}
}
if ((asc_dvc->tagqng_able & tidmask) && inq->byte7.CmdQue)
{
AdvReadWordLram(iop_base, ASC_MC_TAGQNG_ABLE, cfg_word);
cfg_word |= tidmask;
AdvWriteWordLram(iop_base, ASC_MC_TAGQNG_ABLE, cfg_word);
AdvWriteByteLram(iop_base, ASC_MC_NUMBER_OF_MAX_CMD + tid,
asc_dvc->max_dvc_qng);
}
}
}