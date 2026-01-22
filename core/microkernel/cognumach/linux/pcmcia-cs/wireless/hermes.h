#ifndef _HERMES_H
#define _HERMES_H
#include <linux/delay.h>
#include <linux/if_ether.h>
#include <asm/byteorder.h>
#define		HERMES_ALLOC_LEN_MIN		(4)
#define		HERMES_ALLOC_LEN_MAX		(2400)
#define		HERMES_LTV_LEN_MAX		(34)
#define		HERMES_BAP_DATALEN_MAX		(4096)
#define		HERMES_BAP_OFFSET_MAX		(4096)
#define		HERMES_PORTID_MAX		(7)
#define		HERMES_NUMPORTS_MAX		(HERMES_PORTID_MAX+1)
#define		HERMES_PDR_LEN_MAX		(260)
#define		HERMES_PDA_RECS_MAX		(200)
#define		HERMES_PDA_LEN_MAX		(1024)
#define		HERMES_SCANRESULT_MAX		(35)
#define		HERMES_CHINFORESULT_MAX		(8)
#define		HERMES_MAX_MULTICAST		(16)
#define		HERMES_MAGIC			(0x7d1f)
#define		HERMES_CMD			(0x00)
#define		HERMES_PARAM0			(0x02)
#define		HERMES_PARAM1			(0x04)
#define		HERMES_PARAM2			(0x06)
#define		HERMES_STATUS			(0x08)
#define		HERMES_RESP0			(0x0A)
#define		HERMES_RESP1			(0x0C)
#define		HERMES_RESP2			(0x0E)
#define		HERMES_INFOFID			(0x10)
#define		HERMES_RXFID			(0x20)
#define		HERMES_ALLOCFID			(0x22)
#define		HERMES_TXCOMPLFID		(0x24)
#define		HERMES_SELECT0			(0x18)
#define		HERMES_OFFSET0			(0x1C)
#define		HERMES_DATA0			(0x36)
#define		HERMES_SELECT1			(0x1A)
#define		HERMES_OFFSET1			(0x1E)
#define		HERMES_DATA1			(0x38)
#define		HERMES_EVSTAT			(0x30)
#define		HERMES_INTEN			(0x32)
#define		HERMES_EVACK			(0x34)
#define		HERMES_CONTROL			(0x14)
#define		HERMES_SWSUPPORT0		(0x28)
#define		HERMES_SWSUPPORT1		(0x2A)
#define		HERMES_SWSUPPORT2		(0x2C)
#define		HERMES_AUXPAGE			(0x3A)
#define		HERMES_AUXOFFSET		(0x3C)
#define		HERMES_AUXDATA			(0x3E)
#define		HERMES_CMD_BUSY			(0x8000)
#define		HERMES_CMD_AINFO		(0x7f00)
#define		HERMES_CMD_MACPORT		(0x0700)
#define		HERMES_CMD_RECL			(0x0100)
#define		HERMES_CMD_WRITE		(0x0100)
#define		HERMES_CMD_PROGMODE		(0x0300)
#define		HERMES_CMD_CMDCODE		(0x003f)
#define		HERMES_STATUS_RESULT		(0x7f00)
#define		HERMES_STATUS_CMDCODE		(0x003f)
#define		HERMES_OFFSET_BUSY		(0x8000)
#define		HERMES_OFFSET_ERR		(0x4000)
#define		HERMES_OFFSET_DATAOFF		(0x0ffe)
#define		HERMES_EV_TICK			(0x8000)
#define		HERMES_EV_WTERR			(0x4000)
#define		HERMES_EV_INFDROP		(0x2000)
#define		HERMES_EV_INFO			(0x0080)
#define		HERMES_EV_DTIM			(0x0020)
#define		HERMES_EV_CMD			(0x0010)
#define		HERMES_EV_ALLOC			(0x0008)
#define		HERMES_EV_TXEXC			(0x0004)
#define		HERMES_EV_TX			(0x0002)
#define		HERMES_EV_RX			(0x0001)
#define		HERMES_CMD_INIT			(0x0000)
#define		HERMES_CMD_ENABLE		(0x0001)
#define		HERMES_CMD_DISABLE		(0x0002)
#define		HERMES_CMD_DIAG			(0x0003)
#define		HERMES_CMD_ALLOC		(0x000A)
#define		HERMES_CMD_TX			(0x000B)
#define		HERMES_CMD_CLRPRST		(0x0012)
#define		HERMES_CMD_NOTIFY		(0x0010)
#define		HERMES_CMD_INQUIRE		(0x0011)
#define		HERMES_CMD_ACCESS		(0x0021)
#define		HERMES_CMD_DOWNLD		(0x0022)
#define 	HERMES_CMD_MONITOR		(0x0038)
#define		HERMES_MONITOR_ENABLE		(0x000b)
#define		HERMES_MONITOR_DISABLE		(0x000f)
#define HERMES_DESCRIPTOR_OFFSET	0
#define HERMES_802_11_OFFSET		(14)
#define HERMES_802_3_OFFSET		(14+32)
#define HERMES_802_2_OFFSET		(14+32+14)
struct hermes_rx_descriptor {
u16 status;
u32 time;
u8 silence;
u8 signal;
u8 rate;
u8 rxflow;
u32 reserved;
} __attribute__ ((packed));
#define HERMES_RXSTAT_ERR		(0x0003)
#define	HERMES_RXSTAT_BADCRC		(0x0001)
#define	HERMES_RXSTAT_UNDECRYPTABLE	(0x0002)
#define	HERMES_RXSTAT_MACPORT		(0x0700)
#define HERMES_RXSTAT_PCF		(0x1000)
#define	HERMES_RXSTAT_MSGTYPE		(0xE000)
#define	HERMES_RXSTAT_1042		(0x2000)
#define	HERMES_RXSTAT_TUNNEL		(0x4000)
#define	HERMES_RXSTAT_WMP		(0x6000)
struct hermes_tx_descriptor {
u16 status;
u16 reserved1;
u16 reserved2;
u32 sw_support;
u8 retry_count;
u8 tx_rate;
u16 tx_control;
} __attribute__ ((packed));
#define HERMES_TXSTAT_RETRYERR		(0x0001)
#define HERMES_TXSTAT_AGEDERR		(0x0002)
#define HERMES_TXSTAT_DISCON		(0x0004)
#define HERMES_TXSTAT_FORMERR		(0x0008)
#define HERMES_TXCTRL_TX_OK		(0x0002)
#define HERMES_TXCTRL_TX_EX		(0x0004)
#define HERMES_TXCTRL_802_11		(0x0008)
#define HERMES_TXCTRL_ALT_RTRY		(0x0020)
#define HERMES_INQ_TALLIES		(0xF100)
#define HERMES_INQ_SCAN			(0xF101)
#define HERMES_INQ_LINKSTATUS		(0xF200)
struct hermes_tallies_frame {
u16 TxUnicastFrames;
u16 TxMulticastFrames;
u16 TxFragments;
u16 TxUnicastOctets;
u16 TxMulticastOctets;
u16 TxDeferredTransmissions;
u16 TxSingleRetryFrames;
u16 TxMultipleRetryFrames;
u16 TxRetryLimitExceeded;
u16 TxDiscards;
u16 RxUnicastFrames;
u16 RxMulticastFrames;
u16 RxFragments;
u16 RxUnicastOctets;
u16 RxMulticastOctets;
u16 RxFCSErrors;
u16 RxDiscards_NoBuffer;
u16 TxDiscardsWrongSA;
u16 RxWEPUndecryptable;
u16 RxMsgInMsgFragments;
u16 RxMsgInBadMsgFragments;
u16 RxDiscards_WEPICVError;
u16 RxDiscards_WEPExcluded;
} __attribute__ ((packed));
struct hermes_scan_apinfo {
u16 channel;
u16 noise;
u16 level;
u8 bssid[ETH_ALEN];
u16 beacon_interv;
u16 capabilities;
u8 essid[32];
u8 rates[10];
u16 proberesp_rate;
} __attribute__ ((packed));
struct hermes_scan_frame {
u16 rsvd;
u16 scanreason;
struct hermes_scan_apinfo aps[35];
} __attribute__ ((packed));
#define HERMES_LINKSTATUS_NOT_CONNECTED   (0x0000)
#define HERMES_LINKSTATUS_CONNECTED       (0x0001)
#define HERMES_LINKSTATUS_DISCONNECTED    (0x0002)
#define HERMES_LINKSTATUS_AP_CHANGE       (0x0003)
#define HERMES_LINKSTATUS_AP_OUT_OF_RANGE (0x0004)
#define HERMES_LINKSTATUS_AP_IN_RANGE     (0x0005)
#define HERMES_LINKSTATUS_ASSOC_FAILED    (0x0006)
struct hermes_linkstatus {
u16 linkstatus;
} __attribute__ ((packed));
#define HERMES_DEBUG_BUFSIZE 4096
struct hermes_debug_entry {
int bap;
u16 id, offset;
int cycles;
};
#ifdef __KERNEL__
#define HERMES_BAP_BUSY_TIMEOUT (500)
typedef struct hermes {
unsigned long iobase;
int io_space;
#define HERMES_IO	1
#define HERMES_MEM	0
int reg_spacing;
#define HERMES_16BIT_REGSPACING	0
#define HERMES_32BIT_REGSPACING	1
u16 inten;
#ifdef HERMES_DEBUG_BUFFER
struct hermes_debug_entry dbuf[HERMES_DEBUG_BUFSIZE];
unsigned long dbufp;
unsigned long profile[HERMES_BAP_BUSY_TIMEOUT+1];
#endif
} hermes_t;
typedef struct hermes_response {
u16 status, resp0, resp1, resp2;
} hermes_response_t;
#define hermes_read_reg(hw, off) ((hw)->io_space ? \
inw((hw)->iobase + ( (off) << (hw)->reg_spacing )) : \
readw((hw)->iobase + ( (off) << (hw)->reg_spacing )))
#define hermes_write_reg(hw, off, val) ((hw)->io_space ? \
outw_p((val), (hw)->iobase + ( (off) << (hw)->reg_spacing )) : \
writew((val), (hw)->iobase + ( (off) << (hw)->reg_spacing )))
#define hermes_read_regn(hw, name) (hermes_read_reg((hw), HERMES_##name))
#define hermes_write_regn(hw, name, val) (hermes_write_reg((hw), HERMES_##name, (val)))
void hermes_struct_init(hermes_t *hw, ulong address, int io_space, int reg_spacing);
int hermes_init(hermes_t *hw);
int hermes_docmd_wait(hermes_t *hw, u16 cmd, u16 parm0, hermes_response_t *resp);
int hermes_allocate(hermes_t *hw, u16 size, u16 *fid);
int hermes_bap_pread(hermes_t *hw, int bap, void *buf, unsigned len,
u16 id, u16 offset);
int hermes_bap_pwrite(hermes_t *hw, int bap, const void *buf, unsigned len,
u16 id, u16 offset);
int hermes_read_ltv(hermes_t *hw, int bap, u16 rid, unsigned buflen,
u16 *length, void *buf);
int hermes_write_ltv(hermes_t *hw, int bap, u16 rid,
u16 length, const void *value);
static inline int hermes_present(hermes_t *hw)
{
return hermes_read_regn(hw, SWSUPPORT0) == HERMES_MAGIC;
}
static inline void hermes_set_irqmask(hermes_t *hw, u16 events)
{
hw->inten = events;
hermes_write_regn(hw, INTEN, events);
}
static inline int hermes_enable_port(hermes_t *hw, int port)
{
return hermes_docmd_wait(hw, HERMES_CMD_ENABLE | (port << 8),
0, NULL);
}
static inline int hermes_disable_port(hermes_t *hw, int port)
{
return hermes_docmd_wait(hw, HERMES_CMD_DISABLE | (port << 8),
0, NULL);
}
static inline int hermes_inquire(hermes_t *hw, u16 rid)
{
return hermes_docmd_wait(hw, HERMES_CMD_INQUIRE, rid, NULL);
}
#define HERMES_BYTES_TO_RECLEN(n) ( (((n)+1)/2) + 1 )
#define HERMES_RECLEN_TO_BYTES(n) ( ((n)-1) * 2 )
static inline void hermes_read_words(struct hermes *hw, int off, void *buf, unsigned count)
{
off = off << hw->reg_spacing;;
if (hw->io_space) {
insw(hw->iobase + off, buf, count);
} else {
unsigned i;
u16 *p;
for (i = 0, p = buf; i < count; i++) {
*p++ = cpu_to_le16(readw(hw->iobase + off));
}
}
}
static inline void hermes_write_words(struct hermes *hw, int off, const void *buf, unsigned count)
{
off = off << hw->reg_spacing;;
if (hw->io_space) {
outsw(hw->iobase + off, buf, count);
} else {
unsigned i;
const u16 *p;
for (i = 0, p = buf; i < count; i++) {
writew(le16_to_cpu(*p++), hw->iobase + off);
}
}
}
static inline void hermes_clear_words(struct hermes *hw, int off, unsigned count)
{
unsigned i;
off = off << hw->reg_spacing;;
if (hw->io_space) {
for (i = 0; i < count; i++)
outw(0, hw->iobase + off);
} else {
for (i = 0; i < count; i++)
writew(0, hw->iobase + off);
}
}
#define HERMES_READ_RECORD(hw, bap, rid, buf) \
(hermes_read_ltv((hw),(bap),(rid), sizeof(*buf), NULL, (buf)))
#define HERMES_WRITE_RECORD(hw, bap, rid, buf) \
(hermes_write_ltv((hw),(bap),(rid),HERMES_BYTES_TO_RECLEN(sizeof(*buf)),(buf)))
static inline int hermes_read_wordrec(hermes_t *hw, int bap, u16 rid, u16 *word)
{
u16 rec;
int err;
err = HERMES_READ_RECORD(hw, bap, rid, &rec);
*word = le16_to_cpu(rec);
return err;
}
static inline int hermes_write_wordrec(hermes_t *hw, int bap, u16 rid, u16 word)
{
u16 rec = cpu_to_le16(word);
return HERMES_WRITE_RECORD(hw, bap, rid, &rec);
}
#else
#define hermes_read_reg(base, off) (inw((base) + (off)))
#define hermes_write_reg(base, off, val) (outw((val), (base) + (off)))
#define hermes_read_regn(base, name) (hermes_read_reg((base), HERMES_##name))
#define hermes_write_regn(base, name, val) (hermes_write_reg((base), HERMES_##name, (val)))
#define hermes_read_data(base, off, buf, count) (insw((base) + (off), (buf), (count)))
#define hermes_write_data(base, off, buf, count) (outsw((base) + (off), (buf), (count)))
#endif
#endif