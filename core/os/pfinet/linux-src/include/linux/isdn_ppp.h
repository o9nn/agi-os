#ifndef _LINUX_ISDN_PPP_H
#define _LINUX_ISDN_PPP_H
#include <linux/config.h>
#define CALLTYPE_INCOMING 0x1
#define CALLTYPE_OUTGOING 0x2
#define CALLTYPE_CALLBACK 0x4
#define IPPP_VERSION    "2.2.0"
struct pppcallinfo
{
int calltype;
unsigned char local_num[64];
unsigned char remote_num[64];
int charge_units;
};
#define PPPIOCGCALLINFO _IOWR('t',128,struct pppcallinfo)
#define PPPIOCBUNDLE   _IOW('t',129,int)
#define PPPIOCGMPFLAGS _IOR('t',130,int)
#define PPPIOCSMPFLAGS _IOW('t',131,int)
#define PPPIOCSMPMTU   _IOW('t',132,int)
#define PPPIOCSMPMRU   _IOW('t',133,int)
#define PPPIOCGCOMPRESSORS _IOR('t',134,unsigned long [8])
#define PPPIOCSCOMPRESSOR _IOW('t',135,int)
#define PPPIOCGIFNAME      _IOR('t',136, char [IFNAMSIZ] )
#define PPP_MP          0x003d
#define PPP_LINK_COMP   0x00fb
#define PPP_LINK_CCP    0x80fb
#define SC_MP_PROT       0x00000200
#define SC_REJ_MP_PROT   0x00000400
#define SC_OUT_SHORT_SEQ 0x00000800
#define SC_IN_SHORT_SEQ  0x00004000
#define SC_DECOMP_ON		0x01
#define SC_COMP_ON		0x02
#define SC_DECOMP_DISCARD	0x04
#define SC_COMP_DISCARD		0x08
#define SC_LINK_DECOMP_ON	0x10
#define SC_LINK_COMP_ON		0x20
#define SC_LINK_DECOMP_DISCARD	0x40
#define SC_LINK_COMP_DISCARD	0x80
#define DECOMP_ERR_NOMEM	(-10)
#define MP_END_FRAG    0x40
#define MP_BEGIN_FRAG  0x80
#define ISDN_PPP_COMP_MAX_OPTIONS 16
#define IPPP_COMP_FLAG_XMIT 0x1
#define IPPP_COMP_FLAG_LINK 0x2
struct isdn_ppp_comp_data {
int num;
unsigned char options[ISDN_PPP_COMP_MAX_OPTIONS];
int optlen;
int flags;
};
#ifdef __KERNEL__
#define IPPP_RESET_MAXDATABYTES	32
struct isdn_ppp_resetparams {
unsigned char valid:1;
unsigned char rsend:1;
unsigned char idval:1;
unsigned char dtval:1;
unsigned char expra:1;
unsigned char id;
unsigned short maxdlen;
unsigned short dlen;
unsigned char *data;
};
struct isdn_ppp_compressor {
struct isdn_ppp_compressor *next, *prev;
int num;
void *(*alloc) (struct isdn_ppp_comp_data *);
void (*free) (void *state);
int  (*init) (void *state, struct isdn_ppp_comp_data *,
int unit,int debug);
void (*reset) (void *state, unsigned char code, unsigned char id,
unsigned char *data, unsigned len,
struct isdn_ppp_resetparams *rsparm);
int  (*compress) (void *state, struct sk_buff *in,
struct sk_buff *skb_out, int proto);
int  (*decompress) (void *state,struct sk_buff *in,
struct sk_buff *skb_out,
struct isdn_ppp_resetparams *rsparm);
void (*incomp) (void *state, struct sk_buff *in,int proto);
void (*stat) (void *state, struct compstat *stats);
};
extern int isdn_ppp_register_compressor(struct isdn_ppp_compressor *);
extern int isdn_ppp_unregister_compressor(struct isdn_ppp_compressor *);
extern int isdn_ppp_dial_slave(char *);
extern int isdn_ppp_hangup_slave(char *);
struct ippp_bundle {
int mp_mrru;
struct mpqueue *last;
int min;
long next_num;
struct sqqueue *sq;
int modify:1;
int bundled:1;
};
#define NUM_RCV_BUFFS     64
struct sqqueue {
struct sqqueue *next;
long sqno_start;
long sqno_end;
struct sk_buff *skb;
long timer;
};
struct mpqueue {
struct mpqueue *next;
struct mpqueue *last;
long sqno;
struct sk_buff *skb;
int BEbyte;
unsigned long time;
};
struct ippp_buf_queue {
struct ippp_buf_queue *next;
struct ippp_buf_queue *last;
char *buf;
int len;
};
enum ippp_ccp_reset_states {
CCPResetIdle,
CCPResetSentReq,
CCPResetRcvdReq,
CCPResetSentAck,
CCPResetRcvdAck
};
struct ippp_ccp_reset_state {
enum ippp_ccp_reset_states state;
struct ippp_struct *is;
unsigned char id;
unsigned char ta:1;
unsigned char expra:1;
int dlen;
struct timer_list timer;
unsigned char data[IPPP_RESET_MAXDATABYTES];
};
struct ippp_ccp_reset {
struct ippp_ccp_reset_state *rs[256];
unsigned char lastid;
};
struct ippp_struct {
struct ippp_struct *next_link;
int state;
struct ippp_buf_queue rq[NUM_RCV_BUFFS];
struct ippp_buf_queue *first;
struct ippp_buf_queue *last;
struct wait_queue *wq;
struct task_struct *tk;
unsigned int mpppcfg;
unsigned int pppcfg;
unsigned int mru;
unsigned int mpmru;
unsigned int mpmtu;
unsigned int maxcid;
struct isdn_net_local_s *lp;
int unit;
int minor;
long last_link_seqno;
long mp_seqno;
long range;
#ifdef CONFIG_ISDN_PPP_VJ
unsigned char *cbuf;
struct slcompress *slcomp;
#endif
unsigned long debug;
struct isdn_ppp_compressor *compressor,*decompressor;
struct isdn_ppp_compressor *link_compressor,*link_decompressor;
void *decomp_stat,*comp_stat,*link_decomp_stat,*link_comp_stat;
struct ippp_ccp_reset *reset;
unsigned long compflags;
};
#endif
#endif