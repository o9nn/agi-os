#define NP_IP 0
#define NP_IPX 1
#define NP_AT 2
#define NP_IPV6 3
#define NUM_NP 4
#define OBUFSIZE 256
struct ppp {
int magic;
struct ppp *next;
unsigned long inuse;
int line;
__u32 flags;
int mtu;
int mru;
struct slcompress *slcomp;
struct sk_buff_head xmt_q;
struct sk_buff_head rcv_q;
unsigned long xmit_busy;
struct tty_struct *tty;
struct tty_struct *backup_tty;
__u8 escape;
__u8 toss;
volatile __u8 tty_pushing;
volatile __u8 woke_up;
__u32 xmit_async_map[8];
__u32 recv_async_map;
__u32 bytes_sent;
__u32 bytes_rcvd;
struct sk_buff *tpkt;
int tpkt_pos;
__u16 tfcs;
unsigned char *optr;
unsigned char *olim;
struct sk_buff *rpkt;
__u16 rfcs;
struct wait_queue *read_wait;
unsigned long last_xmit;
unsigned long last_recv;
struct pppstat stats;
struct compressor *sc_xcomp;
void *sc_xc_state;
struct compressor *sc_rcomp;
void *sc_rc_state;
enum NPmode sc_npmode[NUM_NP];
int sc_xfer;
char name[8];
struct device dev;
struct enet_statistics estats;
unsigned char obuf[OBUFSIZE];
};