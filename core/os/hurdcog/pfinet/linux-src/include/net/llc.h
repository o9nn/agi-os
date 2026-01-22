#include <linux/skbuff.h>
#define LLC_MODULE
typedef struct llc_struct llc;
typedef struct llc_struct *llcptr;
struct llc_struct
{
char eye[4];
int retry_count;
unsigned char name[9];
unsigned char s_flag;
unsigned char p_flag;
unsigned char f_flag;
unsigned char data_flag;
unsigned char cause_flag;
unsigned char vs;
unsigned char vr;
unsigned char remote_busy;
unsigned char state;
int n1;
int n2;
unsigned char k;
unsigned char rw;
struct
{
unsigned char cntl1;
unsigned char cntl2;
unsigned char vs;
unsigned char vr_cr;
unsigned char xxyz;
} frmr_info_fld;
#define P_TIMER 0
#define REJ_TIMER 1
#define ACK_TIMER 2
#define BUSY_TIMER 3
unsigned long timer_expire_time[4];
unsigned char timer_state[4];
#define TIMER_IDLE 0
#define TIMER_RUNNING 1
#define TIMER_EXPIRED 2
unsigned long timer_interval[4];
struct timer_list tl[4];
void (*llc_event)(struct llc_struct *);
char * client_data;
unsigned char local_sap;
unsigned char remote_sap ;
char remote_mac[MAX_ADDR_LEN];
struct device *dev;
unsigned char llc_mode;
#define MODE_ADM 1
#define MODE_ABM 2
int llc_callbacks;
#define LLC_CONN_INDICATION 1
#define LLC_CONN_CONFIRM 2
#define LLC_DATA_INDIC 4
#define LLC_DISC_INDICATION 8
#define LLC_RESET_INDIC_LOC 16
#define LLC_RESET_INDIC_REM 32
#define LLC_RST_CONFIRM 64
#define LLC_FRMR_RECV 128
#define LLC_FRMR_SENT 256
#define LLC_REMOTE_BUSY 512
#define LLC_REMOTE_NOTBUSY 1024
#define LLC_TEST_INDICATION 2048
#define LLC_XID_INDICATION 4096
#define LLC_UI_DATA 8192
struct sk_buff *inc_skb;
struct sk_buff_head rtq;
struct sk_buff_head atq;
unsigned char xid_count;
struct llc_struct *nextllc;
};
#define ADD_TO_RTQ(skb) skb_queue_tail(&lp->rtq,skb)
#define ADD_TO_ATQ(skb) skb_queue_tail(&lp->atq,skb)
void llc_cancel_timers(llcptr lp);
int llc_decode_frametype(frameptr fr);
llcptr llc_find(void);
int llc_free_acknowledged_skbs(llcptr lp, unsigned char ack);
void llc_handle_xid_indication( char *chsp, short int ll, char *xid_data);
void llc_interpret_pseudo_code(llcptr lp, int pc_label, struct sk_buff *skb, char type);
void llc_add_to_queue(struct sk_buff *skb, struct sk_buff **f, struct sk_buff **b);
void llc_process_otype2_frame(llcptr lp, struct sk_buff *skb, char type);
struct sk_buff *llc_pull_from_atq(llcptr lp);
int llc_resend_ipdu(llcptr lp, unsigned char ack_nr, unsigned char type, char p);
void llc_sendpdu(llcptr lp, char type, char pf, int data_len, char *pdu_data);
void llc_sendipdu(llcptr lp, char type, char pf, struct sk_buff *skb);
void llc_start_timer(llcptr lp, int t);
void llc_stop_timer(llcptr lp, int t);
void llc_timer_expired(llcptr lp, int t);
int llc_validate_seq_nos(llcptr lp, frameptr fr);
int llc_data_request(llcptr lp, struct sk_buff *skb);
void llc_unit_data_request(llcptr lp, int ll, char * data);
void llc_disconnect_request(llcptr lp);
void llc_connect_request(llcptr lp);
void llc_xid_request(llcptr lp, char opt, int data_len, char *pdu_data);
void llc_test_request(llcptr lp, int data_len, char *pdu_data);
int register_cl2llc_client(llcptr llc, const char *device, void (*ops)(llcptr), u8 *rmac, u8 ssap, u8 dsap);
void unregister_cl2llc_client(llcptr lp);
int llc_mac_data_indicate(llcptr lp, struct sk_buff *skb );