#ifndef _AX25_H
#define _AX25_H
#include <linux/ax25.h>
#define AX25_SLOWHZ 10
#define AX25_T1CLAMPLO (1 * AX25_SLOWHZ)
#define AX25_T1CLAMPHI (30 * AX25_SLOWHZ)
#define AX25_BPQ_HEADER_LEN 16
#define AX25_KISS_HEADER_LEN 1
#define AX25_HEADER_LEN 17
#define AX25_ADDR_LEN 7
#define AX25_DIGI_HEADER_LEN (AX25_MAX_DIGIS * AX25_ADDR_LEN)
#define AX25_MAX_HEADER_LEN (AX25_HEADER_LEN + AX25_DIGI_HEADER_LEN)
#define AX25_P_ROSE 0x01
#define AX25_P_IP 0xCC
#define AX25_P_ARP 0xCD
#define AX25_P_TEXT 0xF0
#define AX25_P_NETROM 0xCF
#define AX25_P_SEGMENT 0x08
#define AX25_SEG_REM 0x7F
#define AX25_SEG_FIRST 0x80
#define AX25_CBIT 0x80
#define AX25_EBIT 0x01
#define AX25_HBIT 0x80
#define AX25_SSSID_SPARE 0x60
#define AX25_ESSID_SPARE 0x20
#define AX25_DAMA_FLAG 0x20
#define AX25_COND_ACK_PENDING 0x01
#define AX25_COND_REJECT 0x02
#define AX25_COND_PEER_RX_BUSY 0x04
#define AX25_COND_OWN_RX_BUSY 0x08
#ifndef _LINUX_NETDEVICE_H
#include <linux/netdevice.h>
#endif
#define AX25_I 0x00
#define AX25_S 0x01
#define AX25_RR 0x01
#define AX25_RNR 0x05
#define AX25_REJ 0x09
#define AX25_U 0x03
#define AX25_SABM 0x2f
#define AX25_SABME 0x6f
#define AX25_DISC 0x43
#define AX25_DM 0x0f
#define AX25_UA 0x63
#define AX25_FRMR 0x87
#define AX25_UI 0x03
#define AX25_PF 0x10
#define AX25_EPF 0x01
#define AX25_ILLEGAL 0x100
#define AX25_POLLOFF 0
#define AX25_POLLON 1
#define AX25_COMMAND 1
#define AX25_RESPONSE 2
enum {
AX25_STATE_0,
AX25_STATE_1,
AX25_STATE_2,
AX25_STATE_3,
AX25_STATE_4
};
#define AX25_MAX_DEVICES 20
#define AX25_MODULUS 8
#define AX25_EMODULUS 128
enum {
AX25_VALUES_IPDEFMODE,
AX25_VALUES_AXDEFMODE,
AX25_VALUES_BACKOFF,
AX25_VALUES_CONMODE,
AX25_VALUES_WINDOW,
AX25_VALUES_EWINDOW,
AX25_VALUES_T1,
AX25_VALUES_T2,
AX25_VALUES_T3,
AX25_VALUES_IDLE,
AX25_VALUES_N2,
AX25_VALUES_PACLEN,
AX25_MAX_VALUES
};
#define AX25_DEF_IPDEFMODE 0
#define AX25_DEF_AXDEFMODE 0
#define AX25_DEF_BACKOFF 1
#define AX25_DEF_CONMODE 2
#define AX25_DEF_WINDOW 2
#define AX25_DEF_EWINDOW 32
#define AX25_DEF_T1 (10 * AX25_SLOWHZ)
#define AX25_DEF_T2 (3 * AX25_SLOWHZ)
#define AX25_DEF_T3 (300 * AX25_SLOWHZ)
#define AX25_DEF_N2 10
#define AX25_DEF_IDLE (0 * 60 * AX25_SLOWHZ)
#define AX25_DEF_PACLEN 256
typedef struct ax25_uid_assoc {
struct ax25_uid_assoc *next;
uid_t uid;
ax25_address call;
} ax25_uid_assoc;
typedef struct {
ax25_address calls[AX25_MAX_DIGIS];
unsigned char repeated[AX25_MAX_DIGIS];
unsigned char ndigi;
char lastrepeat;
} ax25_digi;
typedef struct ax25_cb {
struct ax25_cb *next;
ax25_address source_addr, dest_addr;
struct device *device;
unsigned char dama_slave, iamdigi;
unsigned char state, modulus, pidincl;
unsigned short vs, vr, va;
unsigned char condition, backoff;
unsigned char n2, n2count;
unsigned short t1, t2, t3, idle, rtt;
unsigned short t1timer, t2timer, t3timer, idletimer;
unsigned short paclen;
unsigned short fragno, fraglen;
ax25_digi *digipeat;
struct sk_buff_head write_queue;
struct sk_buff_head reseq_queue;
struct sk_buff_head ack_queue;
struct sk_buff_head frag_queue;
unsigned char window;
struct timer_list timer;
struct sock *sk;
} ax25_cb;
#ifndef _LINUX_SYSCTL_H
#include <linux/sysctl.h>
#endif
struct ax25_dev {
char name[20];
struct device *dev;
struct device *forward;
struct ctl_table systable[AX25_MAX_VALUES+1];
int values[AX25_MAX_VALUES];
};
extern ax25_address null_ax25_address;
extern char *ax2asc(ax25_address *);
extern ax25_address *asc2ax(char *);
extern int ax25cmp(ax25_address *, ax25_address *);
extern ax25_cb *ax25_send_frame(struct sk_buff *, int, ax25_address *, ax25_address *, ax25_digi *, struct device *);
extern ax25_cb *ax25_find_cb(ax25_address *, ax25_address *, ax25_digi *, struct device *);
extern void ax25_destroy_socket(ax25_cb *);
extern struct device *ax25rtr_get_dev(ax25_address *);
extern int ax25_encapsulate(struct sk_buff *, struct device *, unsigned short,
void *, void *, unsigned int);
extern int ax25_rebuild_header(void *, struct device *, unsigned long, struct sk_buff *);
extern ax25_uid_assoc *ax25_uid_list;
extern int ax25_uid_policy;
extern ax25_address *ax25_findbyuid(uid_t);
extern void ax25_queue_xmit(struct sk_buff *, struct device *, int);
extern int ax25_dev_is_dama_slave(struct device *);
#include <net/ax25call.h>
extern int ax25_process_rx_frame(ax25_cb *, struct sk_buff *, int, int);
extern void ax25_output(ax25_cb *, int, struct sk_buff *);
extern void ax25_kick(ax25_cb *);
extern void ax25_transmit_buffer(ax25_cb *, struct sk_buff *, int);
extern void ax25_nr_error_recovery(ax25_cb *);
extern void ax25_establish_data_link(ax25_cb *);
extern void ax25_transmit_enquiry(ax25_cb *);
extern void ax25_enquiry_response(ax25_cb *);
extern void ax25_timeout_response(ax25_cb *);
extern void ax25_check_iframes_acked(ax25_cb *, unsigned short);
extern void dama_enquiry_response(ax25_cb *);
extern void dama_establish_data_link(ax25_cb *);
extern struct ax25_dev ax25_device[];
extern int ax25_rt_get_info(char *, char **, off_t, int, int);
extern int ax25_cs_get_info(char *, char **, off_t, int, int);
extern int ax25_rt_autobind(ax25_cb *, ax25_address *);
extern ax25_digi *ax25_rt_find_path(ax25_address *, struct device *);
extern void ax25_rt_build_path(struct sk_buff *, ax25_address *, ax25_address *, ax25_digi *);
extern void ax25_rt_device_down(struct device *);
extern int ax25_rt_ioctl(unsigned int, void *);
extern char ax25_rt_mode_get(ax25_address *, struct device *);
extern int ax25_dev_get_value(struct device *, int);
extern void ax25_dev_device_up(struct device *);
extern void ax25_dev_device_down(struct device *);
extern int ax25_fwd_ioctl(unsigned int, struct ax25_fwd_struct *);
extern struct device *ax25_fwd_dev(struct device *);
extern void ax25_rt_free(void);
extern void ax25_clear_queues(ax25_cb *);
extern void ax25_frames_acked(ax25_cb *, unsigned short);
extern void ax25_requeue_frames(ax25_cb *);
extern int ax25_validate_nr(ax25_cb *, unsigned short);
extern int ax25_decode(ax25_cb *, struct sk_buff *, int *, int *, int *);
extern void ax25_send_control(ax25_cb *, int, int, int);
extern unsigned short ax25_calculate_t1(ax25_cb *);
extern void ax25_calculate_rtt(ax25_cb *);
extern unsigned char *ax25_parse_addr(unsigned char *, int, ax25_address *,
ax25_address *, ax25_digi *, int *, int *);
extern int build_ax25_addr(unsigned char *, ax25_address *, ax25_address *,
ax25_digi *, int, int);
extern int size_ax25_addr(ax25_digi *);
extern void ax25_digi_invert(ax25_digi *, ax25_digi *);
extern void ax25_return_dm(struct device *, ax25_address *, ax25_address *, ax25_digi *);
extern void ax25_dama_on(ax25_cb *);
extern void ax25_dama_off(ax25_cb *);
extern void ax25_disconnect(ax25_cb *, int);
extern void ax25_set_timer(ax25_cb *);
extern void ax25_t1_timeout(ax25_cb *);
extern void ax25_link_failed(ax25_cb *, int);
extern int (*ax25_protocol_function(unsigned int))(struct sk_buff *, ax25_cb *);
extern int ax25_listen_mine(ax25_address *, struct device *);
extern void ax25_register_sysctl(void);
extern void ax25_unregister_sysctl(void);
extern ax25_cb *volatile ax25_list;
extern int ax25_protocol_register(unsigned int, int (*)(struct sk_buff *, ax25_cb *));
extern void ax25_protocol_release(unsigned int);
extern int ax25_linkfail_register(void (*)(ax25_cb *, int));
extern void ax25_linkfail_release(void (*)(ax25_cb *, int));
extern int ax25_listen_register(ax25_address *, struct device *);
extern void ax25_listen_release(ax25_address *, struct device *);
extern int ax25_protocol_is_registered(unsigned int);
#endif