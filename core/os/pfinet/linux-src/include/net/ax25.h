#ifndef _AX25_H
#define _AX25_H
#include <linux/config.h>
#include <linux/ax25.h>
#define AX25_T1CLAMPLO 1
#define AX25_T1CLAMPHI (30 * HZ)
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
#define AX25_COND_DAMA_MODE 0x10
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
#define AX25_MODULUS 8
#define AX25_EMODULUS 128
enum {
AX25_PROTO_STD_SIMPLEX,
AX25_PROTO_STD_DUPLEX,
AX25_PROTO_DAMA_SLAVE,
AX25_PROTO_DAMA_MASTER
};
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
AX25_VALUES_PROTOCOL,
AX25_VALUES_DS_TIMEOUT,
AX25_MAX_VALUES
};
#define AX25_DEF_IPDEFMODE 0
#define AX25_DEF_AXDEFMODE 0
#define AX25_DEF_BACKOFF 1
#define AX25_DEF_CONMODE 2
#define AX25_DEF_WINDOW 2
#define AX25_DEF_EWINDOW 32
#define AX25_DEF_T1 (10 * HZ)
#define AX25_DEF_T2 (3 * HZ)
#define AX25_DEF_T3 (300 * HZ)
#define AX25_DEF_N2 10
#define AX25_DEF_IDLE (0 * 60 * HZ)
#define AX25_DEF_PACLEN 256
#define AX25_DEF_PROTOCOL AX25_PROTO_STD_SIMPLEX
#define AX25_DEF_DS_TIMEOUT (3 * 60 * HZ)
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
typedef struct ax25_route {
struct ax25_route *next;
ax25_address callsign;
struct device *dev;
ax25_digi *digipeat;
char ip_mode;
} ax25_route;
typedef struct {
char slave;
struct timer_list slave_timer;
unsigned short slave_timeout;
} ax25_dama_info;
#ifndef _LINUX_SYSCTL_H
#include <linux/sysctl.h>
#endif
typedef struct ax25_dev {
struct ax25_dev *next;
struct device *dev;
struct device *forward;
struct ctl_table systable[AX25_MAX_VALUES+1];
int values[AX25_MAX_VALUES];
#if defined(CONFIG_AX25_DAMA_SLAVE) || defined(CONFIG_AX25_DAMA_MASTER)
ax25_dama_info dama;
#endif
} ax25_dev;
typedef struct ax25_cb {
struct ax25_cb *next;
ax25_address source_addr, dest_addr;
ax25_digi *digipeat;
ax25_dev *ax25_dev;
unsigned char iamdigi;
unsigned char state, modulus, pidincl;
unsigned short vs, vr, va;
unsigned char condition, backoff;
unsigned char n2, n2count;
struct timer_list t1timer, t2timer, t3timer, idletimer;
unsigned long t1, t2, t3, idle, rtt;
unsigned short paclen, fragno, fraglen;
struct sk_buff_head write_queue;
struct sk_buff_head reseq_queue;
struct sk_buff_head ack_queue;
struct sk_buff_head frag_queue;
unsigned char window;
struct timer_list timer;
struct sock *sk;
} ax25_cb;
extern ax25_cb *volatile ax25_list;
extern void ax25_free_cb(ax25_cb *);
extern void ax25_insert_socket(ax25_cb *);
struct sock *ax25_find_listener(ax25_address *, int, struct device *, int);
struct sock *ax25_find_socket(ax25_address *, ax25_address *, int);
extern ax25_cb *ax25_find_cb(ax25_address *, ax25_address *, ax25_digi *, struct device *);
extern struct sock *ax25_addr_match(ax25_address *);
extern void ax25_send_to_raw(struct sock *, struct sk_buff *, int);
extern void ax25_destroy_socket(ax25_cb *);
extern ax25_cb *ax25_create_cb(void);
extern void ax25_fillin_cb(ax25_cb *, ax25_dev *);
extern int ax25_create(struct socket *, int);
extern struct sock *ax25_make_new(struct sock *, struct ax25_dev *);
extern ax25_address null_ax25_address;
extern char *ax2asc(ax25_address *);
extern ax25_address *asc2ax(char *);
extern int ax25cmp(ax25_address *, ax25_address *);
extern int ax25digicmp(ax25_digi *, ax25_digi *);
extern unsigned char *ax25_addr_parse(unsigned char *, int, ax25_address *, ax25_address *, ax25_digi *, int *, int *);
extern int ax25_addr_build(unsigned char *, ax25_address *, ax25_address *, ax25_digi *, int, int);
extern int ax25_addr_size(ax25_digi *);
extern void ax25_digi_invert(ax25_digi *, ax25_digi *);
extern ax25_dev *ax25_dev_list;
extern ax25_dev *ax25_dev_ax25dev(struct device *);
extern ax25_dev *ax25_addr_ax25dev(ax25_address *);
extern void ax25_dev_device_up(struct device *);
extern void ax25_dev_device_down(struct device *);
extern int ax25_fwd_ioctl(unsigned int, struct ax25_fwd_struct *);
extern struct device *ax25_fwd_dev(struct device *);
extern void ax25_dev_free(void);
extern int ax25_ds_frame_in(ax25_cb *, struct sk_buff *, int);
extern void ax25_ds_nr_error_recovery(ax25_cb *);
extern void ax25_ds_enquiry_response(ax25_cb *);
extern void ax25_ds_establish_data_link(ax25_cb *);
extern void ax25_dev_dama_on(ax25_dev *);
extern void ax25_dev_dama_off(ax25_dev *);
extern void ax25_dama_on(ax25_cb *);
extern void ax25_dama_off(ax25_cb *);
extern void ax25_ds_set_timer(ax25_dev *);
extern void ax25_ds_del_timer(ax25_dev *);
extern void ax25_ds_timer(ax25_cb *);
extern void ax25_ds_t1_timeout(ax25_cb *);
extern void ax25_ds_heartbeat_expiry(ax25_cb *);
extern void ax25_ds_t3timer_expiry(ax25_cb *);
extern void ax25_ds_idletimer_expiry(ax25_cb *);
#include <net/ax25call.h>
extern int ax25_protocol_register(unsigned int, int (*)(struct sk_buff *, ax25_cb *));
extern void ax25_protocol_release(unsigned int);
extern int ax25_linkfail_register(void (*)(ax25_cb *, int));
extern void ax25_linkfail_release(void (*)(ax25_cb *, int));
extern int ax25_listen_register(ax25_address *, struct device *);
extern void ax25_listen_release(ax25_address *, struct device *);
extern int (*ax25_protocol_function(unsigned int))(struct sk_buff *, ax25_cb *);
extern int ax25_listen_mine(ax25_address *, struct device *);
extern void ax25_link_failed(ax25_cb *, int);
extern int ax25_protocol_is_registered(unsigned int);
extern int ax25_rx_iframe(ax25_cb *, struct sk_buff *);
extern int ax25_kiss_rcv(struct sk_buff *, struct device *, struct packet_type *);
extern int ax25_encapsulate(struct sk_buff *, struct device *, unsigned short, void *, void *, unsigned int);
extern int ax25_rebuild_header(struct sk_buff *);
extern ax25_cb *ax25_send_frame(struct sk_buff *, int, ax25_address *, ax25_address *, ax25_digi *, struct device *);
extern void ax25_output(ax25_cb *, int, struct sk_buff *);
extern void ax25_kick(ax25_cb *);
extern void ax25_transmit_buffer(ax25_cb *, struct sk_buff *, int);
extern void ax25_queue_xmit(struct sk_buff *);
extern int ax25_check_iframes_acked(ax25_cb *, unsigned short);
extern void ax25_rt_device_down(struct device *);
extern int ax25_rt_ioctl(unsigned int, void *);
extern int ax25_rt_get_info(char *, char **, off_t, int, int);
extern int ax25_rt_autobind(ax25_cb *, ax25_address *);
extern ax25_route *ax25_rt_find_route(ax25_address *, struct device *);
extern struct sk_buff *ax25_rt_build_path(struct sk_buff *, ax25_address *, ax25_address *, ax25_digi *);
extern void ax25_rt_free(void);
extern int ax25_std_frame_in(ax25_cb *, struct sk_buff *, int);
extern void ax25_std_nr_error_recovery(ax25_cb *);
extern void ax25_std_establish_data_link(ax25_cb *);
extern void ax25_std_transmit_enquiry(ax25_cb *);
extern void ax25_std_enquiry_response(ax25_cb *);
extern void ax25_std_timeout_response(ax25_cb *);
extern void ax25_std_heartbeat_expiry(ax25_cb *);
extern void ax25_std_t1timer_expiry(ax25_cb *);
extern void ax25_std_t2timer_expiry(ax25_cb *);
extern void ax25_std_t3timer_expiry(ax25_cb *);
extern void ax25_std_idletimer_expiry(ax25_cb *);
extern void ax25_clear_queues(ax25_cb *);
extern void ax25_frames_acked(ax25_cb *, unsigned short);
extern void ax25_requeue_frames(ax25_cb *);
extern int ax25_validate_nr(ax25_cb *, unsigned short);
extern int ax25_decode(ax25_cb *, struct sk_buff *, int *, int *, int *);
extern void ax25_send_control(ax25_cb *, int, int, int);
extern void ax25_return_dm(struct device *, ax25_address *, ax25_address *, ax25_digi *);
extern void ax25_calculate_t1(ax25_cb *);
extern void ax25_calculate_rtt(ax25_cb *);
extern void ax25_disconnect(ax25_cb *, int);
extern void ax25_start_heartbeat(ax25_cb *);
extern void ax25_start_t1timer(ax25_cb *);
extern void ax25_start_t2timer(ax25_cb *);
extern void ax25_start_t3timer(ax25_cb *);
extern void ax25_start_idletimer(ax25_cb *);
extern void ax25_stop_heartbeat(ax25_cb *);
extern void ax25_stop_t1timer(ax25_cb *);
extern void ax25_stop_t2timer(ax25_cb *);
extern void ax25_stop_t3timer(ax25_cb *);
extern void ax25_stop_idletimer(ax25_cb *);
extern int ax25_t1timer_running(ax25_cb *);
extern unsigned long ax25_display_timer(struct timer_list *);
extern int ax25_uid_policy;
extern ax25_address *ax25_findbyuid(uid_t);
extern int ax25_uid_ioctl(int, struct sockaddr_ax25 *);
extern int ax25_uid_get_info(char *, char **, off_t, int, int);
extern void ax25_uid_free(void);
extern void ax25_register_sysctl(void);
extern void ax25_unregister_sysctl(void);
#endif