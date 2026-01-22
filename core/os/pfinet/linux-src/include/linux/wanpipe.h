#ifndef _WANPIPE_H
#define _WANPIPE_H
#ifdef __SMP__
#include <asm/spinlock.h>
#endif
#include <linux/wanrouter.h>
#ifndef PACKED
#define PACKED __attribute__((packed))
#endif
#define WANPIPE_MAGIC 0x414C4453L
#define WANPIPE_DUMP (ROUTER_USER+0)
#define WANPIPE_EXEC (ROUTER_USER+1)
#define TRACE_ALL 0x00
#define TRACE_PROT 0x01
#define TRACE_DATA 0x02
#define UDPMGMT_REQUEST 0x01
#define UDPMGMT_REPLY 0x02
#define UDP_OFFSET 12
typedef struct sdla_dump
{
unsigned long magic;
unsigned long offset;
unsigned long length;
void* ptr;
} sdla_dump_t;
typedef struct sdla_exec
{
unsigned long magic;
void* cmd;
void* data;
} sdla_exec_t;
typedef struct wum_header
{
unsigned char signature[8];
unsigned char type;
unsigned char command;
unsigned char reserved[6];
} wum_header_t;
typedef struct global_stats
{
unsigned long isr_entry;
unsigned long isr_already_critical;
unsigned long isr_rx;
unsigned long isr_tx;
unsigned long isr_intr_test;
unsigned long isr_spurious;
unsigned long isr_enable_tx_int;
unsigned long rx_intr_corrupt_rx_bfr;
unsigned long rx_intr_on_orphaned_DLCI;
unsigned long rx_intr_dev_not_started;
unsigned long tx_intr_dev_not_started;
unsigned long poll_entry;
unsigned long poll_already_critical;
unsigned long poll_processed;
unsigned long poll_tbusy_bad_status;
unsigned long poll_host_disable_irq;
unsigned long poll_host_enable_irq;
} global_stats_t;
typedef struct{
unsigned short udp_src_port PACKED;
unsigned short udp_dst_port PACKED;
unsigned short udp_length PACKED;
unsigned short udp_checksum PACKED;
} udp_pkt_t;
typedef struct {
unsigned char ver_inet_hdr_length PACKED;
unsigned char service_type PACKED;
unsigned short total_length PACKED;
unsigned short identifier PACKED;
unsigned short flags_frag_offset PACKED;
unsigned char ttl PACKED;
unsigned char protocol PACKED;
unsigned short hdr_checksum PACKED;
unsigned long ip_src_address PACKED;
unsigned long ip_dst_address PACKED;
} ip_pkt_t;
typedef struct {
unsigned char signature[8] PACKED;
unsigned char request_reply PACKED;
unsigned char id PACKED;
unsigned char reserved[6] PACKED;
} wp_mgmt_t;
typedef struct if_send_stat{
unsigned long if_send_entry;
unsigned long if_send_skb_null;
unsigned long if_send_broadcast;
unsigned long if_send_multicast;
unsigned long if_send_critical_ISR;
unsigned long if_send_critical_non_ISR;
unsigned long if_send_tbusy;
unsigned long if_send_tbusy_timeout;
unsigned long if_send_PIPE_request;
unsigned long if_send_wan_disconnected;
unsigned long if_send_dlci_disconnected;
unsigned long if_send_no_bfrs;
unsigned long if_send_adptr_bfrs_full;
unsigned long if_send_bfr_passed_to_adptr;
unsigned long if_send_protocol_error;
unsigned long if_send_bfr_not_passed_to_adptr;
unsigned long if_send_tx_int_enabled;
unsigned long if_send_consec_send_fail;
} if_send_stat_t;
typedef struct rx_intr_stat{
unsigned long rx_intr_no_socket;
unsigned long rx_intr_dev_not_started;
unsigned long rx_intr_PIPE_request;
unsigned long rx_intr_bfr_not_passed_to_stack;
unsigned long rx_intr_bfr_passed_to_stack;
} rx_intr_stat_t;
typedef struct pipe_mgmt_stat{
unsigned long UDP_PIPE_mgmt_kmalloc_err;
unsigned long UDP_PIPE_mgmt_direction_err;
unsigned long UDP_PIPE_mgmt_adptr_type_err;
unsigned long UDP_PIPE_mgmt_adptr_cmnd_OK;
unsigned long UDP_PIPE_mgmt_adptr_cmnd_timeout;
unsigned long UDP_PIPE_mgmt_adptr_send_passed;
unsigned long UDP_PIPE_mgmt_adptr_send_failed;
unsigned long UDP_PIPE_mgmt_not_passed_to_stack;
unsigned long UDP_PIPE_mgmt_passed_to_stack;
unsigned long UDP_PIPE_mgmt_no_socket;
unsigned long UDP_PIPE_mgmt_passed_to_adptr;
} pipe_mgmt_stat_t;
#define MAX_LGTH_UDP_MGNT_PKT 2000
#define INTR_TEST_MODE 0x02
#define WUM_SIGNATURE_L 0x50495046
#define WUM_SIGNATURE_H 0x444E3845
#define WUM_KILL 0x50
#define WUM_EXEC 0x51
#ifdef __KERNEL__
#include <linux/sdladrv.h>
#include <linux/sdlasfm.h>
#ifndef min
#define min(a,b) (((a)<(b))?(a):(b))
#endif
#ifndef max
#define max(a,b) (((a)>(b))?(a):(b))
#endif
#define is_digit(ch) (((ch)>=(unsigned)'0'&&(ch)<=(unsigned)'9')?1:0)
#define is_alpha(ch) ((((ch)>=(unsigned)'a'&&(ch)<=(unsigned)'z')||\
((ch)>=(unsigned)'A'&&(ch)<=(unsigned)'Z'))?1:0)
#define is_hex_digit(ch) ((((ch)>=(unsigned)'0'&&(ch)<=(unsigned)'9')||\
((ch)>=(unsigned)'a'&&(ch)<=(unsigned)'f')||\
((ch)>=(unsigned)'A'&&(ch)<=(unsigned)'F'))?1:0)
typedef struct sdla
{
char devname[WAN_DRVNAME_SZ+1];
sdlahw_t hw;
wan_device_t wandev;
unsigned open_cnt;
unsigned long state_tick;
unsigned intr_mode;
char in_isr;
char buff_int_mode_unbusy;
char dlci_int_mode_unbusy;
char configured;
unsigned short irq_dis_if_send_count;
unsigned short irq_dis_poll_count;
unsigned short force_enable_irq;
char TracingEnabled;
global_stats_t statistics;
#ifdef __SMP__
spinlock_t lock;
#endif
void* mbox;
void* rxmb;
void* flags;
void (*isr)(struct sdla* card);
void (*poll)(struct sdla* card);
int (*exec)(struct sdla* card, void* u_cmd, void* u_data);
struct sdla *next;
union
{
struct
{
unsigned lo_pvc;
unsigned hi_pvc;
unsigned lo_svc;
unsigned hi_svc;
} x;
struct
{
void* rxmb_base;
void* rxmb_last;
unsigned rx_base;
unsigned rx_top;
unsigned short node_dlci[100];
unsigned short dlci_num;
struct device *dlci_to_dev_map[991 + 1];
unsigned tx_interrupts_pending;
unsigned short timer_int_enabled;
unsigned short udp_pkt_lgth;
int udp_type;
char udp_pkt_src;
unsigned udp_dlci;
char udp_pkt_data[MAX_LGTH_UDP_MGNT_PKT];
void* trc_el_base;
void* trc_el_last;
void *curr_trc_el;
unsigned short trc_bfr_space;
unsigned char update_comms_stats;
} f;
struct
{
char if_name[WAN_IFNAME_SZ+1];
void* txbuf;
void* txbuf_base;
void* txbuf_last;
void* rxbuf_base;
void* rxbuf_last;
unsigned rx_base;
unsigned rx_top;
char ip_mode;
char authenticator;
} p;
struct
{
char if_name[WAN_IFNAME_SZ+1];
unsigned char comm_port;
unsigned char usedby;
void* rxmb;
void* flags;
void* tx_status;
void* rx_status;
void* txbuf;
void* txbuf_base;
void* txbuf_last;
void* rxbuf_base;
void* rxbuf_last;
unsigned rx_base;
unsigned rx_top;
unsigned short protocol_options;
unsigned short kpalv_tx;
unsigned short kpalv_rx;
unsigned short kpalv_err;
unsigned short slarp_timer;
unsigned state;
unsigned char api_status;
unsigned char update_call_count;
} c;
struct
{
void* tx_status;
void* rx_status;
void* trace_status;
void* txbuf;
void* txbuf_base;
void* txbuf_last;
void* rxbuf_base;
void* rxbuf_last;
void* tracebuf;
void* tracebuf_base;
void* tracebuf_last;
unsigned rx_base;
unsigned rx_end;
unsigned trace_base;
unsigned trace_end;
} h;
} u;
} sdla_t;
void wanpipe_open (sdla_t* card);
void wanpipe_close (sdla_t* card);
void wanpipe_set_state (sdla_t* card, int state);
int wpx_init (sdla_t* card, wandev_conf_t* conf);
int wpf_init (sdla_t* card, wandev_conf_t* conf);
int wpp_init (sdla_t* card, wandev_conf_t* conf);
int wpc_init (sdla_t* card, wandev_conf_t* conf);
int bsc_init (sdla_t* card, wandev_conf_t* conf);
int hdlc_init(sdla_t* card, wandev_conf_t* conf);
int wpft1_init (sdla_t* card, wandev_conf_t* conf);
#endif
#endif