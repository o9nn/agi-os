#ifndef	_SDLA_FR_H
#define	_SDLA_FR_H
#ifndef	PACKED
#    define	PACKED	__attribute__((packed))
#endif
#define	FR_MB_VECTOR	0xE000
#define	FR502_RX_VECTOR	0xA000
#define	FR502_MBOX_OFFS	0xF60
#define	FR508_MBOX_OFFS	0
#define	FR502_FLAG_OFFS	0x1FF0
#define	FR508_FLAG_OFFS	0x1000
#define	FR502_RXMB_OFFS	0x900
#define	FR508_TXBC_OFFS	0x1100
#define	FR508_RXBC_OFFS	0x1120
#define FR502_MAX_DATA	4096
#define FR508_MAX_DATA	4080
#define MIN_LGTH_FR_DATA_CFG         300
#define FR_MAX_NO_DATA_BYTES_IN_FRAME  15354
#define HIGHEST_VALID_DLCI	991
typedef struct fr_cmd
{
unsigned char  command	PACKED;
unsigned short length	PACKED;
unsigned char  result	PACKED;
unsigned short dlci	PACKED;
unsigned char  attr	PACKED;
unsigned short rxlost1	PACKED;
unsigned long  rxlost2	PACKED;
unsigned char  rsrv[2]	PACKED;
} fr_cmd_t;
#define	FR_WRITE		0x01
#define	FR_READ			0x02
#define	FR_ISSUE_IS_FRAME	0x03
#define FR_SET_CONFIG		0x10
#define FR_READ_CONFIG		0x11
#define FR_COMM_DISABLE		0x12
#define FR_COMM_ENABLE		0x13
#define FR_READ_STATUS		0x14
#define FR_READ_STATISTICS	0x15
#define FR_FLUSH_STATISTICS	0x16
#define	FR_LIST_ACTIVE_DLCI	0x17
#define FR_FLUSH_DATA_BUFFERS	0x18
#define FR_READ_ADD_DLC_STATS	0x19
#define	FR_ADD_DLCI		0x20
#define	FR_DELETE_DLCI		0x21
#define	FR_ACTIVATE_DLCI	0x22
#define	FR_DEACTIVATE_DLCI	0x22
#define FR_READ_MODEM_STATUS	0x30
#define FR_SET_MODEM_STATUS	0x31
#define FR_READ_ERROR_STATS	0x32
#define FR_FLUSH_ERROR_STATS	0x33
#define FR_READ_DLCI_IB_MAPPING 0x34
#define FR_READ_CODE_VERSION	0x40
#define	FR_SET_INTR_MODE	0x50
#define	FR_READ_INTR_MODE	0x51
#define FR_SET_TRACE_CONFIG	0x60
#define FR_FT1_STATUS_CTRL 	0x80
#define FR_SET_FT1_MODE		0x81
#define FPIPE_ENABLE_TRACING          	0x41
#define FPIPE_DISABLE_TRACING		0x42
#define FPIPE_GET_TRACE_INFO            0x43
#define FPIPE_FT1_READ_STATUS           0x44
#define FPIPE_DRIVER_STAT_IFSEND        0x45
#define FPIPE_DRIVER_STAT_INTR          0x46
#define FPIPE_DRIVER_STAT_GEN           0x47
#define FPIPE_FLUSH_DRIVER_STATS        0x48
#define FPIPE_ROUTER_UP_TIME            0x49
#define FRRES_OK		0x00
#define	FRRES_DISABLED		0x01
#define	FRRES_INOPERATIVE	0x02
#define	FRRES_DLCI_INACTIVE	0x03
#define	FRRES_DLCI_INVALID	0x04
#define	FRRES_TOO_LONG		0x05
#define	FRRES_TOO_MANY		0x06
#define	FRRES_CIR_OVERFLOW	0x07
#define	FRRES_BUFFER_OVERFLOW	0x08
#define	FRRES_MODEM_FAILURE	0x10
#define	FRRES_CHANNEL_DOWN	0x11
#define	FRRES_CHANNEL_UP	0x12
#define	FRRES_DLCI_CHANGE	0x13
#define	FRRES_DLCI_MISMATCH	0x14
#define	FRRES_INVALID_CMD	0x1F
#define	FRATTR_
typedef struct fr_mbox
{
unsigned char opflag	PACKED;
fr_cmd_t cmd		PACKED;
unsigned char data[1]	PACKED;
} fr_mbox_t;
typedef struct	fr502_flags
{
unsigned char rsrv1[1]	PACKED;
unsigned char tx_ready	PACKED;
unsigned char rx_ready	PACKED;
unsigned char event	PACKED;
unsigned char mstatus	PACKED;
unsigned char rsrv2[8]	PACKED;
unsigned char iflag	PACKED;
unsigned char imask	PACKED;
} fr502_flags_t;
typedef struct	fr508_flags
{
unsigned char rsrv1[3]	PACKED;
unsigned char event	PACKED;
unsigned char mstatus	PACKED;
unsigned char rsrv2[11]	PACKED;
unsigned char iflag	PACKED;
unsigned char imask	PACKED;
unsigned long tse_offs	PACKED;
unsigned short dlci	PACKED;
} fr508_flags_t;
#define	FR_EVENT_STATUS		0x01
#define	FR_EVENT_DLC_STATUS	0x02
#define	FR_EVENT_BAD_DLCI	0x04
#define	FR_EVENT_LINK_DOWN	0x40
#define	FR_MDM_DCD		0x08
#define	FR_MDM_CTS		0x20
#define	FR_INTR_RXRDY		0x01
#define	FR_INTR_TXRDY		0x02
#define	FR_INTR_MODEM		0x04
#define	FR_INTR_READY		0x08
#define	FR_INTR_DLC		0x10
#define	FR_INTR_TIMER		0x20
#define FR_INTR_TX_MULT_DLCIs	0x80
typedef struct	fr_buf_info
{
unsigned short rse_num	PACKED;
unsigned long rse_base	PACKED;
unsigned long rse_next	PACKED;
unsigned long buf_base	PACKED;
unsigned short reserved	PACKED;
unsigned long buf_top	PACKED;
} fr_buf_info_t;
typedef struct	fr_rx_buf_ctl
{
unsigned char flag	PACKED;
unsigned short length	PACKED;
unsigned short dlci	PACKED;
unsigned char attr	PACKED;
unsigned short tmstamp	PACKED;
unsigned short rsrv[2]	PACKED;
unsigned long offset	PACKED;
} fr_rx_buf_ctl_t;
typedef struct  fr_tx_buf_ctl
{
unsigned char flag      PACKED;
unsigned short rsrv0[2]	PACKED;
unsigned short length   PACKED;
unsigned short dlci     PACKED;
unsigned char attr      PACKED;
unsigned short rsrv1 	PACKED;
unsigned long offset    PACKED;
} fr_tx_buf_ctl_t;
typedef struct	fr_conf
{
unsigned short station	PACKED;
unsigned short options	PACKED;
unsigned short kbps	PACKED;
unsigned short port	PACKED;
unsigned short mtu	PACKED;
unsigned short t391	PACKED;
unsigned short t392	PACKED;
unsigned short n391	PACKED;
unsigned short n392	PACKED;
unsigned short n393	PACKED;
unsigned short cir_fwd	PACKED;
unsigned short bc_fwd	PACKED;
unsigned short be_fwd	PACKED;
unsigned short cir_bwd	PACKED;
unsigned short bc_bwd	PACKED;
unsigned short be_bwd	PACKED;
unsigned short dlci[0]	PACKED;
} fr_conf_t;
#define	FRCFG_STATION_CPE	0
#define	FRCFG_STATION_NODE	1
#define	FRCFG_IGNORE_TX_CIR	0x0001
#define	FRCFG_IGNORE_RX_CIR	0x0002
#define	FRCFG_DONT_RETRANSMIT	0x0004
#define	FRCFG_IGNORE_CBS	0x0008
#define	FRCFG_THROUGHPUT	0x0010
#define	FRCFG_DIRECT_RX		0x0080
#define	FRCFG_AUTO_CONFIG	0x8000
#define	FRCFG_BAUD_1200		12
#define	FRCFG_BAUD_2400		24
#define	FRCFG_BAUD_4800		48
#define	FRCFG_BAUD_9600		96
#define	FRCFG_BAUD_19200	19
#define	FRCFG_BAUD_38400	38
#define	FRCFG_BAUD_56000	56
#define	FRCFG_BAUD_64000	64
#define	FRCFG_BAUD_128000	128
#define	FRCFG_MODE_EXT_CLK	0x0000
#define	FRCFG_MODE_INT_CLK	0x0001
#define	FRCFG_MODE_V35		0x0000
#define	FRCFG_MODE_RS232	0x0002
typedef struct {
unsigned char flag      PACKED;
unsigned short length   PACKED;
unsigned char rsrv0[2]  PACKED;
unsigned char attr      PACKED;
unsigned short tmstamp  PACKED;
unsigned char rsrv1[4]  PACKED;
unsigned long offset    PACKED;
} fr_trc_el_t;
typedef struct {
unsigned char status    	PACKED;
unsigned char data_passed	PACKED;
unsigned short length   	PACKED;
unsigned short tmstamp  	PACKED;
} fpipemon_trc_hdr_t;
typedef struct {
fpipemon_trc_hdr_t fpipemon_trc_hdr			PACKED;
unsigned char data[FR_MAX_NO_DATA_BYTES_IN_FRAME]	PACKED;
} fpipemon_trc_t;
#define TRC_OUTGOING_FRM	0x01
#define TRC_ABORT_ERROR         0x10
#define TRC_CRC_ERROR           0x20
#define TRC_OVERRUN_ERROR       0x40
#define MORE_TRC_DATA		0x80
#define MAX_FRMS_TRACED		0x07
#define NO_TRC_ELEMENTS_OFF		0x9000
#define BASE_TRC_ELEMENTS_OFF		0x9002
#define TRC_ACTIVE			0x01
#define FLUSH_TRC_BUFFERS 		0x02
#define FLUSH_TRC_STATISTICS		0x04
#define TRC_SIGNALLING_FRMS		0x10
#define TRC_INFO_FRMS			0x20
#define ACTIVATE_TRC	(TRC_ACTIVE | TRC_SIGNALLING_FRMS | TRC_INFO_FRMS)
#define RESET_TRC	(FLUSH_TRC_BUFFERS | FLUSH_TRC_STATISTICS)
typedef struct	fr_dlc_conf
{
unsigned short conf_flags	PACKED;
unsigned short cir_fwd		PACKED;
unsigned short bc_fwd		PACKED;
unsigned short be_fwd		PACKED;
unsigned short cir_bwd		PACKED;
unsigned short bc_bwd		PACKED;
unsigned short be_bwd		PACKED;
} fr_dlc_conf_t;
typedef struct fr502_intr_ctl
{
unsigned char mode	PACKED;
unsigned short tx_len	PACKED;
} fr502_intr_ctl_t;
typedef struct fr508_intr_ctl
{
unsigned char mode	PACKED;
unsigned short tx_len	PACKED;
unsigned char irq	PACKED;
unsigned char flags	PACKED;
unsigned short timeout	PACKED;
} fr508_intr_ctl_t;
typedef struct	fr_dlc_Status
{
unsigned char status		PACKED;
struct
{
unsigned short dlci	PACKED;
unsigned char status	PACKED;
} circuit[1]			PACKED;
} fr_dlc_status_t;
#define	FR_LINK_INOPER	0x00
#define	FR_LINK_OPER	0x01
#define	FR_DLCI_DELETED	0x01
#define	FR_DLCI_ACTIVE	0x02
#define	FR_DLCI_WAITING	0x04
#define	FR_DLCI_NEW	0x08
#define	FR_DLCI_REPORT	0x40
typedef struct	fr_link_stat
{
unsigned short rx_too_long	PACKED;
unsigned short rx_dropped	PACKED;
unsigned short rx_dropped2	PACKED;
unsigned short rx_bad_dlci	PACKED;
unsigned short rx_bad_format	PACKED;
unsigned short retransmitted	PACKED;
unsigned short cpe_tx_FSE	PACKED;
unsigned short cpe_tx_LIV	PACKED;
unsigned short cpe_rx_FSR	PACKED;
unsigned short cpe_rx_LIV	PACKED;
unsigned short node_rx_FSE	PACKED;
unsigned short node_rx_LIV	PACKED;
unsigned short node_tx_FSR	PACKED;
unsigned short node_tx_LIV	PACKED;
unsigned short rx_ISF_err	PACKED;
unsigned short rx_unsolicited	PACKED;
unsigned short rx_SSN_err	PACKED;
unsigned short rx_RSN_err	PACKED;
unsigned short T391_timeouts	PACKED;
unsigned short T392_timeouts	PACKED;
unsigned short N392_reached	PACKED;
unsigned short cpe_SSN_RSN	PACKED;
unsigned short current_SSN	PACKED;
unsigned short current_RSN	PACKED;
unsigned short curreny_T391	PACKED;
unsigned short current_T392	PACKED;
unsigned short current_N392	PACKED;
unsigned short current_N393	PACKED;
} fr_link_stat_t;
typedef struct	fr_dlci_stat
{
unsigned long tx_frames		PACKED;
unsigned long tx_bytes		PACKED;
unsigned long rx_frames		PACKED;
unsigned long rx_bytes		PACKED;
unsigned long rx_dropped	PACKED;
unsigned long rx_inactive	PACKED;
unsigned long rx_exceed_CIR	PACKED;
unsigned long rx_DE_set		PACKED;
unsigned long tx_throughput	PACKED;
unsigned long tx_calc_timer	PACKED;
unsigned long rx_throughput	PACKED;
unsigned long rx_calc_timer	PACKED;
} fr_dlci_stat_t;
typedef struct	fr_comm_stat
{
unsigned char rx_overruns	PACKED;
unsigned char rx_bad_crc	PACKED;
unsigned char rx_aborts		PACKED;
unsigned char rx_too_long	PACKED;
unsigned char tx_aborts		PACKED;
unsigned char tx_underruns	PACKED;
unsigned char tx_missed_undr	PACKED;
unsigned char dcd_dropped	PACKED;
unsigned char cts_dropped	PACKED;
} fr_comm_stat_t;
#define	FR_ISF_LVE	2
#define	FR_ISF_FSE	3
typedef struct arphdr_fr
{
unsigned short ar_hrd PACKED;
unsigned short ar_pro PACKED;
unsigned char  ar_hln PACKED;
unsigned char  ar_pln PACKED;
unsigned short ar_op  PACKED;
unsigned short ar_sha PACKED;
unsigned long  ar_sip PACKED;
unsigned short ar_tha PACKED;
unsigned long  ar_tip PACKED;
} arphdr_fr_t;
typedef struct arphdr_1490
{
unsigned char control PACKED;
unsigned char pad     PACKED;
unsigned char NLPID   PACKED;
unsigned char OUI[3]  PACKED;
unsigned short PID    PACKED;
}  arphdr_1490_t;
typedef struct {
unsigned char  opp_flag PACKED;
unsigned char  command  PACKED;
unsigned short length   PACKED;
unsigned char  result   PACKED;
unsigned short dlci     PACKED;
unsigned char  attr     PACKED;
unsigned short rxlost1  PACKED;
unsigned long  rxlost2  PACKED;
unsigned char  rsrv[2]  PACKED;
} cblock_t;
typedef struct {
unsigned char   control                 PACKED;
unsigned char   NLPID                   PACKED;
} fr_encap_hdr_t;
typedef struct {
fr_encap_hdr_t 		fr_encap_hdr	PACKED;
ip_pkt_t 		ip_pkt		PACKED;
udp_pkt_t		udp_pkt		PACKED;
wp_mgmt_t 		wp_mgmt       	PACKED;
cblock_t                cblock          PACKED;
unsigned char           data[4080]      PACKED;
} fr_udp_pkt_t;
#define UDPMGMT_UDP_PROTOCOL 0x11
#define UDPMGMT_FPIPE_SIGNATURE         "FPIPE8ND"
#define UDPMGMT_DRVRSTATS_SIGNATURE     "DRVSTATS"
#define UDPMGMT_REQUEST	0x01
#define UDPMGMT_REPLY	0x02
#define UDP_OFFSET	12
typedef struct {
unsigned long if_send_entry;
unsigned long if_send_skb_null;
unsigned long if_send_broadcast;
unsigned long if_send_multicast;
unsigned long if_send_critical_ISR;
unsigned long if_send_critical_non_ISR;
unsigned long if_send_busy;
unsigned long if_send_busy_timeout;
unsigned long if_send_DRVSTATS_request;
unsigned long if_send_FPIPE_request;
unsigned long if_send_wan_disconnected;
unsigned long if_send_dlci_disconnected;
unsigned long if_send_no_bfrs;
unsigned long if_send_adptr_bfrs_full;
unsigned long if_send_bfrs_passed_to_adptr;
unsigned long if_send_consec_send_fail;
} drvstats_if_send_t;
typedef struct {
unsigned long rx_intr_no_socket;
unsigned long rx_intr_dev_not_started;
unsigned long rx_intr_DRVSTATS_request;
unsigned long rx_intr_FPIPE_request;
unsigned long rx_intr_bfr_not_passed_to_stack;
unsigned long rx_intr_bfr_passed_to_stack;
} drvstats_rx_intr_t;
typedef struct {
unsigned long UDP_FPIPE_mgmt_kmalloc_err;
unsigned long UDP_FPIPE_mgmt_direction_err;
unsigned long UDP_FPIPE_mgmt_adptr_type_err;
unsigned long UDP_FPIPE_mgmt_adptr_cmnd_OK;
unsigned long UDP_FPIPE_mgmt_adptr_cmnd_timeout;
unsigned long UDP_FPIPE_mgmt_adptr_send_passed;
unsigned long UDP_FPIPE_mgmt_adptr_send_failed;
unsigned long UDP_FPIPE_mgmt_not_passed_to_stack;
unsigned long UDP_FPIPE_mgmt_passed_to_stack;
unsigned long UDP_FPIPE_mgmt_no_socket;
unsigned long UDP_DRVSTATS_mgmt_kmalloc_err;
unsigned long UDP_DRVSTATS_mgmt_adptr_cmnd_OK;
unsigned long UDP_DRVSTATS_mgmt_adptr_cmnd_timeout;
unsigned long UDP_DRVSTATS_mgmt_adptr_send_passed;
unsigned long UDP_DRVSTATS_mgmt_adptr_send_failed;
unsigned long UDP_DRVSTATS_mgmt_not_passed_to_stack;
unsigned long UDP_DRVSTATS_mgmt_passed_to_stack;
unsigned long UDP_DRVSTATS_mgmt_no_socket;
} drvstats_gen_t;
typedef struct {
unsigned char   attr      	PACKED;
unsigned short  time_stamp      PACKED;
unsigned char   reserved[13]    PACKED;
} api_rx_hdr_t;
typedef struct {
api_rx_hdr_t    api_rx_hdr      PACKED;
void *          data            PACKED;
} api_rx_element_t;
typedef struct {
unsigned char   attr            PACKED;
unsigned char   reserved[15]    PACKED;
} api_tx_hdr_t;
typedef struct {
api_tx_hdr_t    api_tx_hdr      PACKED;
void *          data            PACKED;
} api_tx_element_t;
#ifdef		_MSC_
#  pragma	pack()
#endif
#endif