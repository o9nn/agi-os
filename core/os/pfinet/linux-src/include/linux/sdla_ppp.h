#ifndef	_SDLA_PPP_H
#define	_SDLA_PPP_H
#ifndef	PACKED
#    define	PACKED	__attribute__((packed))
#endif
#define	PPP508_MB_VECT	0xE000
#define	PPP508_MB_OFFS	0
#define	PPP508_FLG_OFFS	0x1000
#define	PPP508_BUF_OFFS	0x1100
#define PPP514_MB_OFFS  0xE000
#define PPP514_FLG_OFFS 0xF000
#define PPP514_BUF_OFFS 0xF100
#define PPP_MAX_DATA	1008
typedef struct ppp_cmd{
unsigned char  command	PACKED;
unsigned short length	PACKED;
unsigned char  result	PACKED;
unsigned char  rsrv[11]	PACKED;
} ppp_cmd_t;
typedef struct cblock{
unsigned char  opp_flag	PACKED;
unsigned char  command	PACKED;
unsigned short length	PACKED;
unsigned char  result	PACKED;
unsigned char  rsrv[11]	PACKED;
} cblock_t;
typedef struct ppp_udp_pkt{
ip_pkt_t 	ip_pkt	PACKED;
udp_pkt_t	udp_pkt	PACKED;
wp_mgmt_t	wp_mgmt PACKED;
cblock_t	cblock  PACKED;
unsigned char   data[MAX_LGTH_UDP_MGNT_PKT] PACKED;
} ppp_udp_pkt_t;
typedef struct {
unsigned char	status		PACKED;
unsigned char	data_avail	PACKED;
unsigned short	real_length	PACKED;
unsigned short	time_stamp	PACKED;
unsigned char	data[1]		PACKED;
} trace_pkt_t;
typedef struct {
unsigned char 	opp_flag	PACKED;
unsigned char	trace_type	PACKED;
unsigned short 	trace_length	PACKED;
unsigned short 	trace_data_ptr	PACKED;
unsigned short  trace_time_stamp PACKED;
} trace_element_t;
#define PPP_READ_CODE_VERSION	0x10
#define PPP_SET_CONFIG		0x05
#define PPP_READ_CONFIG		0x06
#define	PPP_SET_INTR_FLAGS	0x20
#define	PPP_READ_INTR_FLAGS	0x21
#define	PPP_SET_INBOUND_AUTH	0x30
#define	PPP_SET_OUTBOUND_AUTH	0x31
#define	PPP_GET_CONNECTION_INFO	0x32
#define PPP_COMM_ENABLE		0x03
#define PPP_COMM_DISABLE	0x04
#define	PPP_SEND_SIGN_FRAME	0x23
#define	PPP_READ_SIGN_RESPONSE	0x24
#define	PPP_DATALINE_MONITOR	0x33
#define PPP_READ_STATISTICS	0x07
#define PPP_FLUSH_STATISTICS	0x08
#define PPP_READ_ERROR_STATS	0x09
#define PPP_FLUSH_ERROR_STATS	0x0A
#define PPP_READ_PACKET_STATS	0x12
#define PPP_FLUSH_PACKET_STATS	0x13
#define PPP_READ_LCP_STATS	0x14
#define PPP_FLUSH_LCP_STATS	0x15
#define PPP_READ_LPBK_STATS	0x16
#define PPP_FLUSH_LPBK_STATS	0x17
#define PPP_READ_IPCP_STATS	0x18
#define PPP_FLUSH_IPCP_STATS	0x19
#define PPP_READ_IPXCP_STATS	0x1A
#define PPP_FLUSH_IPXCP_STATS	0x1B
#define PPP_READ_PAP_STATS	0x1C
#define PPP_FLUSH_PAP_STATS	0x1D
#define PPP_READ_CHAP_STATS	0x1E
#define PPP_FLUSH_CHAP_STATS	0x1F
#define PPPRES_OK		0x00
#define	PPPRES_INVALID_STATE	0x09
typedef struct ppp_mbox
{
unsigned char flag	PACKED;
ppp_cmd_t     cmd	PACKED;
unsigned char data[1]	PACKED;
} ppp_mbox_t;
typedef struct	ppp_flags
{
unsigned char iflag		PACKED;
unsigned char imask		PACKED;
unsigned char resrv		PACKED;
unsigned char mstatus		PACKED;
unsigned char lcp_state		PACKED;
unsigned char ppp_phase		PACKED;
unsigned char ip_state		PACKED;
unsigned char ipx_state		PACKED;
unsigned char pap_state		PACKED;
unsigned char chap_state	PACKED;
unsigned short disc_cause	PACKED;
} ppp_flags_t;
#define	PPP_INTR_RXRDY		0x01
#define	PPP_INTR_TXRDY		0x02
#define	PPP_INTR_MODEM		0x04
#define	PPP_INTR_CMD		0x08
#define	PPP_INTR_DISC		0x10
#define	PPP_INTR_OPEN		0x20
#define	PPP_INTR_DROP_DTR	0x40
#define PPP_INTR_TIMER          0x80
#define	PPP_MDM_DCD		0x08
#define	PPP_MDM_CTS		0x20
#define PPP_LOCAL_TERMINATION   0x0001
#define PPP_DCD_CTS_DROP        0x0002
#define PPP_REMOTE_TERMINATION	0x0800
#define DONT_RE_TX_ABORTED_I_FRAMES 	0x01
#define TX_FRM_BYTE_COUNT_STATS         0x02
#define RX_FRM_BYTE_COUNT_STATS         0x04
#define TIME_STAMP_IN_RX_FRAMES         0x08
#define NON_STD_ADPTR_FREQ              0x10
#define INTERFACE_LEVEL_RS232           0x20
#define AUTO_LINK_RECOVERY              0x100
#define DONT_TERMINATE_LNK_MAX_CONFIG   0x200
#define NO_AUTHENTICATION	0x00
#define INBOUND_AUTH		0x80
#define PAP_AUTH		0x01
#define CHAP_AUTH		0x02
#define L_AND_R_IP_NO_ASSIG	0x00
#define L_IP_LOCAL_ASSIG    	0x01
#define L_IP_REMOTE_ASSIG   	0x02
#define R_IP_LOCAL_ASSIG        0x04
#define R_IP_REMOTE_ASSIG       0x08
#define ENABLE_IP		0x80
#define ROUTING_PROT_DEFAULT    0x20
#define ENABLE_IPX		0x80
#define DISABLE_IPX		0x00
typedef struct	ppp508_buf_info
{
unsigned short txb_num	PACKED;
unsigned long  txb_ptr	PACKED;
unsigned char  rsrv1[26] PACKED;
unsigned short rxb_num	PACKED;
unsigned long  rxb_ptr	PACKED;
unsigned long  rxb1_ptr	PACKED;
unsigned long  rxb_base	PACKED;
unsigned char  rsrv2[2]	PACKED;
unsigned long  rxb_end	PACKED;
} ppp508_buf_info_t;
typedef struct	ppp_buf_ctl
{
unsigned char  flag		PACKED;
unsigned short length		PACKED;
unsigned char  reserved1[1]	PACKED;
unsigned char  proto		PACKED;
unsigned short timestamp	PACKED;
unsigned char  reserved2[5]	PACKED;
union
{
unsigned short o_p[2];
unsigned long  ptr;
} buf				PACKED;
} ppp_buf_ctl_t;
typedef struct	ppp508_conf
{
unsigned long  line_speed	PACKED;
unsigned short txbuf_percent	PACKED;
unsigned short conf_flags	PACKED;
unsigned short mtu_local	PACKED;
unsigned short mtu_remote	PACKED;
unsigned short restart_tmr	PACKED;
unsigned short auth_rsrt_tmr	PACKED;
unsigned short auth_wait_tmr	PACKED;
unsigned short mdm_fail_tmr	PACKED;
unsigned short dtr_drop_tmr	PACKED;
unsigned short connect_tmout	PACKED;
unsigned short conf_retry	PACKED;
unsigned short term_retry	PACKED;
unsigned short fail_retry	PACKED;
unsigned short auth_retry	PACKED;
unsigned char  auth_options	PACKED;
unsigned char  ip_options	PACKED;
unsigned long  ip_local		PACKED;
unsigned long  ip_remote	PACKED;
unsigned char  ipx_options	PACKED;
unsigned char  ipx_netno[4]	PACKED;
unsigned char  ipx_local[6]	PACKED;
unsigned char  ipx_remote[6]	PACKED;
unsigned char  ipx_router[48]	PACKED;
unsigned long  alt_cpu_clock	PACKED;
} ppp508_conf_t;
typedef struct	ppp508_connect_info
{
unsigned short 	mru		PACKED;
unsigned char  	ip_options 	PACKED;
unsigned long  	ip_local	PACKED;
unsigned long  	ip_remote	PACKED;
unsigned char	ipx_options	PACKED;
unsigned char  	ipx_netno[4]	PACKED;
unsigned char  	ipx_local[6]	PACKED;
unsigned char  	ipx_remote[6]	PACKED;
unsigned char  	ipx_router[48]	PACKED;
unsigned char	auth_status	PACKED;
unsigned char 	inbd_auth_peerID[1] PACKED;
} ppp508_connect_info_t;
#define	PPP_BITRATE_1200	0x01
#define	PPP_BITRATE_2400	0x02
#define	PPP_BITRATE_4800	0x03
#define	PPP_BITRATE_9600	0x04
#define	PPP_BITRATE_19200	0x05
#define	PPP_BITRATE_38400	0x06
#define	PPP_BITRATE_45000	0x07
#define	PPP_BITRATE_56000	0x08
#define	PPP_BITRATE_64000	0x09
#define	PPP_BITRATE_74000	0x0A
#define	PPP_BITRATE_112000	0x0B
#define	PPP_BITRATE_128000	0x0C
#define	PPP_BITRATE_156000	0x0D
#define	PPP_IGNORE_TX_ABORT	0x01
#define	PPP_ENABLE_TX_STATS	0x02
#define	PPP_ENABLE_RX_STATS	0x04
#define	PPP_ENABLE_TIMESTAMP	0x08
#define	PPP_LOCAL_IP_LOCAL	0x01
#define	PPP_LOCAL_IP_REMOTE	0x02
#define	PPP_REMOTE_IP_LOCAL	0x04
#define	PPP_REMOTE_IP_REMOTE	0x08
#define	PPP_REMOTE_IPX_NETNO	0x01
#define	PPP_REMOTE_IPX_LOCAL	0x02
#define	PPP_REMOTE_IPX_REMOTE	0x04
#define	PPP_IPX_ROUTE_RIP_SAP	0x08
#define	PPP_IPX_ROUTE_NLSP	0x10
#define	PPP_IPX_ROUTE_DEFAULT	0x20
#define	PPP_IPX_CONF_COMPLETE	0x40
#define	PPP_IPX_ENABLE		0x80
typedef struct	ppp508_get_conf
{
unsigned long  bps	PACKED;
ppp508_conf_t  conf	PACKED;
unsigned short txb_num	PACKED;
unsigned short rxb_num	PACKED;
} ppp508_get_conf_t;
typedef struct ppp508_stats
{
unsigned short reserved1	PACKED;
unsigned short rx_bad_len	PACKED;
unsigned short reserved2	PACKED;
unsigned long  tx_frames	PACKED;
unsigned long  tx_bytes	PACKED;
unsigned long  rx_frames	PACKED;
unsigned long  rx_bytes	PACKED;
} ppp508_stats_t;
typedef struct	ppp_err_stats
{
unsigned char	 rx_overrun	PACKED;
unsigned char	 rx_bad_crc	PACKED;
unsigned char	 rx_abort	PACKED;
unsigned char	 rx_lost	PACKED;
unsigned char	 tx_abort	PACKED;
unsigned char	 tx_underrun	PACKED;
unsigned char	 tx_missed_intr	PACKED;
unsigned char	 reserved	PACKED;
unsigned char	 dcd_trans	PACKED;
unsigned char	 cts_trans	PACKED;
} ppp_err_stats_t;
typedef struct	ppp_pkt_stats
{
unsigned short rx_bad_header	PACKED;
unsigned short rx_prot_unknwn	PACKED;
unsigned short rx_too_large	PACKED;
unsigned short rx_lcp		PACKED;
unsigned short tx_lcp		PACKED;
unsigned short rx_ipcp		PACKED;
unsigned short tx_ipcp		PACKED;
unsigned short rx_ipxcp		PACKED;
unsigned short tx_ipxcp		PACKED;
unsigned short rx_pap		PACKED;
unsigned short tx_pap		PACKED;
unsigned short rx_chap		PACKED;
unsigned short tx_chap		PACKED;
unsigned short rx_lqr		PACKED;
unsigned short tx_lqr		PACKED;
unsigned short rx_ip		PACKED;
unsigned short tx_ip		PACKED;
unsigned short rx_ipx		PACKED;
unsigned short tx_ipx		PACKED;
} ppp_pkt_stats_t;
typedef struct	ppp_lcp_stats
{
unsigned short rx_unknown	PACKED;
unsigned short rx_conf_rqst	PACKED;
unsigned short rx_conf_ack	PACKED;
unsigned short rx_conf_nak	PACKED;
unsigned short rx_conf_rej	PACKED;
unsigned short rx_term_rqst	PACKED;
unsigned short rx_term_ack	PACKED;
unsigned short rx_code_rej	PACKED;
unsigned short rx_proto_rej	PACKED;
unsigned short rx_echo_rqst	PACKED;
unsigned short rx_echo_reply	PACKED;
unsigned short rx_disc_rqst	PACKED;
unsigned short tx_conf_rqst	PACKED;
unsigned short tx_conf_ack	PACKED;
unsigned short tx_conf_nak	PACKED;
unsigned short tx_conf_rej	PACKED;
unsigned short tx_term_rqst	PACKED;
unsigned short tx_term_ack	PACKED;
unsigned short tx_code_rej	PACKED;
unsigned short tx_proto_rej	PACKED;
unsigned short tx_echo_rqst	PACKED;
unsigned short tx_echo_reply	PACKED;
unsigned short tx_disc_rqst	PACKED;
unsigned short rx_too_large	PACKED;
unsigned short rx_ack_inval	PACKED;
unsigned short rx_rej_inval	PACKED;
unsigned short rx_rej_badid	PACKED;
} ppp_lcp_stats_t;
typedef struct	ppp_lpbk_stats
{
unsigned short conf_magic	PACKED;
unsigned short loc_echo_rqst	PACKED;
unsigned short rem_echo_rqst	PACKED;
unsigned short loc_echo_reply	PACKED;
unsigned short rem_echo_reply	PACKED;
unsigned short loc_disc_rqst	PACKED;
unsigned short rem_disc_rqst	PACKED;
unsigned short echo_tx_collsn	PACKED;
unsigned short echo_rx_collsn	PACKED;
} ppp_lpbk_stats_t;
typedef struct	ppp_prot_stats
{
unsigned short rx_unknown	PACKED;
unsigned short rx_conf_rqst	PACKED;
unsigned short rx_conf_ack	PACKED;
unsigned short rx_conf_nak	PACKED;
unsigned short rx_conf_rej	PACKED;
unsigned short rx_term_rqst	PACKED;
unsigned short rx_term_ack	PACKED;
unsigned short rx_code_rej	PACKED;
unsigned short reserved		PACKED;
unsigned short tx_conf_rqst	PACKED;
unsigned short tx_conf_ack	PACKED;
unsigned short tx_conf_nak	PACKED;
unsigned short tx_conf_rej	PACKED;
unsigned short tx_term_rqst	PACKED;
unsigned short tx_term_ack	PACKED;
unsigned short tx_code_rej	PACKED;
unsigned short rx_too_large	PACKED;
unsigned short rx_ack_inval	PACKED;
unsigned short rx_rej_inval	PACKED;
unsigned short rx_rej_badid	PACKED;
} ppp_prot_stats_t;
typedef struct	ppp_pap_stats
{
unsigned short rx_unknown	PACKED;
unsigned short rx_auth_rqst	PACKED;
unsigned short rx_auth_ack	PACKED;
unsigned short rx_auth_nak	PACKED;
unsigned short reserved		PACKED;
unsigned short tx_auth_rqst	PACKED;
unsigned short tx_auth_ack	PACKED;
unsigned short tx_auth_nak	PACKED;
unsigned short rx_too_large	PACKED;
unsigned short rx_bad_peerid	PACKED;
unsigned short rx_bad_passwd	PACKED;
} ppp_pap_stats_t;
typedef struct	ppp_chap_stats
{
unsigned short rx_unknown	PACKED;
unsigned short rx_challenge	PACKED;
unsigned short rx_response	PACKED;
unsigned short rx_success	PACKED;
unsigned short rx_failure	PACKED;
unsigned short reserved		PACKED;
unsigned short tx_challenge	PACKED;
unsigned short tx_response	PACKED;
unsigned short tx_success	PACKED;
unsigned short tx_failure	PACKED;
unsigned short rx_too_large	PACKED;
unsigned short rx_bad_peerid	PACKED;
unsigned short rx_bad_passwd	PACKED;
unsigned short rx_bad_md5	PACKED;
unsigned short rx_bad_resp	PACKED;
} ppp_chap_stats_t;
typedef struct	ppp_conn_info
{
unsigned short remote_mru	PACKED;
unsigned char  ip_options	PACKED;
unsigned char  ip_local[4]	PACKED;
unsigned char  ip_remote[4]	PACKED;
unsigned char  ipx_options	PACKED;
unsigned char  ipx_network[4]	PACKED;
unsigned char  ipx_local[6]	PACKED;
unsigned char  ipx_remote[6]	PACKED;
unsigned char  ipx_router[48]	PACKED;
unsigned char  auth_status	PACKED;
unsigned char  peer_id[0]	PACKED;
} ppp_conn_info_t;
typedef struct ppp_intr_info{
unsigned char  i_enable		PACKED;
unsigned char  irq              PACKED;
unsigned short timer_len        PACKED;
} ppp_intr_info_t;
#define FT1_MONITOR_STATUS_CTRL                         0x80
#define SET_FT1_MODE                                    0x81
#define PPIPE_ENABLE_TRACING                            0x20
#define PPIPE_DISABLE_TRACING                           0x21
#define PPIPE_GET_TRACE_INFO                            0x22
#define PPIPE_GET_IBA_DATA                              0x23
#define PPIPE_KILL_BOARD     				0x24
#define PPIPE_FT1_READ_STATUS                           0x25
#define PPIPE_DRIVER_STAT_IFSEND                        0x26
#define PPIPE_DRIVER_STAT_INTR                          0x27
#define PPIPE_DRIVER_STAT_GEN                           0x28
#define PPIPE_FLUSH_DRIVER_STATS                        0x29
#define PPIPE_ROUTER_UP_TIME                            0x30
#define DISABLE_TRACING 				0x00
#define TRACE_SIGNALLING_FRAMES				0x01
#define TRACE_DATA_FRAMES				0x02
#ifdef		_MSC_
#  pragma	pack()
#endif
#endif