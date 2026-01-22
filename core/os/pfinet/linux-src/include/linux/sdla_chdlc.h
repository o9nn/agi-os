#ifndef _SDLA_CHDLC_H
# define _SDLC_CHDLC_H
#ifndef PACKED
#define PACKED __attribute__((packed))
#endif
#define PRI_BASE_ADDR_MB_STRUCT 0xE000
#define SEC_BASE_ADDR_MB_STRUCT 0xE800
#define SIZEOF_MB_DATA_BFR 2032
#define NUMBER_MB_RESERVED_BYTES 0x0B
#define MIN_LGTH_CHDLC_DATA_CFG 300
#define PRI_MAX_NO_DATA_BYTES_IN_FRAME 15354
typedef struct {
unsigned char opp_flag PACKED;
unsigned char command PACKED;
unsigned short buffer_length PACKED;
unsigned char return_code PACKED;
unsigned char MB_reserved[NUMBER_MB_RESERVED_BYTES] PACKED;
unsigned char data[SIZEOF_MB_DATA_BFR] PACKED;
} CHDLC_MAILBOX_STRUCT;
typedef struct {
pid_t pid_num PACKED;
CHDLC_MAILBOX_STRUCT cmdarea PACKED;
} CMDBLOCK_STRUCT;
#define READ_GLOBAL_EXCEPTION_CONDITION 0x01
#define SET_GLOBAL_CONFIGURATION 0x02
#define READ_GLOBAL_CONFIGURATION 0x03
#define READ_GLOBAL_STATISTICS 0x04
#define FLUSH_GLOBAL_STATISTICS 0x05
#define SET_MODEM_STATUS 0x06
#define READ_MODEM_STATUS 0x07
#define READ_COMMS_ERROR_STATS 0x08
#define FLUSH_COMMS_ERROR_STATS 0x09
#define SET_TRACE_CONFIGURATION 0x0A
#define READ_TRACE_CONFIGURATION 0x0B
#define READ_TRACE_STATISTICS 0x0C
#define FLUSH_TRACE_STATISTICS 0x0D
#define FT1_MONITOR_STATUS_CTRL 0x1C
#define SET_FT1_CONFIGURATION 0x18
#define READ_FT1_CONFIGURATION 0x19
#define TRANSMIT_ASYNC_DATA_TO_FT1 0x1A
#define RECEIVE_ASYNC_DATA_FROM_FT1 0x1B
#define FT1_MONITOR_STATUS_CTRL 0x1C
#define READ_FT1_OPERATIONAL_STATS 0x1D
#define SET_FT1_MODE 0x1E
#define READ_CHDLC_CODE_VERSION 0x20
#define READ_CHDLC_EXCEPTION_CONDITION 0x21
#define SET_CHDLC_CONFIGURATION 0x22
#define READ_CHDLC_CONFIGURATION 0x23
#define ENABLE_CHDLC_COMMUNICATIONS 0x24
#define DISABLE_CHDLC_COMMUNICATIONS 0x25
#define READ_CHDLC_LINK_STATUS 0x26
#define READ_CHDLC_OPERATIONAL_STATS 0x27
#define FLUSH_CHDLC_OPERATIONAL_STATS 0x28
#define SET_CHDLC_INTERRUPT_TRIGGERS 0x30
#define READ_CHDLC_INTERRUPT_TRIGGERS 0x31
#define CPIPE_ENABLE_TRACING 0x50
#define CPIPE_DISABLE_TRACING 0x51
#define CPIPE_GET_TRACE_INFO 0x52
#define CPIPE_GET_IBA_DATA 0x53
#define CPIPE_FT1_READ_STATUS 0x54
#define CPIPE_DRIVER_STAT_IFSEND 0x55
#define CPIPE_DRIVER_STAT_INTR 0x56
#define CPIPE_DRIVER_STAT_GEN 0x57
#define CPIPE_FLUSH_DRIVER_STATS 0x58
#define CPIPE_ROUTER_UP_TIME 0x59
#define CHDLC_READ_TRACE_DATA 0xE4
#define TRACE_ALL 0x00
#define TRACE_PROT 0x01
#define TRACE_DATA 0x02
#define COMMAND_OK 0x00
#define NO_GLOBAL_EXCEP_COND_TO_REPORT 0x01
#define LGTH_GLOBAL_CFG_DATA_INVALID 0x01
#define LGTH_TRACE_CFG_DATA_INVALID 0x01
#define IRQ_TIMEOUT_VALUE_INVALID 0x02
#define TRACE_CONFIG_INVALID 0x02
#define ADAPTER_OPERATING_FREQ_INVALID 0x03
#define TRC_DEAC_TMR_INVALID 0x03
#define S508_FT1_ADPTR_NOT_PRESENT 0x0C
#define INVALID_FT1_STATUS_SELECTION 0x0D
#define FT1_OP_STATS_NOT_ENABLED 0x0D
#define FT1_OP_STATS_NOT_AVAILABLE 0x0E
#define S508_FT1_MODE_SELECTION_BUSY 0x0E
#define EXCEP_MODEM_STATUS_CHANGE 0x10
#define EXCEP_TRC_DISABLED 0x11
#define EXCEP_IRQ_TIMEOUT 0x12
#define NO_CHDLC_EXCEP_COND_TO_REPORT 0x21
#define CHDLC_COMMS_DISABLED 0x21
#define CHDLC_COMMS_ENABLED 0x21
#define DISABLE_CHDLC_COMMS_BEFORE_CFG 0x21
#define ENABLE_CHDLC_COMMS_BEFORE_CONN 0x21
#define CHDLC_CFG_BEFORE_COMMS_ENABLED 0x22
#define LGTH_CHDLC_CFG_DATA_INVALID 0x22
#define LGTH_INT_TRIGGERS_DATA_INVALID 0x22
#define INVALID_IRQ_SELECTED 0x23
#define INVALID_CHDLC_CFG_DATA 0x23
#define IRQ_TMR_VALUE_INVALID 0x24
#define LARGER_PERCENT_TX_BFR_REQUIRED 0x24
#define LARGER_PERCENT_RX_BFR_REQUIRED 0x25
#define S514_BOTH_PORTS_SAME_CLK_MODE 0x26
#define INVALID_CMND_HDLC_STREAM_MODE 0x4E
#define INVALID_CHDLC_COMMAND 0x4F
#define EXCEP_LINK_ACTIVE 0x30
#define EXCEP_LINK_INACTIVE_MODEM 0x31
#define EXCEP_LINK_INACTIVE_KPALV 0x32
#define EXCEP_IP_ADDRESS_DISCOVERED 0x33
#define EXCEP_LOOPBACK_CONDITION 0x34
#define LINK_DISCONNECTED 0x21
#define NO_TX_BFRS_AVAIL 0x24
typedef struct {
unsigned short adapter_config_options PACKED;
unsigned short app_IRQ_timeout PACKED;
unsigned long adapter_operating_frequency PACKED;
} GLOBAL_CONFIGURATION_STRUCT;
#define MAX_APP_IRQ_TIMEOUT_VALUE 5000
typedef struct {
unsigned short app_IRQ_timeout_count PACKED;
} GLOBAL_STATS_STRUCT;
typedef struct {
unsigned short Rx_overrun_err_count PACKED;
unsigned short CRC_err_count PACKED;
unsigned short Rx_abort_count PACKED;
unsigned short Rx_dis_pri_bfrs_full_count PACKED;
unsigned short comms_err_stat_reserved_1 PACKED;
unsigned short sec_Tx_abort_msd_Tx_int_count PACKED;
unsigned short missed_Tx_und_int_count PACKED;
unsigned short sec_Tx_abort_count PACKED;
unsigned short DCD_state_change_count PACKED;
unsigned short CTS_state_change_count PACKED;
} COMMS_ERROR_STATS_STRUCT;
typedef struct {
unsigned char trace_config PACKED;
unsigned short trace_deactivation_timer PACKED;
unsigned long ptr_trace_stat_el_cfg_struct PACKED;
} LINE_TRACE_CONFIG_STRUCT;
#define TRACE_INACTIVE 0x00
#define TRACE_ACTIVE 0x01
#define TRACE_DELAY_MODE 0x04
#define TRACE_DATA_FRAMES 0x08
#define TRACE_SLARP_FRAMES 0x10
#define TRACE_CDP_FRAMES 0x20
typedef struct {
unsigned short number_trace_status_elements PACKED;
unsigned long base_addr_trace_status_elements PACKED;
unsigned long next_trace_element_to_use PACKED;
unsigned long base_addr_trace_buffer PACKED;
unsigned long end_addr_trace_buffer PACKED;
} TRACE_STATUS_EL_CFG_STRUCT;
typedef struct {
unsigned char opp_flag PACKED;
unsigned short trace_length PACKED;
unsigned char trace_type PACKED;
unsigned short trace_time_stamp PACKED;
unsigned short trace_reserved_1 PACKED;
unsigned long trace_reserved_2 PACKED;
unsigned long ptr_data_bfr PACKED;
} TRACE_STATUS_ELEMENT_STRUCT;
#define TRACE_INCOMING 0x00
#define TRACE_OUTGOINGING 0x01
#define TRACE_INCOMING_ABORTED 0x10
#define TRACE_INCOMING_CRC_ERROR 0x20
#define TRACE_INCOMING_OVERRUN_ERROR 0x40
typedef struct {
unsigned long frames_traced_count PACKED;
unsigned long trc_frms_not_recorded_count PACKED;
} LINE_TRACE_STATS_STRUCT;
#define DISABLE_FT1_STATUS_STATISTICS 0x00
#define ENABLE_READ_FT1_STATUS 0x01
#define ENABLE_READ_FT1_OP_STATS 0x02
#define FLUSH_FT1_OP_STATS 0x04
typedef struct {
unsigned long baud_rate PACKED;
unsigned short line_config_options PACKED;
unsigned short modem_config_options PACKED;
unsigned short modem_status_timer PACKED;
unsigned short CHDLC_API_options PACKED;
unsigned short CHDLC_protocol_options PACKED;
unsigned short percent_data_buffer_for_Tx PACKED;
unsigned short CHDLC_statistics_options PACKED;
unsigned short max_CHDLC_data_field_length PACKED;
unsigned short transmit_keepalive_timer PACKED;
unsigned short receive_keepalive_timer PACKED;
unsigned short keepalive_error_tolerance PACKED;
unsigned short SLARP_request_timer PACKED;
unsigned long IP_address PACKED;
unsigned long IP_netmask PACKED;
unsigned long ptr_shared_mem_info_struct PACKED;
unsigned long ptr_CHDLC_Tx_stat_el_cfg_struct PACKED;
unsigned long ptr_CHDLC_Rx_stat_el_cfg_struct PACKED;
} CHDLC_CONFIGURATION_STRUCT;
#define INTERFACE_LEVEL_V35 0x0000
#define INTERFACE_LEVEL_RS232 0x0001
#define DONT_RAISE_DTR_RTS_ON_EN_COMMS 0x0001
#define DONT_REPORT_CHG_IN_MODEM_STAT 0x0002
#define IGNORE_DCD_FOR_LINK_STAT 0x0001
#define IGNORE_CTS_FOR_LINK_STAT 0x0002
#define IGNORE_KPALV_FOR_LINK_STAT 0x0004
#define HDLC_STREAMING_MODE 0x8000
#define CHDLC_TX_DATA_BYTE_COUNT_STAT 0x0001
#define CHDLC_RX_DATA_BYTE_COUNT_STAT 0x0002
#define CHDLC_TX_THROUGHPUT_STAT 0x0004
#define CHDLC_RX_THROUGHPUT_STAT 0x0008
#define PRI_MAX_BAUD_RATE_S508 2666666
#define SEC_MAX_BAUD_RATE_S508 258064
#define PRI_MAX_BAUD_RATE_S514 2750000
#define SEC_MAX_BAUD_RATE_S514 515625
#define MIN_MODEM_TIMER 0
#define MAX_MODEM_TIMER 5000
#define SEC_MAX_NO_DATA_BYTES_IN_FRAME 2048
#define MIN_Tx_KPALV_TIMER 0
#define MAX_Tx_KPALV_TIMER 60000
#define DEFAULT_Tx_KPALV_TIMER 10000
#define MIN_Rx_KPALV_TIMER 10
#define MAX_Rx_KPALV_TIMER 60000
#define DEFAULT_Rx_KPALV_TIMER 10000
#define MIN_KPALV_ERR_TOL 1
#define MAX_KPALV_ERR_TOL 20
#define DEFAULT_KPALV_ERR_TOL 3
#define MIN_SLARP_REQ_TIMER 0
#define MAX_SLARP_REQ_TIMER 60000
#define DEFAULT_SLARP_REQ_TIMER 0
typedef struct {
unsigned char CHDLC_link_status PACKED;
unsigned char no_Data_frms_for_app PACKED;
unsigned char receiver_status PACKED;
unsigned char SLARP_state PACKED;
} CHDLC_LINK_STATUS_STRUCT;
#define CHDLC_LINK_INACTIVE 0x00
#define CHDLC_LINK_ACTIVE 0x01
typedef struct {
unsigned long Data_frames_Tx_count PACKED;
unsigned long Data_bytes_Tx_count PACKED;
unsigned long Data_Tx_throughput PACKED;
unsigned long no_ms_for_Data_Tx_thruput_comp PACKED;
unsigned long Tx_Data_discard_lgth_err_count PACKED;
unsigned long reserved_Data_frm_Tx_stat1 PACKED;
unsigned long reserved_Data_frm_Tx_stat2 PACKED;
unsigned long reserved_Data_frm_Tx_stat3 PACKED;
unsigned long Data_frames_Rx_count PACKED;
unsigned long Data_bytes_Rx_count PACKED;
unsigned long Data_Rx_throughput PACKED;
unsigned long no_ms_for_Data_Rx_thruput_comp PACKED;
unsigned long Rx_Data_discard_short_count PACKED;
unsigned long Rx_Data_discard_long_count PACKED;
unsigned long Rx_Data_discard_inactive_count PACKED;
unsigned long reserved_Data_frm_Rx_stat1 PACKED;
unsigned long CHDLC_SLARP_REQ_Tx_count PACKED;
unsigned long CHDLC_SLARP_REQ_Rx_count PACKED;
unsigned long CHDLC_SLARP_REPLY_Tx_count PACKED;
unsigned long CHDLC_SLARP_REPLY_Rx_count PACKED;
unsigned long CHDLC_SLARP_KPALV_Tx_count PACKED;
unsigned long CHDLC_SLARP_KPALV_Rx_count PACKED;
unsigned long reserved_SLARP_stat1 PACKED;
unsigned long reserved_SLARP_stat2 PACKED;
unsigned long CHDLC_CDP_Tx_count PACKED;
unsigned long CHDLC_CDP_Rx_count PACKED;
unsigned long reserved_CDP_stat1 PACKED;
unsigned long reserved_CDP_stat2 PACKED;
unsigned long reserved_CDP_stat3 PACKED;
unsigned long reserved_CDP_stat4 PACKED;
unsigned long reserved_CDP_stat5 PACKED;
unsigned long reserved_CDP_stat6 PACKED;
unsigned short Rx_frm_incomp_CHDLC_hdr_count PACKED;
unsigned short Rx_frms_too_long_count PACKED;
unsigned short Rx_invalid_CHDLC_addr_count PACKED;
unsigned short Rx_invalid_CHDLC_ctrl_count PACKED;
unsigned short Rx_invalid_CHDLC_type_count PACKED;
unsigned short Rx_SLARP_invalid_code_count PACKED;
unsigned short Rx_SLARP_Reply_bad_IP_addr PACKED;
unsigned short Rx_SLARP_Reply_bad_netmask PACKED;
unsigned long reserved_frm_format_err1 PACKED;
unsigned long reserved_frm_format_err2 PACKED;
unsigned long reserved_frm_format_err3 PACKED;
unsigned long reserved_frm_format_err4 PACKED;
unsigned short SLARP_Rx_keepalive_TO_count PACKED;
unsigned short SLARP_Request_TO_count PACKED;
unsigned long To_retry_reserved_stat1 PACKED;
unsigned long To_retry_reserved_stat2 PACKED;
unsigned long To_retry_reserved_stat3 PACKED;
unsigned short link_active_count PACKED;
unsigned short link_inactive_modem_count PACKED;
unsigned short link_inactive_keepalive_count PACKED;
unsigned short link_looped_count PACKED;
unsigned long link_status_reserved_stat1 PACKED;
unsigned long link_status_reserved_stat2 PACKED;
unsigned long reserved_misc_stat1 PACKED;
unsigned long reserved_misc_stat2 PACKED;
unsigned long reserved_misc_stat3 PACKED;
unsigned long reserved_misc_stat4 PACKED;
} CHDLC_OPERATIONAL_STATS_STRUCT;
typedef struct {
unsigned char CHDLC_interrupt_triggers PACKED;
unsigned char IRQ PACKED;
unsigned short interrupt_timer PACKED;
unsigned short misc_interrupt_bits PACKED;
} CHDLC_INT_TRIGGERS_STRUCT;
#define APP_INT_ON_RX_FRAME 0x01
#define APP_INT_ON_TX_FRAME 0x02
#define APP_INT_ON_COMMAND_COMPLETE 0x04
#define APP_INT_ON_TIMER 0x08
#define APP_INT_ON_GLOBAL_EXCEP_COND 0x10
#define APP_INT_ON_CHDLC_EXCEP_COND 0x20
#define APP_INT_ON_TRACE_DATA_AVAIL 0x80
#define NO_APP_INTS_PEND 0x00
#define RX_APP_INT_PEND 0x01
#define TX_APP_INT_PEND 0x02
#define COMMAND_COMPLETE_APP_INT_PEND 0x04
#define TIMER_APP_INT_PEND 0x08
#define GLOBAL_EXCEP_COND_APP_INT_PEND 0x10
#define CHDLC_EXCEP_COND_APP_INT_PEND 0x20
#define TRACE_DATA_AVAIL_APP_INT_PEND 0x80
#define DCD_HIGH 0x08
#define CTS_HIGH 0x20
typedef struct {
unsigned short number_Tx_status_elements PACKED;
unsigned long base_addr_Tx_status_elements PACKED;
unsigned long next_Tx_status_element_to_use PACKED;
} CHDLC_TX_STATUS_EL_CFG_STRUCT;
typedef struct {
unsigned char opp_flag PACKED;
unsigned short frame_length PACKED;
unsigned char reserved_1 PACKED;
unsigned long reserved_2 PACKED;
unsigned long reserved_3 PACKED;
unsigned long ptr_data_bfr PACKED;
} CHDLC_DATA_TX_STATUS_EL_STRUCT;
typedef struct {
unsigned short number_Rx_status_elements PACKED;
unsigned long base_addr_Rx_status_elements PACKED;
unsigned long next_Rx_status_element_to_use PACKED;
unsigned long base_addr_Rx_buffer PACKED;
unsigned long end_addr_Rx_buffer PACKED;
} CHDLC_RX_STATUS_EL_CFG_STRUCT;
typedef struct {
unsigned char opp_flag PACKED;
unsigned short frame_length PACKED;
unsigned char error_flag PACKED;
unsigned short time_stamp PACKED;
unsigned long reserved_1 PACKED;
unsigned short reserved_2 PACKED;
unsigned long ptr_data_bfr PACKED;
} CHDLC_DATA_RX_STATUS_EL_STRUCT;
typedef struct {
unsigned char global_status PACKED;
unsigned char modem_status PACKED;
unsigned char global_excep_conditions PACKED;
unsigned char glob_info_reserved[5] PACKED;
unsigned char codename[4] PACKED;
unsigned char codeversion[4] PACKED;
} GLOBAL_INFORMATION_STRUCT;
typedef struct {
unsigned char CHDLC_status PACKED;
unsigned char CHDLC_excep_conditions PACKED;
unsigned char CHDLC_info_reserved[14] PACKED;
} CHDLC_INFORMATION_STRUCT;
typedef struct {
unsigned char interrupt_type PACKED;
unsigned char interrupt_permission PACKED;
unsigned char int_info_reserved[14] PACKED;
} INTERRUPT_INFORMATION_STRUCT;
typedef struct {
unsigned char parallel_port_A_input PACKED;
unsigned char parallel_port_B_input PACKED;
unsigned char FT1_info_reserved[14] PACKED;
} FT1_INFORMATION_STRUCT;
typedef struct {
GLOBAL_INFORMATION_STRUCT global_info_struct PACKED;
CHDLC_INFORMATION_STRUCT CHDLC_info_struct PACKED;
INTERRUPT_INFORMATION_STRUCT interrupt_info_struct PACKED;
FT1_INFORMATION_STRUCT FT1_info_struct PACKED;
} SHARED_MEMORY_INFO_STRUCT;
typedef struct {
unsigned char opp_flag PACKED;
unsigned char command PACKED;
unsigned short buffer_length PACKED;
unsigned char return_code PACKED;
unsigned char MB_reserved[NUMBER_MB_RESERVED_BYTES] PACKED;
} cblock_t;
typedef struct {
unsigned char num_frames PACKED;
unsigned char ismoredata PACKED;
} trace_info_t;
typedef struct {
ip_pkt_t ip_pkt PACKED;
udp_pkt_t udp_pkt PACKED;
wp_mgmt_t wp_mgmt PACKED;
cblock_t cblock PACKED;
trace_info_t trace_info PACKED;
unsigned char data[SIZEOF_MB_DATA_BFR] PACKED;
} chdlc_udp_pkt_t;
typedef struct ft1_exec_cmd{
unsigned char command PACKED;
unsigned short buffer_length PACKED;
unsigned char return_code PACKED;
unsigned char MB_reserved[NUMBER_MB_RESERVED_BYTES] PACKED;
} ft1_exec_cmd_t;
typedef struct {
unsigned char opp_flag PACKED;
ft1_exec_cmd_t cmd PACKED;
unsigned char data[SIZEOF_MB_DATA_BFR] PACKED;
} ft1_exec_t;
#define UDPMGMT_SIGNATURE "CTPIPEAB"
#define UDPMGMT_UDP_PROTOCOL 0x11
typedef struct {
unsigned char status PACKED;
unsigned char data_avail PACKED;
unsigned short real_length PACKED;
unsigned short time_stamp PACKED;
unsigned char data[1] PACKED;
} trace_pkt_t;
typedef struct {
unsigned char error_flag PACKED;
unsigned short time_stamp PACKED;
unsigned char reserved[13] PACKED;
} api_rx_hdr_t;
typedef struct {
api_rx_hdr_t api_rx_hdr PACKED;
void * data PACKED;
} api_rx_element_t;
typedef struct {
unsigned char attr PACKED;
unsigned char reserved[15] PACKED;
} api_tx_hdr_t;
typedef struct {
api_tx_hdr_t api_tx_hdr PACKED;
void * data PACKED;
} api_tx_element_t;
typedef struct {
unsigned short framing_mode;
unsigned short encoding_mode;
unsigned short line_build_out;
unsigned short channel_base;
unsigned short baud_rate_kbps;
unsigned short clock_mode;
} ft1_config_t;
#define ESF_FRAMING 0x00
#define D4_FRAMING 0x01
#define B8ZS_ENCODING 0x00
#define AMI_ENCODING 0x01
#define LN_BLD_CSU_0dB_DSX1_0_to_133 0x00
#define LN_BLD_DSX1_133_to_266 0x01
#define LN_BLD_DSX1_266_to_399 0x02
#define LN_BLD_DSX1_399_to_533 0x03
#define LN_BLD_DSX1_533_to_655 0x04
#define LN_BLD_CSU_NEG_7dB 0x05
#define LN_BLD_CSU_NEG_15dB 0x06
#define LN_BLD_CSU_NEG_22dB 0x07
#define MIN_CHANNEL_BASE_VALUE 1
#define MAX_CHANNEL_BASE_VALUE 24
#define MIN_BAUD_RATE_KBPS 0
#define MAX_BAUD_RATE_KBPS 1536
#define BAUD_RATE_FT1_AUTO_CONFIG 0xFFFF
#define CLOCK_MODE_NORMAL 0x00
#define CLOCK_MODE_MASTER 0x01
#define BAUD_RATE_FT1_AUTO_CONFIG 0xFFFF
#define AUTO_FT1_CONFIG_NOT_COMPLETE 0x08
#define AUTO_FT1_CFG_FAIL_OP_MODE 0x0C
#define AUTO_FT1_CFG_FAIL_INVALID_LINE 0x0D
#ifdef _MSC_
# pragma pack()
#endif
#endif