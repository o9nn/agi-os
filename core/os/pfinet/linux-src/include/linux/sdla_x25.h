#ifndef	_SDLA_X25_H
#define	_SDLA_X25_H
#ifdef		_GNUC_
#  ifndef	PACKED
#    define	PACKED	__attribute__((packed))
#  endif
#else
#  define	PACKED
#endif
#ifdef		_MSC_
#  pragma	pack(1)
#endif
#define	X25_MAX_CHAN	255
#define	X25_MAX_DATA	1024
#define	X25_MBOX_OFFS	0x16B0
#define	X25_RXMBOX_OFFS	0x1AD0
#define	X25_STATUS_OFFS	0x1EF0
typedef struct X25Cmd
{
unsigned char command	PACKED;
unsigned short length	PACKED;
unsigned char result	PACKED;
unsigned char pf	PACKED;
unsigned short lcn	PACKED;
unsigned char qdm	PACKED;
unsigned char cause	PACKED;
unsigned char diagn	PACKED;
unsigned char pktType	PACKED;
unsigned char resrv[4]	PACKED;
} TX25Cmd;
#define X25_SET_GLOBAL_VARS	0x0B
#define X25_READ_MODEM_STATUS	0x0C
#define X25_READ_CODE_VERSION	0x15
#define X25_TRACE_CONFIGURE	0x14
#define X25_READ_TRACE_DATA	0x16
#define	X25_SET_INTERRUPT_MODE	0x17
#define	X25_READ_INTERRUPT_MODE	0x18
#define X25_HDLC_LINK_CONFIGURE	0x01
#define X25_HDLC_LINK_OPEN	0x02
#define X25_HDLC_LINK_CLOSE	0x03
#define X25_HDLC_LINK_SETUP	0x04
#define X25_HDLC_LINK_DISC	0x05
#define X25_HDLC_LINK_STATUS	0x06
#define X25_HDLC_READ_STATS	0x07
#define X25_HDLC_FLUSH_STATS	0x08
#define X25_HDLC_READ_COMM_ERR	0x09
#define X25_HDLC_FLUSH_COMM_ERR	0x0A
#define X25_HDLC_FLUSH_BUFFERS	0x0D
#define X25_HDLC_SPRVS_CNT_STAT 0x0F
#define X25_HDLC_SEND_UI_FRAME	0x10
#define X25_HDLC_WRITE		0x11
#define X25_HDLC_READ		0x21
#define X25_HDLC_READ_CONFIG	0x12
#define X25_HDLC_SET_CONFIG	0x13
#define X25_READ		0x22
#define X25_WRITE		0x23
#define X25_PLACE_CALL		0x30
#define X25_ACCEPT_CALL		0x31
#define X25_CLEAR_CALL		0x32
#define X25_CLEAR_CONFRM	0x33
#define X25_RESET		0x34
#define X25_RESET_CONFRM	0x35
#define X25_RESTART		0x36
#define X25_RESTART_CONFRM	0x37
#define X25_INTERRUPT		0x38
#define X25_INTERRUPT_CONFRM	0x39
#define X25_REGISTRATION_RQST	0x3A
#define X25_REGISTRATION_CONFRM	0x3B
#define X25_IS_DATA_AVAILABLE	0x40
#define X25_INCOMING_CALL_CTL	0x41
#define X25_CONFIGURE_PVC	0x42
#define X25_GET_ACTIVE_CHANNELS	0x43
#define X25_READ_CHANNEL_CONFIG	0x44
#define X25_FLUSH_DATA_BUFFERS	0x45
#define X25_READ_HISTORY_TABLE	0x46
#define X25_HISTORY_TABLE_CTL	0x47
#define	X25_GET_TX_D_BIT_STATUS	0x48
#define	X25_READ_STATISTICS	0x49
#define	X25_FLUSH_STATISTICS	0x4A
#define	X25_READ_CONFIGURATION	0x50
#define	X25_SET_CONFIGURATION	0x51
#define X25RES_OK		0x00
#define X25RES_ERROR		0x01
#define X25RES_LINK_NOT_IN_ABM	0x02
#define X25RES_LINK_CLOSED	0x03
#define X25RES_INVAL_LENGTH	0x04
#define X25RES_INVAL_CMD	0x05
#define X25RES_UNNUMBERED_FRAME	0x06
#define X25RES_FRM_REJECT_MODE	0x07
#define X25RES_MODEM_FAILURE	0x08
#define X25RES_N2_RETRY_LIMIT	0x09
#define X25RES_INVAL_LCN	0x30
#define X25RES_INVAL_STATE	0x31
#define X25RES_INVAL_DATA_LEN	0x32
#define X25RES_NOT_READY	0x33
#define X25RES_NETWORK_DOWN	0x34
#define X25RES_CHANNEL_IN_USE	0x35
#define X25RES_REGST_NOT_SUPPRT	0x36
#define X25RES_INVAL_FORMAT	0x37
#define X25RES_D_BIT_NOT_SUPPRT	0x38
#define X25RES_FACIL_NOT_SUPPRT	0x39
#define X25RES_INVAL_CALL_ARG	0x3A
#define X25RES_INVAL_CALL_DATA	0x3B
#define X25RES_ASYNC_PACKET	0x40
#define X25RES_PROTO_VIOLATION	0x41
#define X25RES_PKT_TIMEOUT	0x42
#define X25RES_PKT_RETRY_LIMIT	0x43
#define X25RES_LINK_DISC	0x00
#define X25RES_LINK_IN_ABM	0x01
#define X25RES_NO_DATA		0x01
#define X25RES_TRACE_INACTIVE	0x02
#define X25RES_LINK_IS_OPEN	0x01
#define X25RES_LINK_IS_DISC	0x02
#define X25RES_LINK_IS_CLOSED	0x03
#define X25RES_INVAL_PARAM	0x31
#define X25RES_INVAL_CONFIG	0x35
#define X25CMD_Q_BIT_MASK	0x04
#define X25CMD_D_BIT_MASK	0x02
#define X25CMD_M_BIT_MASK	0x01
#define ASE_CLEAR_RQST		0x02
#define ASE_RESET_RQST		0x04
#define ASE_RESTART_RQST	0x08
#define ASE_INTERRUPT		0x10
#define ASE_DTE_REGISTR_RQST	0x20
#define ASE_CALL_RQST		0x30
#define ASE_CALL_ACCEPTED	0x31
#define ASE_CLEAR_CONFRM	0x32
#define ASE_RESET_CONFRM	0x33
#define ASE_RESTART_CONFRM	0x34
#define ASE_INTERRUPT_CONFRM	0x35
#define ASE_DCE_REGISTR_CONFRM	0x36
#define ASE_DIAGNOSTIC		0x37
#define ASE_CALL_AUTO_CLEAR	0x38
#define AUTO_RESPONSE_FLAG	0x80
#define TOE_RESTART_RQST	0x03
#define TOE_CALL_RQST		0x05
#define TOE_CLEAR_RQST		0x08
#define TOE_RESET_RQST		0x0A
#define PVE_CLEAR_RQST		0x32
#define PVE_RESET_RQST		0x33
#define PVE_RESTART_RQST	0x34
#define PVE_DIAGNOSTIC		0x37
typedef struct X25Mbox
{
unsigned char opflag	PACKED;
TX25Cmd cmd		PACKED;
unsigned char data[1]	PACKED;
} TX25Mbox;
typedef struct X25TimeStamp
{
unsigned char month	PACKED;
unsigned char date	PACKED;
unsigned char sec	PACKED;
unsigned char min	PACKED;
unsigned char hour	PACKED;
} TX25TimeStamp;
typedef struct X25Status
{
unsigned short pvc_map	PACKED;
unsigned short icc_map	PACKED;
unsigned short twc_map	PACKED;
unsigned short ogc_map	PACKED;
TX25TimeStamp tstamp	PACKED;
unsigned char iflags	PACKED;
unsigned char imask     PACKED;
unsigned char resrv	PACKED;
unsigned char gflags	PACKED;
unsigned char cflags[X25_MAX_CHAN] PACKED;
} TX25Status;
#define X25_RX_INTR	0x01
#define X25_TX_INTR	0x02
#define X25_MODEM_INTR	0x04
#define X25_EVENT_INTR	0x10
#define X25_CMD_INTR	0x08
#define X25_HDLC_ABM	0x01
#define X25_RX_READY	0x02
#define X25_TRACE_READY	0x08
#define X25_EVENT_IND	0x20
#define X25_TX_READY	0x40
#define X25_XFER_MODE	0x80
#define X25_TXWIN_OPEN	0x40
#define X25_RXBUF_MASK	0x3F
typedef struct X25GlobalVars
{
unsigned char resrv	PACKED;
unsigned char dtrCtl	PACKED;
unsigned char resErr	PACKED;
} TX25GlobalVars;
#define X25_RAISE_DTR	0x01
#define X25_DROP_DTR	0x02
typedef struct X25ModemStatus
{
unsigned char	status	PACKED;
} TX25ModemStatus;
#define X25_CTS_MASK	0x20
#define X25_DCD_MASK	0x08
typedef struct X25LinkStatus
{
unsigned char txQueued	PACKED;
unsigned char rxQueued	PACKED;
unsigned char station	PACKED;
unsigned char reserved	PACKED;
unsigned char sfTally	PACKED;
} TX25LinkStatus;
#define	X25_STATION_DTE	0x01
#define X25_STATION_DCE	0x02
typedef struct HdlcStats
{
unsigned short rxIFrames	PACKED;
unsigned short rxNoseq		PACKED;
unsigned short rxNodata		PACKED;
unsigned short rxDiscarded	PACKED;
unsigned short rxTooLong	PACKED;
unsigned short rxBadAddr	PACKED;
unsigned short txAcked		PACKED;
unsigned short txRetransm	PACKED;
unsigned short t1Timeout	PACKED;
unsigned short rxSABM		PACKED;
unsigned short rxDISC		PACKED;
unsigned short rxDM		PACKED;
unsigned short rxFRMR		PACKED;
unsigned short txSABM		PACKED;
unsigned short txDISC		PACKED;
unsigned short txDM		PACKED;
unsigned short txFRMR		PACKED;
} THdlcStats;
typedef struct HdlcCommErr
{
unsigned char rxOverrun		PACKED;
unsigned char rxBadCrc		PACKED;
unsigned char rxAborted		PACKED;
unsigned char rxDropped		PACKED;
unsigned char txAborted		PACKED;
unsigned char txUnderrun	PACKED;
unsigned char txMissIntr	PACKED;
unsigned char reserved		PACKED;
unsigned char droppedDCD	PACKED;
unsigned char droppedCTS	PACKED;
} THdlcCommErr;
typedef struct X25Config
{
unsigned char baudRate		PACKED;
unsigned char t1		PACKED;
unsigned char t2		PACKED;
unsigned char n2		PACKED;
unsigned short hdlcMTU		PACKED;
unsigned char hdlcWindow	PACKED;
unsigned char t4		PACKED;
unsigned char autoModem		PACKED;
unsigned char autoHdlc		PACKED;
unsigned char hdlcOptions	PACKED;
unsigned char station		PACKED;
unsigned char pktWindow		PACKED;
unsigned short defPktSize	PACKED;
unsigned short pktMTU		PACKED;
unsigned short loPVC		PACKED;
unsigned short hiPVC		PACKED;
unsigned short loIncomingSVC	PACKED;
unsigned short hiIncomingSVC	PACKED;
unsigned short loTwoWaySVC	PACKED;
unsigned short hiTwoWaySVC	PACKED;
unsigned short loOutgoingSVC	PACKED;
unsigned short hiOutgoingSVC	PACKED;
unsigned short options		PACKED;
unsigned char responseOpt	PACKED;
unsigned short facil1		PACKED;
unsigned short facil2		PACKED;
unsigned short ccittFacil	PACKED;
unsigned short otherFacil	PACKED;
unsigned short ccittCompat	PACKED;
unsigned char t10t20		PACKED;
unsigned char t11t21		PACKED;
unsigned char t12t22		PACKED;
unsigned char t13t23		PACKED;
unsigned char t16t26		PACKED;
unsigned char t28		PACKED;
unsigned char r10r20		PACKED;
unsigned char r12r22		PACKED;
unsigned char r13r23		PACKED;
} TX25Config;
typedef struct X25ChanAlloc
{
unsigned short loPVC		PACKED;
unsigned short hiPVC		PACKED;
unsigned short loIncomingSVC	PACKED;
unsigned short hiIncomingSVC	PACKED;
unsigned short loTwoWaySVC	PACKED;
unsigned short hiTwoWaySVC	PACKED;
unsigned short loOutgoingSVC	PACKED;
unsigned short hiOutgoingSVC	PACKED;
} TX25ChanAlloc;
typedef struct X25ChanCfg
{
unsigned char type	PACKED;
unsigned char txConf	PACKED;
unsigned char rxConf	PACKED;
} TX25ChanCfg;
#define	X25_PVC  	0x01
#define	X25_SVC_IN	0x03
#define	X25_SVC_TWOWAY	0x07
#define	X25_SVC_OUT	0x0B
typedef struct X25Stats
{
unsigned short txRestartRqst	PACKED;
unsigned short rxRestartRqst	PACKED;
unsigned short txRestartConf	PACKED;
unsigned short rxRestartConf	PACKED;
unsigned short txResetRqst	PACKED;
unsigned short rxResetRqst	PACKED;
unsigned short txResetConf	PACKED;
unsigned short rxResetConf	PACKED;
unsigned short txCallRequest	PACKED;
unsigned short rxCallRequest	PACKED;
unsigned short txCallAccept	PACKED;
unsigned short rxCallAccept	PACKED;
unsigned short txClearRqst	PACKED;
unsigned short rxClearRqst	PACKED;
unsigned short txClearConf	PACKED;
unsigned short rxClearConf	PACKED;
unsigned short txDiagnostic	PACKED;
unsigned short rxDiagnostic	PACKED;
unsigned short txRegRqst	PACKED;
unsigned short rxRegRqst	PACKED;
unsigned short txRegConf	PACKED;
unsigned short rxRegConf	PACKED;
unsigned short txInterrupt	PACKED;
unsigned short rxInterrupt	PACKED;
unsigned short txIntrConf	PACKED;
unsigned short rxIntrConf	PACKED;
unsigned short txData		PACKED;
unsigned short rxData		PACKED;
unsigned short txRR		PACKED;
unsigned short rxRR		PACKED;
unsigned short txRNR		PACKED;
unsigned short rxRNR		PACKED;
} TX25Stats;
typedef struct X25EventLog
{
unsigned char	type	PACKED;
unsigned short	lcn	PACKED;
unsigned char	packet	PACKED;
unsigned char	cause	PACKED;
unsigned char	diag	PACKED;
TX25TimeStamp	ts	PACKED;
} TX25EventLog;
#define X25LOG_INCOMING		0x00
#define X25LOG_APPLICATION 	0x01
#define X25LOG_AUTOMATIC	0x02
#define X25LOG_ERROR		0x04
#define X25LOG_TIMEOUT		0x08
#define X25LOG_RECOVERY		0x10
#define X25LOG_CALL_RQST	0x0B
#define X25LOG_CALL_ACCEPTED	0x0F
#define X25LOG_CLEAR_RQST	0x13
#define X25LOG_CLEAR_CONFRM	0x17
#define X25LOG_RESET_RQST	0x1B
#define X25LOG_RESET_CONFRM	0x1F
#define X25LOG_RESTART_RQST	0xFB
#define X25LOG_RESTART_COMFRM	0xFF
#define X25LOG_DIAGNOSTIC	0xF1
#define X25LOG_DTE_REG_RQST	0xF3
#define X25LOG_DTE_REG_COMFRM	0xF7
typedef struct X25TraceCfg
{
unsigned char flags	PACKED;
unsigned char timeout	PACKED;
} TX25TraceCfg;
#define X25_TRC_ENABLE		0x01
#define X25_TRC_TIMESTAMP	0x02
#define X25_TRC_DELAY		0x04
#define X25_TRC_DATA		0x08
#define X25_TRC_SUPERVISORY	0x10
#define X25_TRC_ASYNCHRONOUS	0x20
#define X25_TRC_HDLC		0x40
#define X25_TRC_READ		0x80
typedef struct X25Trace
{
unsigned short length	PACKED;
unsigned char type	PACKED;
unsigned char lost_cnt	PACKED;
TX25TimeStamp tstamp	PACKED;
unsigned short millisec	PACKED;
unsigned char data[0]	PACKED;
} TX25Trace;
#define X25_TRC_TYPE_MASK	0x0F
#define X25_TRC_TYPE_RX_FRAME	0x00
#define X25_TRC_TYPE_TX_FRAME	0x01
#define X25_TRC_TYPE_ERR_FRAME	0x02
#define X25_TRC_ERROR_MASK	0xF0
#define X25_TRCERR_RX_ABORT	0x10
#define X25_TRCERR_RX_BADCRC	0x20
#define X25_TRCERR_RX_OVERRUN	0x30
#define X25_TRCERR_RX_TOO_LONG	0x40
#define X25_TRCERR_TX_ABORT	0x70
#define X25_TRCERR_TX_UNDERRUN	0x80
typedef struct HDLCFrame
{
unsigned char addr	PACKED;
unsigned char cntl	PACKED;
unsigned char data[0]	PACKED;
} THDLCFrame;
typedef struct X25Pkt
{
unsigned char lcn_hi	PACKED;
unsigned char lcn_lo	PACKED;
unsigned char type	PACKED;
unsigned char data[0]	PACKED;
} TX25Pkt;
#define	X25_Q_BIT_MASK		0x80
#define	X25_D_BIT_MASK		0x40
#define	X25_M_BITS_MASK		0x30
#define	X25_LCN_MSB_MASK	0x0F
#define	X25PKT_DATA		0x01
#define	X25PKT_SUPERVISORY	0x02
#define	X25PKT_CALL_RQST	0x0B
#define	X25PKT_CALL_ACCEPTED	0x0F
#define	X25PKT_CLEAR_RQST	0x13
#define	X25PKT_CLEAR_CONFRM	0x17
#define	X25PKT_RESET_RQST	0x1B
#define	X25PKT_RESET_CONFRM	0x1F
#define	X25PKT_RESTART_RQST	0xFB
#define	X25PKT_RESTART_CONFRM	0xFF
#define	X25PKT_INTERRUPT	0x23
#define	X25PKT_INTERRUPT_CONFRM	0x27
#define	X25PKT_DIAGNOSTIC	0xF1
#define	X25PKT_REGISTR_RQST	0xF3
#define	X25PKT_REGISTR_CONFRM	0xF7
#define	X25PKT_RR_MASKED	0x01
#define	X25PKT_RNR_MASKED	0x05
#ifdef		_MSC_
#  pragma	pack()
#endif
#endif