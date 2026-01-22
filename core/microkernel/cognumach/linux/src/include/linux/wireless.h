#ifndef _LINUX_WIRELESS_H
#define _LINUX_WIRELESS_H
#include <linux/types.h>
#include <linux/socket.h>
#include <linux/if.h>
#define WIRELESS_EXT	10
#define SIOCSIWNAME	0x8B00
#define SIOCGIWNAME	0x8B01
#define SIOCSIWNWID	0x8B02
#define SIOCGIWNWID	0x8B03
#define SIOCSIWFREQ	0x8B04
#define SIOCGIWFREQ	0x8B05
#define SIOCSIWMODE	0x8B06
#define SIOCGIWMODE	0x8B07
#define SIOCSIWSENS	0x8B08
#define SIOCGIWSENS	0x8B09
#define SIOCSIWRANGE	0x8B0A
#define SIOCGIWRANGE	0x8B0B
#define SIOCSIWPRIV	0x8B0C
#define SIOCGIWPRIV	0x8B0D
#define SIOCSIWSPY	0x8B10
#define SIOCGIWSPY	0x8B11
#define SIOCSIWAP	0x8B14
#define SIOCGIWAP	0x8B15
#define SIOCGIWAPLIST	0x8B17
#define SIOCSIWESSID	0x8B1A
#define SIOCGIWESSID	0x8B1B
#define SIOCSIWNICKN	0x8B1C
#define SIOCGIWNICKN	0x8B1D
#define SIOCSIWRATE	0x8B20
#define SIOCGIWRATE	0x8B21
#define SIOCSIWRTS	0x8B22
#define SIOCGIWRTS	0x8B23
#define SIOCSIWFRAG	0x8B24
#define SIOCGIWFRAG	0x8B25
#define SIOCSIWTXPOW	0x8B26
#define SIOCGIWTXPOW	0x8B27
#define SIOCSIWENCODE	0x8B2A
#define SIOCGIWENCODE	0x8B2B
#define SIOCSIWPOWER	0x8B2C
#define SIOCGIWPOWER	0x8B2D
#define SIOCIWFIRST	0x8B00
#define SIOCIWLAST	0x8B30
#define IW_IS_SET(cmd)	(!((cmd) & 0x1))
#define IW_IS_GET(cmd)	((cmd) & 0x1)
#define IW_PRIV_TYPE_MASK	0x7000
#define IW_PRIV_TYPE_NONE	0x0000
#define IW_PRIV_TYPE_BYTE	0x1000
#define IW_PRIV_TYPE_CHAR	0x2000
#define IW_PRIV_TYPE_INT	0x4000
#define IW_PRIV_TYPE_FLOAT	0x5000
#define IW_PRIV_SIZE_FIXED	0x0800
#define IW_PRIV_SIZE_MASK	0x07FF
#define IW_MAX_FREQUENCIES	16
#define IW_MAX_BITRATES		8
#define IW_MAX_TXPOWER		8
#define IW_MAX_SPY		8
#define IW_MAX_AP		8
#define IW_ESSID_MAX_SIZE	32
#define IW_MODE_AUTO	0
#define IW_MODE_ADHOC	1
#define IW_MODE_INFRA	2
#define IW_MODE_MASTER	3
#define IW_MODE_REPEAT	4
#define IW_MODE_SECOND	5
#define IW_MAX_ENCODING_SIZES	8
#define IW_ENCODING_TOKEN_MAX	32
#define IW_ENCODE_INDEX		0x00FF
#define IW_ENCODE_FLAGS		0xFF00
#define IW_ENCODE_MODE		0xF000
#define IW_ENCODE_DISABLED	0x8000
#define IW_ENCODE_ENABLED	0x0000
#define IW_ENCODE_RESTRICTED	0x4000
#define IW_ENCODE_OPEN		0x2000
#define IW_ENCODE_NOKEY         0x0800
#define IW_POWER_ON		0x0000
#define IW_POWER_TYPE		0xF000
#define IW_POWER_PERIOD		0x1000
#define IW_POWER_TIMEOUT	0x2000
#define IW_POWER_MODE		0x0F00
#define IW_POWER_UNICAST_R	0x0100
#define IW_POWER_MULTICAST_R	0x0200
#define IW_POWER_ALL_R		0x0300
#define IW_POWER_FORCE_S	0x0400
#define IW_POWER_REPEATER	0x0800
#define IW_POWER_MODIFIER	0x000F
#define IW_POWER_MIN		0x0001
#define IW_POWER_MAX		0x0002
#define IW_POWER_RELATIVE	0x0004
#define IW_TXPOW_DBM		0x0000
#define IW_TXPOW_MWATT		0x0001
struct	iw_param
{
__s32		value;
__u8		fixed;
__u8		disabled;
__u16		flags;
};
struct	iw_point
{
caddr_t	pointer;
__u16		length;
__u16		flags;
};
struct	iw_freq
{
__u32		m;
__u16		e;
__u8		i;
};
struct	iw_quality
{
__u8		qual;
__u8		level;
__u8		noise;
__u8		updated;
};
struct	iw_discarded
{
__u32		nwid;
__u32		code;
__u32		misc;
};
struct	iw_statistics
{
__u16		status;
struct iw_quality	qual;
struct iw_discarded	discard;
};
struct	iwreq
{
union
{
char	ifrn_name[IFNAMSIZ];
} ifr_ifrn;
union
{
char		name[IFNAMSIZ];
struct iw_point	essid;
struct iw_param	nwid;
struct iw_freq	freq;
struct iw_param	sens;
struct iw_param	bitrate;
struct iw_param	txpower;
struct iw_param	rts;
struct iw_param	frag;
__u32		mode;
struct iw_point	encoding;
struct iw_param	power;
struct sockaddr	ap_addr;
struct iw_point	data;
}	u;
};
struct	iw_range
{
__u32		throughput;
__u32		min_nwid;
__u32		max_nwid;
__u16		num_channels;
__u8		num_frequency;
struct iw_freq	freq[IW_MAX_FREQUENCIES];
__s32	sensitivity;
struct iw_quality	max_qual;
__u8		num_bitrates;
__s32		bitrate[IW_MAX_BITRATES];
__s32		min_rts;
__s32		max_rts;
__s32		min_frag;
__s32		max_frag;
__s32		min_pmp;
__s32		max_pmp;
__s32		min_pmt;
__s32		max_pmt;
__u16		pmp_flags;
__u16		pmt_flags;
__u16		pm_capa;
__u16	encoding_size[IW_MAX_ENCODING_SIZES];
__u8	num_encoding_sizes;
__u8	max_encoding_tokens;
__u16		txpower_capa;
__u8		num_txpower;
__s32		txpower[IW_MAX_TXPOWER];
};
struct	iw_priv_args
{
__u32		cmd;
__u16		set_args;
__u16		get_args;
char		name[IFNAMSIZ];
};
#endif