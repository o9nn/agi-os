#ifndef __NET_IPIP_H
#define __NET_IPIP_H 1
#include <linux/if_tunnel.h>
#define IPTUNNEL_ERR_TIMEO	(30*HZ)
struct ip_tunnel
{
struct ip_tunnel	*next;
struct device		*dev;
struct net_device_stats	stat;
int			recursion;
int			err_count;
unsigned long		err_time;
__u32			i_seqno;
__u32			o_seqno;
int			hlen;
int			mlink;
struct ip_tunnel_parm	parms;
};
extern int	ipip_init(void);
extern int	ipgre_init(void);
extern int	sit_init(void);
extern void	sit_cleanup(void);
#endif