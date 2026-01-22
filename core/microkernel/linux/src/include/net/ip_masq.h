#ifndef _IP_MASQ_H
#define _IP_MASQ_H
#include <linux/types.h>
#include <linux/netdevice.h>
#include <linux/skbuff.h>
#include <linux/config.h>
#define MAX_MASQ_APP_PORTS 12
#define PORT_MASQ_BEGIN	61000
#define PORT_MASQ_END	(PORT_MASQ_BEGIN+4096)
#define MASQUERADE_EXPIRE_TCP     15*60*HZ
#define MASQUERADE_EXPIRE_TCP_FIN  2*60*HZ
#define MASQUERADE_EXPIRE_UDP      5*60*HZ
#define MASQUERADE_EXPIRE_ICMP      125*HZ
#define IP_AUTOFW_EXPIRE	     15*HZ
#define IP_MASQ_F_OUT_SEQ              	0x01
#define IP_MASQ_F_IN_SEQ              	0x02
#define IP_MASQ_F_NO_DPORT    		0x04
#define IP_MASQ_F_NO_DADDR      	0x08
#define IP_MASQ_F_HASHED		0x10
#define IP_MASQ_F_SAW_RST		0x20
#define IP_MASQ_F_SAW_FIN_IN		0x40
#define IP_MASQ_F_SAW_FIN_OUT		0x80
#define IP_MASQ_F_SAW_FIN		(IP_MASQ_F_SAW_FIN_IN | \
IP_MASQ_F_SAW_FIN_OUT)
#define IP_MASQ_F_CONTROL		0x100
#define IP_MASQ_F_NO_SPORT    		0x200
#define IP_MASQ_F_FTP_PASV	    	0x400
#define IP_MASQ_F_NO_REPLY		0x800
#define IP_MASQ_F_AFW_PORT	       0x1000
#ifdef __KERNEL__
struct ip_masq_seq {
__u32		init_seq;
short		delta;
short		previous_delta;
};
struct ip_masq {
struct ip_masq  *m_link, *s_link;
struct timer_list timer;
__u16 		protocol;
__u16		sport, dport, mport;
__u32 		saddr, daddr, maddr;
struct ip_masq_seq out_seq, in_seq;
struct ip_masq_app *app;
void		*app_data;
unsigned  flags;
struct ip_masq	*control;
};
struct ip_fw_masq {
int tcp_timeout;
int tcp_fin_timeout;
int udp_timeout;
};
extern struct ip_fw_masq *ip_masq_expire;
extern int ip_masq_free_ports[3];
extern int ip_masq_init(void);
extern int ip_fw_masquerade(struct sk_buff **, struct device *);
extern int ip_fw_masq_icmp(struct sk_buff **, struct device *);
extern int ip_fw_demasquerade(struct sk_buff **, struct device *);
extern struct ip_masq *ip_masq_new(struct device *dev, int proto, __u32 saddr, __u16 sport, __u32 daddr, __u16 dport, unsigned flags);
extern void ip_masq_set_expire(struct ip_masq *ms, unsigned long tout);
#ifdef CONFIG_IP_MASQUERADE_IPAUTOFW
extern void ip_autofw_expire(unsigned long data);
#endif
struct ip_masq_app
{
struct ip_masq_app *next;
char *name;
unsigned type;
int n_attach;
int (*masq_init_1)
(struct ip_masq_app *, struct ip_masq *);
int (*masq_done_1)
(struct ip_masq_app *, struct ip_masq *);
int (*pkt_out)
(struct ip_masq_app *, struct ip_masq *, struct sk_buff **, struct device *);
int (*pkt_in)
(struct ip_masq_app *, struct ip_masq *, struct sk_buff **, struct device *);
};
extern int ip_masq_app_init(void);
extern int register_ip_masq_app(struct ip_masq_app *mapp, unsigned short proto, __u16 port);
extern int unregister_ip_masq_app(struct ip_masq_app *mapp);
extern struct ip_masq_app * ip_masq_app_get(unsigned short proto, __u16 port);
extern struct ip_masq_app * ip_masq_bind_app(struct ip_masq *ms);
extern int ip_masq_unbind_app(struct ip_masq *ms);
extern int ip_masq_app_pkt_out(struct ip_masq *, struct sk_buff **skb_p, struct device *dev);
extern int ip_masq_app_pkt_in(struct ip_masq *, struct sk_buff **skb_p, struct device *dev);
extern struct ip_masq * ip_masq_out_get_2(int protocol, __u32 s_addr, __u16 s_port, __u32 d_addr, __u16 d_port);
extern struct ip_masq * ip_masq_in_get_2(int protocol, __u32 s_addr, __u16 s_port, __u32 d_addr, __u16 d_port);
extern int ip_masq_app_getinfo(char *buffer, char **start, off_t offset, int length, int dummy);
extern struct sk_buff * ip_masq_skb_replace(struct sk_buff *skb, int pri, char *o_buf, int o_len, char *n_buf, int n_len);
#ifdef CONFIG_IP_MASQUERADE_IPAUTOFW
extern struct ip_autofw * ip_autofw_hosts;
#endif
#endif
#endif