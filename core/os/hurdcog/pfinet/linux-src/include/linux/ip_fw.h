#ifndef _IP_FWCHAINS_H
#define _IP_FWCHAINS_H
#ifdef __KERNEL__
#include <linux/icmp.h>
#include <linux/in.h>
#include <linux/ip.h>
#include <linux/tcp.h>
#include <linux/udp.h>
#endif
#define IP_FW_MAX_LABEL_LENGTH 8
typedef char ip_chainlabel[IP_FW_MAX_LABEL_LENGTH+1];
struct ip_fw
{
struct in_addr fw_src, fw_dst;
struct in_addr fw_smsk, fw_dmsk;
__u32 fw_mark;
__u16 fw_proto;
__u16 fw_flg;
__u16 fw_invflg;
__u16 fw_spts[2];
__u16 fw_dpts[2];
__u16 fw_redirpt;
__u16 fw_outputsize;
char fw_vianame[IFNAMSIZ];
__u8 fw_tosand, fw_tosxor;
};
struct ip_fwuser
{
struct ip_fw ipfw;
ip_chainlabel label;
};
#define IP_FW_F_PRN 0x0001
#define IP_FW_F_TCPSYN 0x0002
#define IP_FW_F_FRAG 0x0004
#define IP_FW_F_MARKABS 0x0008
#define IP_FW_F_WILDIF 0x0010
#define IP_FW_F_NETLINK 0x0020
#define IP_FW_F_MASK 0x003F
#define IP_FW_INV_SRCIP 0x0001
#define IP_FW_INV_DSTIP 0x0002
#define IP_FW_INV_PROTO 0x0004
#define IP_FW_INV_SRCPT 0x0008
#define IP_FW_INV_DSTPT 0x0010
#define IP_FW_INV_VIA 0x0020
#define IP_FW_INV_SYN 0x0040
#define IP_FW_INV_FRAG 0x0080
#define IP_FW_BASE_CTL 64
#define IP_FW_APPEND (IP_FW_BASE_CTL)
#define IP_FW_REPLACE (IP_FW_BASE_CTL+1)
#define IP_FW_DELETE_NUM (IP_FW_BASE_CTL+2)
#define IP_FW_DELETE (IP_FW_BASE_CTL+3)
#define IP_FW_INSERT (IP_FW_BASE_CTL+4)
#define IP_FW_FLUSH (IP_FW_BASE_CTL+5)
#define IP_FW_ZERO (IP_FW_BASE_CTL+6)
#define IP_FW_CHECK (IP_FW_BASE_CTL+7)
#define IP_FW_MASQ_TIMEOUTS (IP_FW_BASE_CTL+8)
#define IP_FW_CREATECHAIN (IP_FW_BASE_CTL+9)
#define IP_FW_DELETECHAIN (IP_FW_BASE_CTL+10)
#define IP_FW_POLICY (IP_FW_BASE_CTL+11)
#define IP_FW_MASQ_CTL (IP_FW_BASE_CTL+12)
#define IP_FW_LABEL_FORWARD "forward"
#define IP_FW_LABEL_INPUT "input"
#define IP_FW_LABEL_OUTPUT "output"
#define IP_FW_LABEL_MASQUERADE "MASQ"
#define IP_FW_LABEL_REDIRECT "REDIRECT"
#define IP_FW_LABEL_ACCEPT "ACCEPT"
#define IP_FW_LABEL_BLOCK "DENY"
#define IP_FW_LABEL_REJECT "REJECT"
#define IP_FW_LABEL_RETURN "RETURN"
#define IP_FW_LABEL_QUEUE "QUEUE"
#define IP_FW_PROC_CHAINS "ip_fwchains"
#define IP_FW_PROC_CHAIN_NAMES "ip_fwnames"
struct ip_fwpkt
{
struct iphdr fwp_iph;
union {
struct tcphdr fwp_tcph;
struct udphdr fwp_udph;
struct icmphdr fwp_icmph;
} fwp_protoh;
struct in_addr fwp_via;
char fwp_vianame[IFNAMSIZ];
};
struct ip_fwchange
{
struct ip_fwuser fwc_rule;
ip_chainlabel fwc_label;
};
struct ip_fwtest
{
struct ip_fwpkt fwt_packet;
ip_chainlabel fwt_label;
};
struct ip_fwdelnum
{
__u32 fwd_rulenum;
ip_chainlabel fwd_label;
};
struct ip_fwnew
{
__u32 fwn_rulenum;
struct ip_fwuser fwn_rule;
ip_chainlabel fwn_label;
};
struct ip_fwpolicy
{
ip_chainlabel fwp_policy;
ip_chainlabel fwp_label;
};
extern int ip_fw_masq_timeouts(void *, int);
#ifdef __KERNEL__
#include <linux/config.h>
#include <linux/version.h>
#if LINUX_VERSION_CODE >= KERNEL_VERSION(2,1,0)
#include <linux/init.h>
extern void ip_fw_init(void) __init;
#else
extern void ip_fw_init(void);
#endif
extern int ip_fw_ctl(int, void *, int);
#ifdef CONFIG_IP_MASQUERADE
extern int ip_masq_uctl(int, char *, int);
#endif
#endif
#endif