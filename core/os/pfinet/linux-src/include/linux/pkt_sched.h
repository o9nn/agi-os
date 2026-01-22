#ifndef __LINUX_PKT_SCHED_H
#define __LINUX_PKT_SCHED_H
#define TC_PRIO_BESTEFFORT		0
#define TC_PRIO_FILLER			1
#define TC_PRIO_BULK			2
#define TC_PRIO_INTERACTIVE_BULK	4
#define TC_PRIO_INTERACTIVE		6
#define TC_PRIO_CONTROL			7
#define TC_PRIO_MAX			15
struct tc_stats
{
__u64	bytes;
__u32	packets;
__u32	drops;
__u32	overlimits;
__u32	bps;
__u32	pps;
__u32	qlen;
__u32	backlog;
};
struct tc_estimator
{
char		interval;
unsigned char	ewma_log;
};
#define TC_H_MAJ_MASK (0xFFFF0000U)
#define TC_H_MIN_MASK (0x0000FFFFU)
#define TC_H_MAJ(h) ((h)&TC_H_MAJ_MASK)
#define TC_H_MIN(h) ((h)&TC_H_MIN_MASK)
#define TC_H_MAKE(maj,min) (((maj)&TC_H_MAJ_MASK)|((min)&TC_H_MIN_MASK))
#define TC_H_UNSPEC	(0U)
#define TC_H_ROOT	(0xFFFFFFFFU)
struct tc_ratespec
{
unsigned char	cell_log;
unsigned char	__reserved;
unsigned short	feature;
short		addend;
unsigned short	mpu;
__u32		rate;
};
struct tc_fifo_qopt
{
__u32	limit;
};
#define TCQ_PRIO_BANDS	16
struct tc_prio_qopt
{
int	bands;
__u8	priomap[TC_PRIO_MAX+1];
};
struct tc_csz_qopt
{
int		flows;
unsigned char	R_log;
unsigned char	delta_log;
__u8		priomap[TC_PRIO_MAX+1];
};
struct tc_csz_copt
{
struct tc_ratespec slice;
struct tc_ratespec rate;
struct tc_ratespec peakrate;
__u32		limit;
__u32		buffer;
__u32		mtu;
};
enum
{
TCA_CSZ_UNSPEC,
TCA_CSZ_PARMS,
TCA_CSZ_RTAB,
TCA_CSZ_PTAB,
};
struct tc_tbf_qopt
{
struct tc_ratespec rate;
struct tc_ratespec peakrate;
__u32		limit;
__u32		buffer;
__u32		mtu;
};
enum
{
TCA_TBF_UNSPEC,
TCA_TBF_PARMS,
TCA_TBF_RTAB,
TCA_TBF_PTAB,
};
struct tc_sfq_qopt
{
unsigned	quantum;
int		perturb_period;
__u32		limit;
unsigned	divisor;
unsigned	flows;
};
enum
{
TCA_RED_UNSPEC,
TCA_RED_PARMS,
TCA_RED_STAB,
};
struct tc_red_qopt
{
__u32		limit;
__u32		qth_min;
__u32		qth_max;
unsigned char   Wlog;
unsigned char   Plog;
unsigned char   Scell_log;
};
#define TC_CBQ_MAXPRIO		8
#define TC_CBQ_MAXLEVEL		8
#define TC_CBQ_DEF_EWMA		5
struct tc_cbq_lssopt
{
unsigned char	change;
unsigned char	flags;
#define TCF_CBQ_LSS_BOUNDED	1
#define TCF_CBQ_LSS_ISOLATED	2
unsigned char  	ewma_log;
unsigned char  	level;
#define TCF_CBQ_LSS_FLAGS	1
#define TCF_CBQ_LSS_EWMA	2
#define TCF_CBQ_LSS_MAXIDLE	4
#define TCF_CBQ_LSS_MINIDLE	8
#define TCF_CBQ_LSS_OFFTIME	0x10
#define TCF_CBQ_LSS_AVPKT	0x20
__u32		maxidle;
__u32		minidle;
__u32		offtime;
__u32		avpkt;
};
struct tc_cbq_wrropt
{
unsigned char	flags;
unsigned char	priority;
unsigned char	cpriority;
unsigned char	__reserved;
__u32		allot;
__u32		weight;
};
struct tc_cbq_ovl
{
unsigned char	strategy;
#define	TC_CBQ_OVL_CLASSIC	0
#define	TC_CBQ_OVL_DELAY	1
#define	TC_CBQ_OVL_LOWPRIO	2
#define	TC_CBQ_OVL_DROP		3
#define	TC_CBQ_OVL_RCLASSIC	4
unsigned char	priority2;
__u32		penalty;
};
struct tc_cbq_police
{
unsigned char	police;
unsigned char	__res1;
unsigned short	__res2;
};
struct tc_cbq_fopt
{
__u32		split;
__u32		defmap;
__u32		defchange;
};
struct tc_cbq_xstats
{
__u32		borrows;
__u32		overactions;
__s32		avgidle;
__s32		undertime;
};
enum
{
TCA_CBQ_UNSPEC,
TCA_CBQ_LSSOPT,
TCA_CBQ_WRROPT,
TCA_CBQ_FOPT,
TCA_CBQ_OVL_STRATEGY,
TCA_CBQ_RATE,
TCA_CBQ_RTAB,
TCA_CBQ_POLICE,
};
#define TCA_CBQ_MAX	TCA_CBQ_POLICE
#endif