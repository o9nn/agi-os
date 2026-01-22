#include <linux/config.h>
#include <linux/netdevice.h>
#include <linux/errno.h>
#define NEXT_DEV NULL
extern int tulip_probe(struct device *dev);
extern int hp100_probe(struct device *dev);
extern int ultra_probe(struct device *dev);
extern int ultra32_probe(struct device *dev);
extern int wd_probe(struct device *dev);
extern int el2_probe(struct device *dev);
extern int ne_probe(struct device *dev);
extern int ne2k_pci_probe(struct device *dev);
extern int hp_probe(struct device *dev);
extern int hp_plus_probe(struct device *dev);
extern int znet_probe(struct device *);
extern int express_probe(struct device *);
extern int eepro_probe(struct device *);
extern int el3_probe(struct device *);
extern int at1500_probe(struct device *);
extern int at1700_probe(struct device *);
extern int fmv18x_probe(struct device *);
extern int eth16i_probe(struct device *);
extern int depca_probe(struct device *);
extern int apricot_probe(struct device *);
extern int ewrk3_probe(struct device *);
extern int de4x5_probe(struct device *);
extern int el1_probe(struct device *);
extern int via_rhine_probe(struct device *);
extern int natsemi_probe(struct device *);
extern int ns820_probe(struct device *);
extern int winbond840_probe(struct device *);
extern int hamachi_probe(struct device *);
extern int sundance_probe(struct device *);
extern int starfire_probe(struct device *);
extern int myson803_probe(struct device *);
extern int igige_probe(struct device *);
#if defined(CONFIG_WAVELAN)
extern int wavelan_probe(struct device *);
#endif
extern int el16_probe(struct device *);
extern int elplus_probe(struct device *);
extern int ac3200_probe(struct device *);
extern int e2100_probe(struct device *);
extern int ni52_probe(struct device *);
extern int ni65_probe(struct device *);
extern int SK_init(struct device *);
extern int seeq8005_probe(struct device *);
extern int tc59x_probe(struct device *);
extern int dgrs_probe(struct device *);
extern int smc_init( struct device * );
extern int sparc_lance_probe(struct device *);
extern int atarilance_probe(struct device *);
extern int a2065_probe(struct device *);
extern int ariadne_probe(struct device *);
extern int hydra_probe(struct device *);
extern int yellowfin_probe(struct device *);
extern int eepro100_probe(struct device *);
extern int epic100_probe(struct device *);
extern int rtl8139_probe(struct device *);
extern int sis900_probe(struct device *);
extern int tlan_probe(struct device *);
extern int isa515_probe(struct device *);
extern int pcnet32_probe(struct device *);
extern int lance_probe(struct device *);
extern int atp_init(struct device *);
extern int de600_probe(struct device *);
extern int de620_probe(struct device *);
extern int tc515_probe(struct device *);
static int
ethif_probe(struct device *dev)
{
u_long base_addr = dev->base_addr;
if ((base_addr == 0xffe0) || (base_addr == 1))
return 1;
if (1
#ifdef CONFIG_DE4X5
&& de4x5_probe(dev)
#endif
#ifdef CONFIG_DGRS
&& dgrs_probe(dev)
#endif
#ifdef CONFIG_EEXPRESS_PRO100B
&& eepro100_probe(dev)
#endif
#ifdef CONFIG_EPIC
&& epic100_probe(dev)
#endif
#if defined(CONFIG_HP100)
&& hp100_probe(dev)
#endif
#if defined(CONFIG_NE2K_PCI)
&& ne2k_pci_probe(dev)
#endif
#ifdef CONFIG_PCNET32
&& pcnet32_probe(dev)
#endif
#ifdef CONFIG_RTL8139
&& rtl8139_probe(dev)
#endif
#ifdef CONFIG_SIS900
&& sis900_probe(dev)
#endif
#ifdef CONFIG_VIA_RHINE
&& via_rhine_probe(dev)
#endif
#ifdef CONFIG_NATSEMI
&& natsemi_probe(dev)
#endif
#ifdef CONFIG_NS820
&& ns820_probe(dev)
#endif
#ifdef CONFIG_WINBOND840
&& winbond840_probe(dev)
#endif
#ifdef CONFIG_HAMACHI
&& hamachi_probe(dev)
#endif
#ifdef CONFIG_SUNDANCE
&& sundance_probe(dev)
#endif
#ifdef CONFIG_STARFIRE
&& starfire_probe(dev)
#endif
#ifdef CONFIG_MYSON803
&& myson803_probe(dev)
#endif
#ifdef CONFIG_INTEL_GIGE
&& igige_probe(dev)
#endif
#if defined(CONFIG_DEC_ELCP)
&& tulip_probe(dev)
#endif
#ifdef CONFIG_YELLOWFIN
&& yellowfin_probe(dev)
#endif
#ifdef CONFIG_AC3200
&& ac3200_probe(dev)
#endif
#if defined(CONFIG_ULTRA32)
&& ultra32_probe(dev)
#endif
#ifdef CONFIG_AT1700
&& at1700_probe(dev)
#endif
#if defined(CONFIG_ULTRA)
&& ultra_probe(dev)
#endif
#if defined(CONFIG_SMC9194)
&& smc_init(dev)
#endif
#if defined(CONFIG_WD80x3)
&& wd_probe(dev)
#endif
#if defined(CONFIG_EL2)
&& el2_probe(dev)
#endif
#if defined(CONFIG_HPLAN)
&& hp_probe(dev)
#endif
#if defined(CONFIG_HPLAN_PLUS)
&& hp_plus_probe(dev)
#endif
#if defined(CONFIG_SEEQ8005)
&& seeq8005_probe(dev)
#endif
#ifdef CONFIG_E2100
&& e2100_probe(dev)
#endif
#if defined(CONFIG_NE2000)
&& ne_probe(dev)
#endif
#ifdef CONFIG_AT1500
&& at1500_probe(dev)
#endif
#ifdef CONFIG_FMV18X
&& fmv18x_probe(dev)
#endif
#ifdef CONFIG_ETH16I
&& eth16i_probe(dev)
#endif
#ifdef CONFIG_EL3
&& el3_probe(dev)
#endif
#if defined(CONFIG_VORTEX)
&& tc59x_probe(dev)
#endif
#ifdef CONFIG_3C515
&& tc515_probe(dev)
#endif
#ifdef CONFIG_ZNET
&& znet_probe(dev)
#endif
#ifdef CONFIG_EEXPRESS
&& express_probe(dev)
#endif
#ifdef CONFIG_EEXPRESS_PRO
&& eepro_probe(dev)
#endif
#ifdef CONFIG_DEPCA
&& depca_probe(dev)
#endif
#ifdef CONFIG_EWRK3
&& ewrk3_probe(dev)
#endif
#ifdef CONFIG_APRICOT
&& apricot_probe(dev)
#endif
#ifdef CONFIG_EL1
&& el1_probe(dev)
#endif
#if defined(CONFIG_WAVELAN)
&& wavelan_probe(dev)
#endif
#ifdef CONFIG_EL16
&& el16_probe(dev)
#endif
#ifdef CONFIG_ELPLUS
&& elplus_probe(dev)
#endif
#ifdef CONFIG_DE600
&& de600_probe(dev)
#endif
#ifdef CONFIG_DE620
&& de620_probe(dev)
#endif
#if defined(CONFIG_SK_G16)
&& SK_init(dev)
#endif
#ifdef CONFIG_NI52
&& ni52_probe(dev)
#endif
#ifdef CONFIG_NI65
&& ni65_probe(dev)
#endif
#ifdef CONFIG_LANCE
&& lance_probe(dev)
#endif
#ifdef CONFIG_ATARILANCE
&& atarilance_probe(dev)
#endif
#ifdef CONFIG_A2065
&& a2065_probe(dev)
#endif
#ifdef CONFIG_ARIADNE
&& ariadne_probe(dev)
#endif
#ifdef CONFIG_HYDRA
&& hydra_probe(dev)
#endif
#ifdef CONFIG_SUNLANCE
&& sparc_lance_probe(dev)
#endif
#ifdef CONFIG_TLAN
&& tlan_probe(dev)
#endif
#ifdef CONFIG_LANCE
&& lance_probe(dev)
#endif
&& 1 ) {
return 1;
}
return 0;
}
#ifdef CONFIG_SDLA
extern int sdla_init(struct device *);
static struct device sdla0_dev = { "sdla0", 0, 0, 0, 0, 0, 0, 0, 0, 0, NEXT_DEV, sdla_init, };
# undef NEXT_DEV
# define NEXT_DEV (&sdla0_dev)
#endif
#ifdef CONFIG_NETROM
extern int nr_init(struct device *);
static struct device nr3_dev = { "nr3", 0, 0, 0, 0, 0, 0, 0, 0, 0, NEXT_DEV, nr_init, };
static struct device nr2_dev = { "nr2", 0, 0, 0, 0, 0, 0, 0, 0, 0, &nr3_dev, nr_init, };
static struct device nr1_dev = { "nr1", 0, 0, 0, 0, 0, 0, 0, 0, 0, &nr2_dev, nr_init, };
static struct device nr0_dev = { "nr0", 0, 0, 0, 0, 0, 0, 0, 0, 0, &nr1_dev, nr_init, };
# undef NEXT_DEV
# define NEXT_DEV (&nr0_dev)
#endif
#ifdef CONFIG_ATP
static struct device atp_dev = {
"atp0", 0, 0, 0, 0, 0, 0, 0, 0, 0, NEXT_DEV, atp_init, };
# undef NEXT_DEV
# define NEXT_DEV (&atp_dev)
#endif
#ifdef CONFIG_ARCNET
extern int arcnet_probe(struct device *dev);
static struct device arcnet_dev = {
"arc0", 0x0, 0x0, 0x0, 0x0, 0, 0, 0, 0, 0, NEXT_DEV, arcnet_probe, };
# undef NEXT_DEV
# define NEXT_DEV (&arcnet_dev)
#endif
#ifdef MACH
#ifndef ETH1_ADDR
# define ETH1_ADDR 0
#endif
#ifndef ETH1_IRQ
# define ETH1_IRQ 0
#endif
#endif
#ifndef ETH0_ADDR
# define ETH0_ADDR 0
#endif
#ifndef ETH0_IRQ
# define ETH0_IRQ 0
#endif
static struct device eth7_dev = {
"eth7", 0,0,0,0,0xffe0 , 0,0,0,0, NEXT_DEV, ethif_probe };
static struct device eth6_dev = {
"eth6", 0,0,0,0,0xffe0 , 0,0,0,0, &eth7_dev, ethif_probe };
static struct device eth5_dev = {
"eth5", 0,0,0,0,0xffe0 , 0,0,0,0, &eth6_dev, ethif_probe };
static struct device eth4_dev = {
"eth4", 0,0,0,0,0xffe0 , 0,0,0,0, &eth5_dev, ethif_probe };
static struct device eth3_dev = {
"eth3", 0,0,0,0,0xffe0 , 0,0,0,0, &eth4_dev, ethif_probe };
static struct device eth2_dev = {
"eth2", 0,0,0,0,0xffe0 , 0,0,0,0, &eth3_dev, ethif_probe };
#ifdef MACH
static struct device eth1_dev = {
"eth1", 0, 0, 0, 0, ETH1_ADDR, ETH1_IRQ, 0, 0, 0, &eth2_dev, ethif_probe };
#else
static struct device eth1_dev = {
"eth1", 0,0,0,0,0xffe0 , 0,0,0,0, &eth2_dev, ethif_probe };
#endif
static struct device eth0_dev = {
"eth0", 0, 0, 0, 0, ETH0_ADDR, ETH0_IRQ, 0, 0, 0, &eth1_dev, ethif_probe };
# undef NEXT_DEV
# define NEXT_DEV (&eth0_dev)
#if defined(PLIP) || defined(CONFIG_PLIP)
extern int plip_init(struct device *);
static struct device plip2_dev = {
"plip2", 0, 0, 0, 0, 0x278, 2, 0, 0, 0, NEXT_DEV, plip_init, };
static struct device plip1_dev = {
"plip1", 0, 0, 0, 0, 0x378, 7, 0, 0, 0, &plip2_dev, plip_init, };
static struct device plip0_dev = {
"plip0", 0, 0, 0, 0, 0x3BC, 5, 0, 0, 0, &plip1_dev, plip_init, };
# undef NEXT_DEV
# define NEXT_DEV (&plip0_dev)
#endif
#if defined(SLIP) || defined(CONFIG_SLIP)
extern int slip_init_ctrl_dev(struct device *);
static struct device slip_bootstrap = {
"slip_proto", 0x0, 0x0, 0x0, 0x0, 0, 0, 0, 0, 0, NEXT_DEV, slip_init_ctrl_dev, };
#undef NEXT_DEV
#define NEXT_DEV (&slip_bootstrap)
#endif
#if defined(CONFIG_STRIP)
extern int strip_init_ctrl_dev(struct device *);
static struct device strip_bootstrap = {
"strip_proto", 0x0, 0x0, 0x0, 0x0, 0, 0, 0, 0, 0, NEXT_DEV, strip_init_ctrl_dev, };
#undef NEXT_DEV
#define NEXT_DEV (&strip_bootstrap)
#endif
#if defined(CONFIG_PPP)
extern int ppp_init(struct device *);
static struct device ppp_bootstrap = {
"ppp_proto", 0x0, 0x0, 0x0, 0x0, 0, 0, 0, 0, 0, NEXT_DEV, ppp_init, };
#undef NEXT_DEV
#define NEXT_DEV (&ppp_bootstrap)
#endif
#ifdef CONFIG_DUMMY
extern int dummy_init(struct device *dev);
static struct device dummy_dev = {
"dummy", 0x0, 0x0, 0x0, 0x0, 0, 0, 0, 0, 0, NEXT_DEV, dummy_init, };
# undef NEXT_DEV
# define NEXT_DEV (&dummy_dev)
#endif
#ifdef CONFIG_EQUALIZER
extern int eql_init(struct device *dev);
struct device eql_dev = {
"eql",
0x0, 0x0, 0x0, 0x0,
0,
0,
0, 0, 0,
NEXT_DEV,
eql_init
};
# undef NEXT_DEV
# define NEXT_DEV (&eql_dev)
#endif
#ifdef CONFIG_IBMTR
extern int tok_probe(struct device *dev);
static struct device ibmtr_dev1 = {
"tr1",
0x0,
0x0,
0x0,
0x0,
0xa24,
0,
0, 0, 0,
NEXT_DEV,
tok_probe
};
# undef NEXT_DEV
# define NEXT_DEV (&ibmtr_dev1)
static struct device ibmtr_dev0 = {
"tr0",
0x0,
0x0,
0x0,
0x0,
0xa20,
0,
0, 0, 0,
NEXT_DEV,
tok_probe
};
# undef NEXT_DEV
# define NEXT_DEV (&ibmtr_dev0)
#endif
#ifdef CONFIG_DEFXX
extern int dfx_probe(struct device *dev);
static struct device fddi7_dev =
{"fddi7", 0, 0, 0, 0, 0, 0, 0, 0, 0, NEXT_DEV, dfx_probe};
static struct device fddi6_dev =
{"fddi6", 0, 0, 0, 0, 0, 0, 0, 0, 0, &fddi7_dev, dfx_probe};
static struct device fddi5_dev =
{"fddi5", 0, 0, 0, 0, 0, 0, 0, 0, 0, &fddi6_dev, dfx_probe};
static struct device fddi4_dev =
{"fddi4", 0, 0, 0, 0, 0, 0, 0, 0, 0, &fddi5_dev, dfx_probe};
static struct device fddi3_dev =
{"fddi3", 0, 0, 0, 0, 0, 0, 0, 0, 0, &fddi4_dev, dfx_probe};
static struct device fddi2_dev =
{"fddi2", 0, 0, 0, 0, 0, 0, 0, 0, 0, &fddi3_dev, dfx_probe};
static struct device fddi1_dev =
{"fddi1", 0, 0, 0, 0, 0, 0, 0, 0, 0, &fddi2_dev, dfx_probe};
static struct device fddi0_dev =
{"fddi0", 0, 0, 0, 0, 0, 0, 0, 0, 0, &fddi1_dev, dfx_probe};
#undef NEXT_DEV
#define NEXT_DEV (&fddi0_dev)
#endif
#ifdef CONFIG_NET_IPIP
extern int tunnel_init(struct device *);
static struct device tunnel_dev1 =
{
"tunl1",
0x0,
0x0,
0x0,
0x0,
0x0,
0,
0, 0, 0,
NEXT_DEV,
tunnel_init
};
static struct device tunnel_dev0 =
{
"tunl0",
0x0,
0x0,
0x0,
0x0,
0x0,
0,
0, 0, 0,
&tunnel_dev1,
tunnel_init
};
# undef NEXT_DEV
# define NEXT_DEV (&tunnel_dev0)
#endif
#ifdef CONFIG_APFDDI
extern int apfddi_init(struct device *dev);
static struct device fddi_dev = {
"fddi", 0x0, 0x0, 0x0, 0x0, 0, 0, 0, 0, 0, NEXT_DEV, apfddi_init };
# undef NEXT_DEV
# define NEXT_DEV (&fddi_dev)
#endif
#ifdef CONFIG_APBIF
extern int bif_init(struct device *dev);
static struct device bif_dev = {
"bif", 0x0, 0x0, 0x0, 0x0, 0, 0, 0, 0, 0, NEXT_DEV, bif_init };
# undef NEXT_DEV
# define NEXT_DEV (&bif_dev)
#endif
#ifdef MACH
struct device *dev_base = &eth0_dev;
#else
extern int loopback_init(struct device *dev);
struct device loopback_dev = {
"lo",
0x0,
0x0,
0x0,
0x0,
0,
0,
0, 0, 0,
NEXT_DEV,
loopback_init
};
struct device *dev_base = &loopback_dev;
#endif