#define SIOCGSCCPARAM SIOCDEVPRIVATE
#define SIOCSSCCPARAM (SIOCDEVPRIVATE+1)
#define TMR_0_HZ 25600
struct scc_param {
int pclk_hz;
int brg_tc;
int nrzi;
int clocks;
int txdelay;
int txtime;
int sqdelay;
int waittime;
int slottime;
int persist;
int dma;
};