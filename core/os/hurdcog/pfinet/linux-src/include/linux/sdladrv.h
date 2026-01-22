#ifndef _SDLADRV_H
#define _SDLADRV_H
#include <linux/version.h>
#if LINUX_VERSION_CODE >= 0x020100
#define LINUX_2_1
#endif
#define SDLA_MAXIORANGE 4
#define SDLA_WINDOWSIZE 0x2000
typedef struct sdlahw
{
unsigned type;
unsigned fwid;
unsigned port;
int irq;
char S514_cpu_no[1];
unsigned char S514_slot_no;
#ifdef LINUX_2_1
struct pci_dev *pci_dev;
#else
unsigned char pci_bus;
unsigned char pci_dev_func;
#endif
void * dpmbase;
unsigned dpmsize;
unsigned pclk;
unsigned long memory;
unsigned long vector;
unsigned io_range;
unsigned char regs[SDLA_MAXIORANGE];
unsigned reserved[5];
} sdlahw_t;
extern int sdla_setup (sdlahw_t* hw, void* sfm, unsigned len);
extern int sdla_down (sdlahw_t* hw);
extern int sdla_inten (sdlahw_t* hw);
extern int sdla_intde (sdlahw_t* hw);
extern int sdla_intack (sdlahw_t* hw);
extern void S514_intack (sdlahw_t* hw, u32 int_status);
extern void read_S514_int_stat (sdlahw_t* hw, u32* int_status);
extern int sdla_intr (sdlahw_t* hw);
extern int sdla_mapmem (sdlahw_t* hw, unsigned long addr);
extern int sdla_peek (sdlahw_t* hw, unsigned long addr, void* buf,
unsigned len);
extern int sdla_poke (sdlahw_t* hw, unsigned long addr, void* buf,
unsigned len);
extern int sdla_exec (void* opflag);
#endif