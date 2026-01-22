#include <sys/types.h>
#include <kern/printf.h>
#include <i386/ipl.h>
#include <i386/pic.h>
#include <i386/spl.h>
#include <i386/pio.h>
spl_t	curr_ipl[NCPUS] = {0};
int	curr_pic_mask;
int	spl_init = 0;
int	iunit[NINTR] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
struct irqinfo irqinfo[NINTR];
unsigned short	master_icw, master_ocw, slaves_icw, slaves_ocw;
u_short PICM_ICW1, PICM_OCW1, PICS_ICW1, PICS_OCW1 ;
u_short PICM_ICW2, PICM_OCW2, PICS_ICW2, PICS_OCW2 ;
u_short PICM_ICW3, PICM_OCW3, PICS_ICW3, PICS_OCW3 ;
u_short PICM_ICW4, PICS_ICW4 ;
void
picinit(void)
{
asm("cli");
int i;
for (i = 0; i < NCPUS; i++)
curr_ipl[i] = SPLHI;
curr_pic_mask = 0;
for (i = 0; i < NINTR; i++)
irqinfo[i].trigger = EDGE_TRIGGER;
master_icw = PIC_MASTER_ICW;
master_ocw = PIC_MASTER_OCW;
slaves_icw = PIC_SLAVE_ICW;
slaves_ocw = PIC_SLAVE_OCW;
PICM_ICW1 =
(ICW_TEMPLATE | EDGE_TRIGGER | ADDR_INTRVL8 | CASCADE_MODE | ICW4__NEEDED);
PICS_ICW1 =
(ICW_TEMPLATE | EDGE_TRIGGER | ADDR_INTRVL8 | CASCADE_MODE | ICW4__NEEDED);
PICM_ICW2 = PICM_VECTBASE;
PICS_ICW2 = PICS_VECTBASE;
#ifdef	AT386
PICM_ICW3 = ( SLAVE_ON_IR2 );
PICS_ICW3 = ( I_AM_SLAVE_2 );
#endif
PICM_ICW4 =
(SNF_MODE_DIS | NONBUFD_MODE | NRML_EOI_MOD | I8086_EMM_MOD);
PICS_ICW4 =
(SNF_MODE_DIS | NONBUFD_MODE | NRML_EOI_MOD | I8086_EMM_MOD);
PICM_OCW1 = (curr_pic_mask & 0x00FF);
PICS_OCW1 = ((curr_pic_mask & 0xFF00)>>8);
PICM_OCW2 = NON_SPEC_EOI;
PICS_OCW2 = NON_SPEC_EOI;
PICM_OCW3 = (OCW_TEMPLATE | READ_NEXT_RD | READ_IR_ONRD );
PICS_OCW3 = (OCW_TEMPLATE | READ_NEXT_RD | READ_IR_ONRD );
outb ( master_icw, PICM_ICW1 );
outb ( master_ocw, PICM_ICW2 );
outb ( master_ocw, PICM_ICW3 );
outb ( master_ocw, PICM_ICW4 );
outb ( master_ocw, PICM_MASK );
outb ( master_icw, PICM_OCW3 );
outb ( slaves_icw, PICS_ICW1 );
outb ( slaves_ocw, PICS_ICW2 );
outb ( slaves_ocw, PICS_ICW3 );
outb ( slaves_ocw, PICS_ICW4 );
outb ( slaves_ocw, PICS_OCW1 );
outb ( slaves_icw, PICS_OCW3 );
outb ( master_ocw, PICM_OCW1 );
}
void
intnull(int unit_dev)
{
static char warned[NINTR];
if (unit_dev >= NINTR)
printf("Unknown interrupt %d\n", unit_dev);
else if (!warned[unit_dev])
{
printf("intnull(%d)\n", unit_dev);
warned[unit_dev] = 1;
}
}
void
mask_irq (unsigned int irq_nr)
{
int new_pic_mask = curr_pic_mask | 1 << irq_nr;
if (curr_pic_mask != new_pic_mask)
{
curr_pic_mask = new_pic_mask;
if (irq_nr < 8)
{
outb (PIC_MASTER_OCW, curr_pic_mask & 0xff);
}
else
{
outb (PIC_SLAVE_OCW, curr_pic_mask >> 8);
}
}
}
void
unmask_irq (unsigned int irq_nr)
{
int mask;
int new_pic_mask;
mask = 1 << irq_nr;
if (irq_nr >= 8)
{
mask |= 1 << 2;
}
new_pic_mask = curr_pic_mask & ~mask;
if (curr_pic_mask != new_pic_mask)
{
curr_pic_mask = new_pic_mask;
if (irq_nr < 8)
{
outb (PIC_MASTER_OCW, curr_pic_mask & 0xff);
}
else
{
outb (PIC_SLAVE_OCW, curr_pic_mask >> 8);
}
}
}