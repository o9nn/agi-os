#ifndef	_I386_PIC_H_
#define	_I386_PIC_H_
#ifndef APIC
#define NINTR	0x10
#endif
#define	NPICS	0x02
#if	defined(AT386) || defined(ATX86_64)
#define ADDR_PIC_BASE		0x20
#define OFF_ICW			0x00
#define OFF_OCW			0x01
#define SIZE_PIC		0x80
#endif
#define PIC_MASTER_ICW		(ADDR_PIC_BASE + OFF_ICW)
#define PIC_MASTER_OCW		(ADDR_PIC_BASE + OFF_OCW)
#define PIC_SLAVE_ICW		(PIC_MASTER_ICW + SIZE_PIC)
#define PIC_SLAVE_OCW		(PIC_MASTER_OCW + SIZE_PIC)
#define ICW_TEMPLATE		0x10
#define LEVL_TRIGGER		0x08
#define EDGE_TRIGGER		0x00
#define ADDR_INTRVL4		0x04
#define ADDR_INTRVL8		0x00
#define SINGLE__MODE		0x02
#define CASCADE_MODE		0x00
#define ICW4__NEEDED		0x01
#define NO_ICW4_NEED		0x00
#if	defined(AT386) || defined(ATX86_64)
#define	PICM_VECTBASE		0x20
#define PICS_VECTBASE		PICM_VECTBASE + 0x08
#endif
#define SLAVE_ON_IR0		0x01
#define SLAVE_ON_IR1		0x02
#define SLAVE_ON_IR2		0x04
#define SLAVE_ON_IR3		0x08
#define SLAVE_ON_IR4		0x10
#define SLAVE_ON_IR5		0x20
#define SLAVE_ON_IR6		0x40
#define SLAVE_ON_IR7		0x80
#define I_AM_SLAVE_0		0x00
#define I_AM_SLAVE_1		0x01
#define I_AM_SLAVE_2		0x02
#define I_AM_SLAVE_3		0x03
#define I_AM_SLAVE_4		0x04
#define I_AM_SLAVE_5		0x05
#define I_AM_SLAVE_6		0x06
#define I_AM_SLAVE_7		0x07
#define SNF_MODE_ENA		0x10
#define SNF_MODE_DIS		0x00
#define BUFFERD_MODE		0x08
#define NONBUFD_MODE		0x00
#define AUTO_EOI_MOD		0x02
#define NRML_EOI_MOD		0x00
#define I8086_EMM_MOD		0x01
#define SET_MCS_MODE		0x00
#define PICM_MASK		0xFF
#define	PICS_MASK		0xFF
#define NON_SPEC_EOI		0x20
#define SPECIFIC_EOI		0x60
#define ROT_NON_SPEC		0xA0
#define SET_ROT_AEOI		0x80
#define RSET_ROTAEOI		0x00
#define ROT_SPEC_EOI		0xE0
#define SET_PRIORITY		0xC0
#define NO_OPERATION		0x40
#define SEND_EOI_IR0		0x00
#define SEND_EOI_IR1		0x01
#define SEND_EOI_IR2		0x02
#define SEND_EOI_IR3		0x03
#define SEND_EOI_IR4		0x04
#define SEND_EOI_IR5		0x05
#define SEND_EOI_IR6		0x06
#define SEND_EOI_IR7		0x07
#define OCW_TEMPLATE		0x08
#define SPECIAL_MASK		0x40
#define MASK_MDE_SET		0x20
#define MASK_MDE_RST		0x00
#define POLL_COMMAND		0x04
#define NO_POLL_CMND		0x00
#define READ_NEXT_RD		0x02
#define READ_IR_ONRD		0x00
#define READ_IS_ONRD		0x01
#define PIC_MASK_ZERO		0x00
#if !defined(__ASSEMBLER__) && !defined(APIC)
struct irqinfo {
unsigned char trigger;
unsigned char vector;
};
extern void picinit (void);
extern int curr_pic_mask;
extern void intnull(int unit);
extern void mask_irq (unsigned int irq_nr);
extern void unmask_irq (unsigned int irq_nr);
extern struct irqinfo irqinfo[];
#endif
#endif