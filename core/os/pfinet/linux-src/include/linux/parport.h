#ifndef _PARPORT_H_
#define _PARPORT_H_
#define PARPORT_MAX  8
#define PARPORT_IRQ_NONE  -1
#define PARPORT_DMA_NONE  -1
#define PARPORT_IRQ_AUTO  -2
#define PARPORT_DMA_AUTO  -2
#define PARPORT_DISABLE   -2
#define PARPORT_IRQ_PROBEONLY -3
#define PARPORT_CONTROL_STROBE    0x1
#define PARPORT_CONTROL_AUTOFD    0x2
#define PARPORT_CONTROL_INIT      0x4
#define PARPORT_CONTROL_SELECT    0x8
#define PARPORT_CONTROL_INTEN     0x10
#define PARPORT_CONTROL_DIRECTION 0x20
#define PARPORT_STATUS_ERROR      0x8
#define PARPORT_STATUS_SELECT     0x10
#define PARPORT_STATUS_PAPEROUT   0x20
#define PARPORT_STATUS_ACK        0x40
#define PARPORT_STATUS_BUSY       0x80
typedef enum {
PARPORT_CLASS_LEGACY = 0,
PARPORT_CLASS_PRINTER,
PARPORT_CLASS_MODEM,
PARPORT_CLASS_NET,
PARPORT_CLASS_HDC,
PARPORT_CLASS_PCMCIA,
PARPORT_CLASS_MEDIA,
PARPORT_CLASS_FDC,
PARPORT_CLASS_PORTS,
PARPORT_CLASS_SCANNER,
PARPORT_CLASS_DIGCAM,
PARPORT_CLASS_OTHER,
PARPORT_CLASS_UNSPEC
} parport_device_class;
#define PARPORT_MODE_PCSPP	        0x0001
#define PARPORT_MODE_PCPS2		0x0002
#define PARPORT_MODE_PCEPP		0x0004
#define PARPORT_MODE_PCECP		0x0008
#define PARPORT_MODE_PCECPEPP		0x0010
#define PARPORT_MODE_PCECR		0x0020
#define PARPORT_MODE_PCECPPS2		0x0040
#ifdef __KERNEL__
#include <asm/system.h>
#include <asm/ptrace.h>
#include <asm/spinlock.h>
#include <linux/proc_fs.h>
#include <linux/config.h>
#define PARPORT_NEED_GENERIC_OPS
struct parport;
struct pc_parport_state {
unsigned int ctr;
unsigned int ecr;
};
struct parport_state {
union {
struct pc_parport_state pc;
void *misc;
} u;
};
struct parport_operations {
void (*write_data)(struct parport *, unsigned char);
unsigned char (*read_data)(struct parport *);
void (*write_control)(struct parport *, unsigned char);
unsigned char (*read_control)(struct parport *);
unsigned char (*frob_control)(struct parport *, unsigned char mask, unsigned char val);
void (*write_econtrol)(struct parport *, unsigned char);
unsigned char (*read_econtrol)(struct parport *);
unsigned char (*frob_econtrol)(struct parport *, unsigned char mask, unsigned char val);
void (*write_status)(struct parport *, unsigned char);
unsigned char (*read_status)(struct parport *);
void (*write_fifo)(struct parport *, unsigned char);
unsigned char (*read_fifo)(struct parport *);
void (*change_mode)(struct parport *, int);
void (*release_resources)(struct parport *);
int (*claim_resources)(struct parport *);
void (*epp_write_data)(struct parport *, unsigned char);
unsigned char (*epp_read_data)(struct parport *);
void (*epp_write_addr)(struct parport *, unsigned char);
unsigned char (*epp_read_addr)(struct parport *);
int (*epp_check_timeout)(struct parport *);
size_t (*epp_write_block)(struct parport *, void *, size_t);
size_t (*epp_read_block)(struct parport *, void *, size_t);
int (*ecp_write_block)(struct parport *, void *, size_t, void (*fn)(struct parport *, void *, size_t), void *);
int (*ecp_read_block)(struct parport *, void *, size_t, void (*fn)(struct parport *, void *, size_t), void *);
void (*init_state)(struct parport_state *);
void (*save_state)(struct parport *, struct parport_state *);
void (*restore_state)(struct parport *, struct parport_state *);
void (*enable_irq)(struct parport *);
void (*disable_irq)(struct parport *);
void (*interrupt)(int, void *, struct pt_regs *);
void (*inc_use_count)(void);
void (*dec_use_count)(void);
void (*fill_inode)(struct inode *inode, int fill);
};
struct parport_device_info {
parport_device_class class;
const char *class_name;
const char *mfr;
const char *model;
const char *cmdset;
const char *description;
};
struct pardevice {
const char *name;
struct parport *port;
int (*preempt)(void *);
void (*wakeup)(void *);
void *private;
void (*irq_func)(int, void *, struct pt_regs *);
unsigned int flags;
struct pardevice *next;
struct pardevice *prev;
struct parport_state *state;
struct wait_queue *wait_q;
unsigned long int time;
unsigned long int timeslice;
unsigned int waiting;
struct pardevice *waitprev;
struct pardevice *waitnext;
};
struct parport_dir {
struct proc_dir_entry *entry;
struct proc_dir_entry *irq;
struct proc_dir_entry *devices;
struct proc_dir_entry *hardware;
struct proc_dir_entry *probe;
char name[4];
};
struct parport {
unsigned long base;
unsigned int size;
const char *name;
int irq;
int dma;
unsigned int modes;
struct pardevice *devices;
struct pardevice *cad;
struct pardevice *waithead;
struct pardevice *waittail;
struct parport *next;
unsigned int flags;
struct parport_dir pdir;
struct parport_device_info probe_info;
struct parport_operations *ops;
void *private_data;
int number;
spinlock_t pardevice_lock;
spinlock_t waitlist_lock;
rwlock_t cad_lock;
unsigned long base_hi;
};
struct parport *parport_register_port(unsigned long base, int irq, int dma,
struct parport_operations *ops);
extern void parport_unregister_port(struct parport *port);
#define parport_in_use(x)  ((x)->devices != NULL)
extern void parport_quiesce(struct parport *);
struct parport *parport_enumerate(void);
struct pardevice *parport_register_device(struct parport *port,
const char *name,
int (*pf)(void *), void (*kf)(void *),
void (*irq_func)(int, void *, struct pt_regs *),
int flags, void *handle);
extern void parport_unregister_device(struct pardevice *dev);
extern int parport_claim(struct pardevice *dev);
extern int parport_claim_or_block(struct pardevice *dev);
extern void parport_release(struct pardevice *dev);
static __inline__ int parport_yield(struct pardevice *dev)
{
unsigned long int timeslip = (jiffies - dev->time);
if ((dev->port->waithead == NULL) || (timeslip < dev->timeslice))
return 0;
parport_release(dev);
return parport_claim(dev);
}
static __inline__ int parport_yield_blocking(struct pardevice *dev)
{
unsigned long int timeslip = (jiffies - dev->time);
if ((dev->port->waithead == NULL) || (timeslip < dev->timeslice))
return 0;
parport_release(dev);
return parport_claim_or_block(dev);
}
static __inline__ void parport_generic_irq(int irq, struct parport *port,
struct pt_regs *regs)
{
read_lock(&port->cad_lock);
if (!port->cad)
goto out_unlock;
if (port->cad->irq_func)
port->cad->irq_func(irq, port->cad->private, regs);
else
printk(KERN_ERR "%s: irq%d happened with irq_func NULL "
"with %s as cad!\n", port->name, irq, port->cad->name);
out_unlock:
read_unlock(&port->cad_lock);
}
#define PARPORT_DEV_TRAN		0
#define PARPORT_DEV_LURK		(1<<0)
#define PARPORT_DEV_EXCL		(1<<1)
#define PARPORT_FLAG_COMA		(1<<0)
#define PARPORT_FLAG_EXCL		(1<<1)
extern void parport_parse_irqs(int, const char *[], int irqval[]);
extern int parport_ieee1284_nibble_mode_ok(struct parport *, unsigned char);
extern int parport_wait_peripheral(struct parport *, unsigned char, unsigned
char);
extern int parport_proc_init(void);
extern void parport_proc_cleanup(void);
extern int parport_proc_register(struct parport *pp);
extern int parport_proc_unregister(struct parport *pp);
extern void dec_parport_count(void);
extern void inc_parport_count(void);
extern int parport_probe(struct parport *port, char *buffer, int len);
extern void parport_probe_one(struct parport *port);
extern void (*parport_probe_hook)(struct parport *port);
#if (defined(CONFIG_PARPORT_PC) || defined(CONFIG_PARPORT_PC_MODULE)) && !(defined(CONFIG_PARPORT_AX) || defined(CONFIG_PARPORT_AX_MODULE)) && !(defined(CONFIG_PARPORT_ARC) || defined(CONFIG_PARPORT_ARC_MODULE)) && !defined(CONFIG_PARPORT_OTHER)
#undef PARPORT_NEED_GENERIC_OPS
#include <linux/parport_pc.h>
#define parport_write_data(p,x)            parport_pc_write_data(p,x)
#define parport_read_data(p)               parport_pc_read_data(p)
#define parport_write_control(p,x)         parport_pc_write_control(p,x)
#define parport_read_control(p)            parport_pc_read_control(p)
#define parport_frob_control(p,m,v)        parport_pc_frob_control(p,m,v)
#define parport_write_econtrol(p,x)        parport_pc_write_econtrol(p,x)
#define parport_read_econtrol(p)           parport_pc_read_econtrol(p)
#define parport_frob_econtrol(p,m,v)       parport_pc_frob_econtrol(p,m,v)
#define parport_write_status(p,v)          parport_pc_write_status(p,v)
#define parport_read_status(p)             parport_pc_read_status(p)
#define parport_write_fifo(p,v)            parport_pc_write_fifo(p,v)
#define parport_read_fifo(p)               parport_pc_read_fifo(p)
#define parport_change_mode(p,m)           parport_pc_change_mode(p,m)
#define parport_release_resources(p)       parport_pc_release_resources(p)
#define parport_claim_resources(p)         parport_pc_claim_resources(p)
#define parport_epp_write_data(p,x)        parport_pc_write_epp(p,x)
#define parport_epp_read_data(p)           parport_pc_read_epp(p)
#define parport_epp_write_addr(p,x)        parport_pc_write_epp_addr(p,x)
#define parport_epp_read_addr(p)           parport_pc_read_epp_addr(p)
#define parport_epp_check_timeout(p)       parport_pc_check_epp_timeout(p)
#endif
#ifdef PARPORT_NEED_GENERIC_OPS
#define parport_write_data(p,x)            (p)->ops->write_data(p,x)
#define parport_read_data(p)               (p)->ops->read_data(p)
#define parport_write_control(p,x)         (p)->ops->write_control(p,x)
#define parport_read_control(p)            (p)->ops->read_control(p)
#define parport_frob_control(p,m,v)        (p)->ops->frob_control(p,m,v)
#define parport_write_econtrol(p,x)        (p)->ops->write_econtrol(p,x)
#define parport_read_econtrol(p)           (p)->ops->read_econtrol(p)
#define parport_frob_econtrol(p,m,v)       (p)->ops->frob_econtrol(p,m,v)
#define parport_write_status(p,v)          (p)->ops->write_status(p,v)
#define parport_read_status(p)             (p)->ops->read_status(p)
#define parport_write_fifo(p,v)            (p)->ops->write_fifo(p,v)
#define parport_read_fifo(p)               (p)->ops->read_fifo(p)
#define parport_change_mode(p,m)           (p)->ops->change_mode(p,m)
#define parport_release_resources(p)       (p)->ops->release_resources(p)
#define parport_claim_resources(p)         (p)->ops->claim_resources(p)
#define parport_epp_write_data(p,x)        (p)->ops->epp_write_data(p,x)
#define parport_epp_read_data(p)           (p)->ops->epp_read_data(p)
#define parport_epp_write_addr(p,x)        (p)->ops->epp_write_addr(p,x)
#define parport_epp_read_addr(p)           (p)->ops->epp_read_addr(p)
#define parport_epp_check_timeout(p)       (p)->ops->epp_check_timeout(p)
#endif
#endif
#endif