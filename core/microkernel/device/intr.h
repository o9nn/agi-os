#ifndef __INTR_H__
#define __INTR_H__
#ifndef MACH_XEN
#include <mach/kern_return.h>
#include <mach/port.h>
#include <kern/queue.h>
#include <ipc/ipc_port.h>
#include <device/conf.h>
#define DEVICE_NOTIFY_MSGH_SEQNO 0
#include <sys/types.h>
struct irqdev;
#include <machine/irq.h>
typedef struct {
queue_chain_t chain;
int interrupts;
int n_unacked;
ipc_port_t dst_port;
int id;
} user_intr_t;
struct irqdev {
char *name;
void (*irqdev_ack)(struct irqdev *dev, int id);
queue_head_t *intr_queue;
int tot_num_intr;
irq_t irq[NINTR];
};
extern queue_head_t main_intr_queue;
extern int install_user_intr_handler (struct irqdev *dev, int id, unsigned long flags, user_intr_t *e);
extern int deliver_user_intr (struct irqdev *dev, int id, user_intr_t *e);
extern user_intr_t *insert_intr_entry (struct irqdev *dev, int id, ipc_port_t receive_port);
void intr_thread (void);
kern_return_t irq_acknowledge (ipc_port_t receive_port);
#endif
#endif