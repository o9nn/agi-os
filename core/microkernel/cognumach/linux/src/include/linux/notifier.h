#ifndef _LINUX_NOTIFIER_H
#define _LINUX_NOTIFIER_H
#include <linux/errno.h>
struct notifier_block
{
int (*notifier_call)(struct notifier_block *this, unsigned long, void *);
struct notifier_block *next;
int priority;
};
#ifdef __KERNEL__
#define NOTIFY_DONE 0x0000
#define NOTIFY_OK 0x0001
#define NOTIFY_STOP_MASK 0x8000
#define NOTIFY_BAD (NOTIFY_STOP_MASK|0x0002)
extern __inline__ int notifier_chain_register(struct notifier_block **list, struct notifier_block *n)
{
while(*list)
{
if(n->priority > (*list)->priority)
break;
list= &((*list)->next);
}
n->next = *list;
*list=n;
return 0;
}
extern __inline__ int notifier_chain_unregister(struct notifier_block **nl, struct notifier_block *n)
{
while((*nl)!=NULL)
{
if((*nl)==n)
{
*nl=n->next;
return 0;
}
nl=&((*nl)->next);
}
return -ENOENT;
}
extern __inline__ int notifier_call_chain(struct notifier_block **n, unsigned long val, void *v)
{
int ret=NOTIFY_DONE;
struct notifier_block *nb = *n;
while(nb)
{
ret=nb->notifier_call(nb,val,v);
if(ret&NOTIFY_STOP_MASK)
return ret;
nb=nb->next;
}
return ret;
}
#define NETDEV_UP 0x0001
#define NETDEV_DOWN 0x0002
#define NETDEV_REBOOT 0x0003
#endif
#endif