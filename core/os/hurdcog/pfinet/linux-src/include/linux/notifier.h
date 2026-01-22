#ifndef _LINUX_NOTIFIER_H
#define _LINUX_NOTIFIER_H
#include <linux/errno.h>
struct notifier_block
{
int (*notifier_call)(struct notifier_block *self, unsigned long, void *);
struct notifier_block *next;
int priority;
};
#ifdef __KERNEL__
#define NOTIFY_DONE 0x0000
#define NOTIFY_OK 0x0001
#define NOTIFY_STOP_MASK 0x8000
#define NOTIFY_BAD (NOTIFY_STOP_MASK|0x0002)
static __inline__ int notifier_chain_register(struct notifier_block **list, struct notifier_block *n)
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
static __inline__ int notifier_chain_unregister(struct notifier_block **nl, struct notifier_block *n)
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
static __inline__ int notifier_call_chain(struct notifier_block **n, unsigned long val, void *v)
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
#define NETDEV_CHANGE 0x0004
#define NETDEV_REGISTER 0x0005
#define NETDEV_UNREGISTER 0x0006
#define NETDEV_CHANGEMTU 0x0007
#define NETDEV_CHANGEADDR 0x0008
#define NETDEV_GOING_DOWN 0x0009
#define NETDEV_CHANGENAME 0x000A
#define SYS_DOWN 0x0001
#define SYS_RESTART SYS_DOWN
#define SYS_HALT 0x0002
#define SYS_POWER_OFF 0x0003
extern struct notifier_block *boot_notifier_list;
#endif
#endif