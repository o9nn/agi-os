#include <linux/config.h>
#include <asm/uaccess.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/mm.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/in.h>
#include <linux/errno.h>
#include <linux/interrupt.h>
#include <linux/if_ether.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/proc_fs.h>
#include <linux/init.h>
#include <net/ip.h>
#include <net/route.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/arp.h>
void dev_mc_upload(struct device *dev)
{
if(!(dev->flags&IFF_UP))
return;
if(dev->set_multicast_list==NULL)
return;
start_bh_atomic();
dev->set_multicast_list(dev);
end_bh_atomic();
}
int dev_mc_delete(struct device *dev, void *addr, int alen, int glbl)
{
int err = 0;
struct dev_mc_list *dmi, **dmip;
start_bh_atomic();
for (dmip=&dev->mc_list; (dmi=*dmip)!=NULL; dmip=&dmi->next) {
if (memcmp(dmi->dmi_addr,addr,dmi->dmi_addrlen)==0 && alen==dmi->dmi_addrlen) {
if (glbl) {
int old_glbl = dmi->dmi_gusers;
dmi->dmi_gusers = 0;
if (old_glbl == 0)
break;
}
if(--dmi->dmi_users)
goto done;
*dmip = dmi->next;
dev->mc_count--;
kfree_s(dmi,sizeof(*dmi));
end_bh_atomic();
dev_mc_upload(dev);
return 0;
}
}
err = -ENOENT;
done:
end_bh_atomic();
return err;
}
int dev_mc_add(struct device *dev, void *addr, int alen, int glbl)
{
int err = 0;
struct dev_mc_list *dmi, *dmi1;
dmi1 = (struct dev_mc_list *)kmalloc(sizeof(*dmi), gfp_any());
start_bh_atomic();
for(dmi=dev->mc_list; dmi!=NULL; dmi=dmi->next) {
if (memcmp(dmi->dmi_addr,addr,dmi->dmi_addrlen)==0 && dmi->dmi_addrlen==alen) {
if (glbl) {
int old_glbl = dmi->dmi_gusers;
dmi->dmi_gusers = 1;
if (old_glbl)
goto done;
}
dmi->dmi_users++;
goto done;
}
}
if ((dmi=dmi1)==NULL)
return -ENOMEM;
memcpy(dmi->dmi_addr, addr, alen);
dmi->dmi_addrlen=alen;
dmi->next=dev->mc_list;
dmi->dmi_users=1;
dmi->dmi_gusers=glbl ? 1 : 0;
dev->mc_list=dmi;
dev->mc_count++;
end_bh_atomic();
dev_mc_upload(dev);
return 0;
done:
end_bh_atomic();
if (dmi1)
kfree(dmi1);
return err;
}
void dev_mc_discard(struct device *dev)
{
start_bh_atomic();
while (dev->mc_list!=NULL) {
struct dev_mc_list *tmp=dev->mc_list;
dev->mc_list=tmp->next;
if (tmp->dmi_users > tmp->dmi_gusers)
printk("dev_mc_discard: multicast leakage! dmi_users=%d\n", tmp->dmi_users);
kfree_s(tmp,sizeof(*tmp));
}
dev->mc_count=0;
end_bh_atomic();
}
#ifdef CONFIG_PROC_FS
static int dev_mc_read_proc(char *buffer, char **start, off_t offset,
int length, int *eof, void *data)
{
off_t pos=0, begin=0;
struct dev_mc_list *m;
int len=0;
struct device *dev;
start_bh_atomic();
for (dev = dev_base; dev; dev = dev->next) {
for (m = dev->mc_list; m; m = m->next) {
int i;
len += sprintf(buffer+len,"%-4d %-15s %-5d %-5d ", dev->ifindex, dev->name,
m->dmi_users, m->dmi_gusers);
for (i=0; i<m->dmi_addrlen; i++)
len += sprintf(buffer+len, "%02x", m->dmi_addr[i]);
len+=sprintf(buffer+len, "\n");
pos=begin+len;
if (pos < offset) {
len=0;
begin=pos;
}
if (pos > offset+length)
goto done;
}
}
*eof = 1;
done:
end_bh_atomic();
*start=buffer+(offset-begin);
len-=(offset-begin);
if(len>length)
len=length;
if(len<0)
len=0;
return len;
}
#endif
__initfunc(void dev_mcast_init(void))
{
#ifdef CONFIG_PROC_FS
struct proc_dir_entry *ent;
ent = create_proc_entry("net/dev_mcast", 0, 0);
ent->read_proc = dev_mc_read_proc;
#endif
}