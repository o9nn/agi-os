#ifndef _LPRREG_H_
#define _LPRREG_H_
#define DATA(addr)	(addr + 0)
#define STATUS(addr)	(addr + 1)
#define INTR_ENAB(addr)	(addr + 2)
extern void lprintr(int unit);
int lprprobe(vm_offset_t port, struct bus_ctlr *dev);
void lprstop(struct tty *tp, int flags);
void lprstart(struct tty *tp);
void lprattach(struct bus_device *dev);
extern io_return_t
lprgetstat(
dev_t		dev,
dev_flavor_t	flavor,
dev_status_t	data,
mach_msg_type_number_t	*count);
extern io_return_t
lprsetstat(
dev_t		dev,
dev_flavor_t	flavor,
dev_status_t	data,
mach_msg_type_number_t	count);
void lprpr_addr(unsigned short addr);
extern int lpropen(dev_t dev, int flag, io_req_t ior);
extern void lprclose(dev_t dev, int flag);
extern int lprread(dev_t dev, io_req_t ior);
extern int lprwrite(dev_t	dev, io_req_t ior);
extern int lprportdeath(dev_t dev, mach_port_t port);
#endif