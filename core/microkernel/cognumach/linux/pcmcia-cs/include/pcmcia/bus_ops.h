#ifndef _LINUX_BUS_OPS_H
#define _LINUX_BUS_OPS_H
#include <linux/config.h>
#ifdef CONFIG_VIRTUAL_BUS
typedef struct bus_operations {
void	*priv;
u32		(*b_in)(void *bus, u32 port, s32 sz);
void	(*b_ins)(void *bus, u32 port, void *buf,
u32 count, s32 sz);
void	(*b_out)(void *bus, u32 val, u32 port, s32 sz);
void	(*b_outs)(void *bus, u32 port, void *buf,
u32 count, s32 sz);
void	*(*b_ioremap)(void *bus, u_long ofs, u_long sz);
void	(*b_iounmap)(void *bus, void *addr);
u32		(*b_read)(void *bus, void *addr, s32 sz);
void	(*b_write)(void *bus, u32 val, void *addr, s32 sz);
void	(*b_copy_from)(void *bus, void *d, void *s, u32 count);
void	(*b_copy_to)(void *bus, void *d, void *s, u32 count);
int		(*b_request_irq)(void *bus, u_int irq,
void (*handler)(int, void *,
struct pt_regs *),
u_long flags, const char *device,
void *dev_id);
void	(*b_free_irq)(void *bus, u_int irq, void *dev_id);
} bus_operations;
#define bus_inb(b,p)		(b)->b_in((b),(p),0)
#define bus_inw(b,p)		(b)->b_in((b),(p),1)
#define bus_inl(b,p)		(b)->b_in((b),(p),2)
#define bus_inw_ns(b,p)		(b)->b_in((b),(p),-1)
#define bus_inl_ns(b,p)		(b)->b_in((b),(p),-2)
#define bus_insb(b,p,a,c)	(b)->b_ins((b),(p),(a),(c),0)
#define bus_insw(b,p,a,c)	(b)->b_ins((b),(p),(a),(c),1)
#define bus_insl(b,p,a,c)	(b)->b_ins((b),(p),(a),(c),2)
#define bus_insw_ns(b,p,a,c)	(b)->b_ins((b),(p),(a),(c),-1)
#define bus_insl_ns(b,p,a,c)	(b)->b_ins((b),(p),(a),(c),-2)
#define bus_outb(b,v,p)		(b)->b_out((b),(v),(p),0)
#define bus_outw(b,v,p)		(b)->b_out((b),(v),(p),1)
#define bus_outl(b,v,p)		(b)->b_out((b),(v),(p),2)
#define bus_outw_ns(b,v,p)	(b)->b_out((b),(v),(p),-1)
#define bus_outl_ns(b,v,p)	(b)->b_out((b),(v),(p),-2)
#define bus_outsb(b,p,a,c)	(b)->b_outs((b),(p),(a),(c),0)
#define bus_outsw(b,p,a,c)	(b)->b_outs((b),(p),(a),(c),1)
#define bus_outsl(b,p,a,c)	(b)->b_outs((b),(p),(a),(c),2)
#define bus_outsw_ns(b,p,a,c)	(b)->b_outs((b),(p),(a),(c),-1)
#define bus_outsl_ns(b,p,a,c)	(b)->b_outs((b),(p),(a),(c),-2)
#define bus_readb(b,a)		(b)->b_read((b),(a),0)
#define bus_readw(b,a)		(b)->b_read((b),(a),1)
#define bus_readl(b,a)		(b)->b_read((b),(a),2)
#define bus_readw_ns(b,a)	(b)->b_read((b),(a),-1)
#define bus_readl_ns(b,a)	(b)->b_read((b),(a),-2)
#define bus_writeb(b,v,a)	(b)->b_write((b),(v),(a),0)
#define bus_writew(b,v,a)	(b)->b_write((b),(v),(a),1)
#define bus_writel(b,v,a)	(b)->b_write((b),(v),(a),2)
#define bus_writew_ns(b,v,a)	(b)->b_write((b),(v),(a),-1)
#define bus_writel_ns(b,v,a)	(b)->b_write((b),(v),(a),-2)
#define bus_ioremap(b,s,n)	(b)->b_ioremap((b),(s),(n))
#define bus_iounmap(b,a)	(b)->b_iounmap((b),(a))
#define bus_memcpy_fromio(b,d,s,n) (b)->b_copy_from((b),(d),(s),(n))
#define bus_memcpy_toio(b,d,s,n) (b)->b_copy_to((b),(d),(s),(n))
#define bus_request_irq(b,i,h,f,n,d) \
(b)->b_request_irq((b),(i),(h),(f),(n),(d))
#define bus_free_irq(b,i,d)	(b)->b_free_irq((b),(i),(d))
#else
#define bus_inb(b,p)		inb(p)
#define bus_inw(b,p)		inw(p)
#define bus_inl(b,p)		inl(p)
#define bus_inw_ns(b,p)		inw_ns(p)
#define bus_inl_ns(b,p)		inl_ns(p)
#define bus_insb(b,p,a,c)	insb(p,a,c)
#define bus_insw(b,p,a,c)	insw(p,a,c)
#define bus_insl(b,p,a,c)	insl(p,a,c)
#define bus_insw_ns(b,p,a,c)	insw_ns(p,a,c)
#define bus_insl_ns(b,p,a,c)	insl_ns(p,a,c)
#define bus_outb(b,v,p)		outb(b,v,p)
#define bus_outw(b,v,p)		outw(b,v,p)
#define bus_outl(b,v,p)		outl(b,v,p)
#define bus_outw_ns(b,v,p)	outw_ns(b,v,p)
#define bus_outl_ns(b,v,p)	outl_ns(b,v,p)
#define bus_outsb(b,p,a,c)	outsb(p,a,c)
#define bus_outsw(b,p,a,c)	outsw(p,a,c)
#define bus_outsl(b,p,a,c)	outsl(p,a,c)
#define bus_outsw_ns(b,p,a,c)	outsw_ns(p,a,c)
#define bus_outsl_ns(b,p,a,c)	outsl_ns(p,a,c)
#define bus_readb(b,a)		readb(a)
#define bus_readw(b,a)		readw(a)
#define bus_readl(b,a)		readl(a)
#define bus_readw_ns(b,a)	readw_ns(a)
#define bus_readl_ns(b,a)	readl_ns(a)
#define bus_writeb(b,v,a)	writeb(v,a)
#define bus_writew(b,v,a)	writew(v,a)
#define bus_writel(b,v,a)	writel(v,a)
#define bus_writew_ns(b,v,a)	writew_ns(v,a)
#define bus_writel_ns(b,v,a)	writel_ns(v,a)
#define bus_ioremap(b,s,n)	ioremap(s,n)
#define bus_iounmap(b,a)	iounmap(a)
#define bus_memcpy_fromio(b,d,s,n) memcpy_fromio(d,s,n)
#define bus_memcpy_toio(b,d,s,n) memcpy_toio(d,s,n)
#ifdef CONFIG_8xx
#define bus_request_irq(b,i,h,f,n,d) request_8xxirq((i),(h),(f),(n),(d))
#else
#define bus_request_irq(b,i,h,f,n,d) request_irq((i),(h),(f),(n),(d))
#endif
#define bus_free_irq(b,i,d)	free_irq((i),(d))
#endif
#endif