#ifndef COMPATMAC_H
#define COMPATMAC_H
#include <linux/version.h>
#include <asm/io.h>
#if LINUX_VERSION_CODE < 0x020100
#define TWO_ZERO
#else
#if LINUX_VERSION_CODE < 0x020200
#warning "Please use a 2.2.x kernel. "
#else
#if LINUX_VERSION_CODE < 0x020300
#define TWO_TWO
#else
#define TWO_THREE
#endif
#endif
#endif
#ifdef TWO_ZERO
#define MAX_SCHEDULE_TIMEOUT ((long)(~0UL>>1))
#include <linux/bios32.h>
#define Get_user(a,b) a = get_user(b)
#define Put_user(a,b) 0,put_user(a,b)
#define copy_to_user(a,b,c) memcpy_tofs(a,b,c)
static inline int copy_from_user(void *to,const void *from, int c)
{
memcpy_fromfs(to, from, c);
return 0;
}
#define pci_present pcibios_present
#define pci_read_config_word pcibios_read_config_word
#define pci_read_config_dword pcibios_read_config_dword
static inline unsigned char get_irq (unsigned char bus, unsigned char fn)
{
unsigned char t;
pcibios_read_config_byte (bus, fn, PCI_INTERRUPT_LINE, &t);
return t;
}
static inline void *ioremap(unsigned long base, long length)
{
if (base < 0x100000) return phys_to_virt(base);
return vremap (base, length);
}
#define my_iounmap(x, b) (((long)x<(long)phys_to_virt(0x100000))?0:vfree ((void*)x))
#define capable(x) suser()
#define queue_task queue_task_irq_off
#define tty_flip_buffer_push(tty) queue_task(&tty->flip.tqueue, &tq_timer)
#define signal_pending(current) (current->signal & ~current->blocked)
#define schedule_timeout(to) do {current->timeout = jiffies + (to);schedule ();} while (0)
#define time_after(t1,t2) (((long)t1-t2) > 0)
#define ASYNC_SPD_SHI -1
#define ASYNC_SPD_WARP -1
#define driver_name name
#define TTY_HW_COOK_OUT 14
#define TTY_HW_COOK_IN 15
#define INT void
#define NO_ERROR
#else
#include <asm/uaccess.h>
#define Get_user(a,b) get_user(a,b)
#define Put_user(a,b) put_user(a,b)
#define get_irq(pdev) pdev->irq
#define INT int
#define NO_ERROR 0
#define my_iounmap(x,b) (iounmap((char *)(b)))
#endif
#endif