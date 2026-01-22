#ifndef _LINUX_RANDOM_H
#define _LINUX_RANDOM_H
#include <linux/ioctl.h>
#define RNDGETENTCNT	_IOR( 'R', 0x00, int )
#define RNDADDTOENTCNT	_IOW( 'R', 0x01, int )
#define RNDGETPOOL	_IOR( 'R', 0x02, int [2] )
#define RNDADDENTROPY	_IOW( 'R', 0x03, int [2] )
#define RNDZAPENTCNT	_IO( 'R', 0x04 )
#define RNDCLEARPOOL	_IO( 'R', 0x06 )
struct rand_pool_info {
int	entropy_count;
int	buf_size;
__u32	buf[0];
};
#ifdef __KERNEL__
extern void rand_initialize(void);
extern void rand_initialize_irq(int irq);
extern void rand_initialize_blkdev(int irq, int mode);
extern void add_keyboard_randomness(unsigned char scancode);
extern void add_mouse_randomness(__u32 mouse_data);
extern void add_interrupt_randomness(int irq);
extern void add_blkdev_randomness(int major);
extern void get_random_bytes(void *buf, int nbytes);
extern __u32 secure_tcp_sequence_number(__u32 saddr, __u32 daddr,
__u16 sport, __u16 dport);
extern __u32 secure_tcp_syn_cookie(__u32 saddr, __u32 daddr,
__u16 sport, __u16 dport,
__u32 sseq, __u32 count,
__u32 data);
extern __u32 check_tcp_syn_cookie(__u32 cookie, __u32 saddr,
__u32 daddr, __u16 sport,
__u16 dport, __u32 sseq,
__u32 count, __u32 maxdiff);
#ifndef MODULE
extern struct file_operations random_fops, urandom_fops;
#endif
#endif
#endif