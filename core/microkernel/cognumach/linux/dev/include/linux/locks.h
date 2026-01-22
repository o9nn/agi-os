#ifndef _LINUX_LOCKS_H
#define _LINUX_LOCKS_H
#ifndef _LINUX_MM_H
#include <linux/mm.h>
#endif
#ifndef _LINUX_PAGEMAP_H
#include <linux/pagemap.h>
#endif
extern struct buffer_head *reuse_list;
extern void __wait_on_buffer(struct buffer_head *);
static inline void wait_on_buffer(struct buffer_head * bh)
{
if (test_bit(BH_Lock, &bh->b_state))
__wait_on_buffer(bh);
}
static inline void lock_buffer(struct buffer_head * bh)
{
while (set_bit(BH_Lock, &bh->b_state))
__wait_on_buffer(bh);
}
void unlock_buffer(struct buffer_head *);
#ifndef MACH
extern void __wait_on_super(struct super_block *);
static inline void wait_on_super(struct super_block * sb)
{
if (sb->s_lock)
__wait_on_super(sb);
}
static inline void lock_super(struct super_block * sb)
{
if (sb->s_lock)
__wait_on_super(sb);
sb->s_lock = 1;
}
static inline void unlock_super(struct super_block * sb)
{
sb->s_lock = 0;
wake_up(&sb->s_wait);
}
#endif
#endif