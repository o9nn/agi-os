#include <sys/types.h>
#include <string.h>
#include <device/conf.h>
#include <device/ds_routines.h>
#include <device/io_req.h>
#include <mach/boolean.h>
#include <kern/lock.h>
#include <device/kmsg.h>
#define KMSGBUFSIZE	(4096)
static char kmsg_buffer[KMSGBUFSIZE];
static int kmsg_write_offset;
static int kmsg_read_offset;
static queue_head_t kmsg_read_queue;
static boolean_t kmsg_in_use;
def_simple_lock_irq_data (static, kmsg_lock);
static boolean_t kmsg_init_done = FALSE;
static void
kmsginit (void)
{
kmsg_write_offset = 0;
kmsg_read_offset = 0;
queue_init (&kmsg_read_queue);
kmsg_in_use = FALSE;
simple_lock_init_irq (&kmsg_lock);
}
io_return_t
kmsgopen (dev_t dev, int flag, const io_req_t ior)
{
spl_t s = simple_lock_irq (&kmsg_lock);
if (kmsg_in_use)
{
simple_unlock_irq (s, &kmsg_lock);
return D_ALREADY_OPEN;
}
kmsg_in_use = TRUE;
simple_unlock_irq (s, &kmsg_lock);
return D_SUCCESS;
}
void
kmsgclose (dev_t dev, int flag)
{
spl_t s = simple_lock_irq (&kmsg_lock);
kmsg_in_use = FALSE;
simple_unlock_irq (s, &kmsg_lock);
}
static boolean_t kmsg_read_done (io_req_t ior);
io_return_t
kmsgread (dev_t dev, io_req_t ior)
{
int err;
int amt, len;
err = device_read_alloc (ior, ior->io_count);
if (err != KERN_SUCCESS)
return err;
spl_t s = simple_lock_irq (&kmsg_lock);
if (kmsg_read_offset == kmsg_write_offset)
{
if (ior->io_mode & D_NOWAIT)
{
simple_unlock_irq (s, &kmsg_lock);
return D_WOULD_BLOCK;
}
ior->io_done = kmsg_read_done;
enqueue_tail (&kmsg_read_queue, (queue_entry_t) ior);
simple_unlock_irq (s, &kmsg_lock);
return D_IO_QUEUED;
}
len = kmsg_write_offset - kmsg_read_offset;
if (len < 0)
len += KMSGBUFSIZE;
amt = ior->io_count;
if (amt > len)
amt = len;
if (kmsg_read_offset + amt <= KMSGBUFSIZE)
{
memcpy (ior->io_data, kmsg_buffer + kmsg_read_offset, amt);
}
else
{
int cnt;
cnt = KMSGBUFSIZE - kmsg_read_offset;
memcpy (ior->io_data, kmsg_buffer + kmsg_read_offset, cnt);
memcpy (ior->io_data + cnt, kmsg_buffer, amt - cnt);
}
kmsg_read_offset += amt;
if (kmsg_read_offset >= KMSGBUFSIZE)
kmsg_read_offset -= KMSGBUFSIZE;
ior->io_residual = ior->io_count - amt;
simple_unlock_irq (s, &kmsg_lock);
return D_SUCCESS;
}
static boolean_t
kmsg_read_done (io_req_t ior)
{
int amt, len;
spl_t s = simple_lock_irq (&kmsg_lock);
if (kmsg_read_offset == kmsg_write_offset)
{
ior->io_done = kmsg_read_done;
enqueue_tail (&kmsg_read_queue, (queue_entry_t) ior);
simple_unlock_irq (s, &kmsg_lock);
return FALSE;
}
len = kmsg_write_offset - kmsg_read_offset;
if (len < 0)
len += KMSGBUFSIZE;
amt = ior->io_count;
if (amt > len)
amt = len;
if (kmsg_read_offset + amt <= KMSGBUFSIZE)
{
memcpy (ior->io_data, kmsg_buffer + kmsg_read_offset, amt);
}
else
{
int cnt;
cnt = KMSGBUFSIZE - kmsg_read_offset;
memcpy (ior->io_data, kmsg_buffer + kmsg_read_offset, cnt);
memcpy (ior->io_data + cnt, kmsg_buffer, amt - cnt);
}
kmsg_read_offset += amt;
if (kmsg_read_offset >= KMSGBUFSIZE)
kmsg_read_offset -= KMSGBUFSIZE;
ior->io_residual = ior->io_count - amt;
simple_unlock_irq (s, &kmsg_lock);
ds_read_done (ior);
return TRUE;
}
io_return_t
kmsggetstat (dev_t dev, dev_flavor_t flavor, dev_status_t data, mach_msg_type_number_t *count)
{
switch (flavor)
{
case DEV_GET_SIZE:
data[DEV_GET_SIZE_DEVICE_SIZE] = 0;
data[DEV_GET_SIZE_RECORD_SIZE] = 1;
*count = DEV_GET_SIZE_COUNT;
break;
default:
return D_INVALID_OPERATION;
}
return D_SUCCESS;
}
void
kmsg_putchar (int c)
{
io_req_t ior;
int offset;
spl_t s = -1;
if (!kmsg_init_done)
{
kmsginit ();
kmsg_init_done = TRUE;
}
if (spl_init)
s = simple_lock_irq (&kmsg_lock);
offset = kmsg_write_offset + 1;
if (offset == KMSGBUFSIZE)
offset = 0;
if (offset == kmsg_read_offset)
{
if (spl_init)
simple_unlock_irq (s, &kmsg_lock);
return;
}
kmsg_buffer[kmsg_write_offset++] = c;
if (kmsg_write_offset == KMSGBUFSIZE)
kmsg_write_offset = 0;
while ((ior = (io_req_t) dequeue_head (&kmsg_read_queue)) != NULL)
iodone (ior);
if (spl_init)
simple_unlock_irq (s, &kmsg_lock);
}