#define MACH_INCLUDE
#include <linux/kernel.h>
#include <linux/errno.h>
#include <asm/dma.h>
#include <asm/system.h>
struct dma_chan
{
int  lock;
const char *device_id;
};
static struct dma_chan dma_chan_busy[MAX_DMA_CHANNELS] =
{
{ 0, 0 },
{ 0, 0 },
{ 0, 0 },
{ 0, 0 },
{ 1, "cascade" },
{ 0, 0 },
{ 0, 0 },
{ 0, 0 }
};
#ifndef MACH
int
get_dma_list (char *buf)
{
int i, len = 0;
for (i = 0 ; i < MAX_DMA_CHANNELS ; i++)
{
if (dma_chan_busy[i].lock)
{
len += linux_sprintf (buf+len, "%2d: %s\n",
i,
dma_chan_busy[i].device_id);
}
}
return len;
}
#endif
int
request_dma (unsigned int dmanr, const char *device_id)
{
if (dmanr >= MAX_DMA_CHANNELS)
return -EINVAL;
if (xchg (&dma_chan_busy[dmanr].lock, 1) != 0)
return -EBUSY;
dma_chan_busy[dmanr].device_id = device_id;
return 0;
}
void
free_dma (unsigned int dmanr)
{
if (dmanr >= MAX_DMA_CHANNELS)
{
printk ("Trying to free DMA%d\n", dmanr);
return;
}
if (xchg (&dma_chan_busy[dmanr].lock, 0) == 0)
{
printk ("Trying to free free DMA%d\n", dmanr);
return;
}
}