#include <asm/uaccess.h>
#include <asm/system.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/mm.h>
static unsigned long net_rand_seed = 152L;
unsigned long net_random(void)
{
net_rand_seed=net_rand_seed*69069L+1;
return net_rand_seed^jiffies;
}
void net_srandom(unsigned long entropy)
{
net_rand_seed ^= entropy;
net_random();
}
int net_msg_cost = 5*HZ;
int net_msg_burst = 10*5*HZ;
int net_ratelimit(void)
{
static unsigned long toks = 10*5*HZ;
static unsigned long last_msg;
static int missed;
unsigned long now = jiffies;
toks += now - xchg(&last_msg, now);
if (toks > net_msg_burst)
toks = net_msg_burst;
if (toks >= net_msg_cost) {
toks -= net_msg_cost;
if (missed)
printk(KERN_WARNING "NET: %d messages suppressed.\n", missed);
missed = 0;
return 1;
}
missed++;
return 0;
}