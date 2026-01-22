#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "io.h"
#include "i2c.h"
static struct {
Lock;
ulong sda;
ulong scl;
} i2c;
static void
i2c_set(int pin)
{
GPIOREG->gpdr &= ~pin;
}
static void
i2c_clear(int pin)
{
GPIOREG->gpcr = pin;
GPIOREG->gpdr |= pin;
}
static int
i2c_getack(void)
{
i2c_set(i2c.sda);
timer_delay(US2TMR(3));
i2c_set(i2c.scl);
timer_delay(US2TMR(5));
if (GPIOREG->gplr & i2c.sda)
print("I2C: Warning did not get ack!\n");
i2c_clear(i2c.sda);
i2c_clear(i2c.scl);
timer_delay(US2TMR(3));
return 1;
}
static void
i2c_putack(void)
{
timer_delay(US2TMR(3));
i2c_clear(i2c.sda);
i2c_set(i2c.scl);
timer_delay(US2TMR(5));
i2c_clear(i2c.scl);
timer_delay(US2TMR(3));
}
static void
i2c_putbyte(uchar b)
{
uchar m;
for(m=0x80; m; m >>= 1) {
if(b&m)
i2c_set(i2c.sda);
else
i2c_clear(i2c.sda);
timer_delay(US2TMR(3));
i2c_set(i2c.scl);
timer_delay(US2TMR(5));
i2c_clear(i2c.scl);
timer_delay(US2TMR(3));
}
i2c_clear(i2c.sda);
}
static uchar
i2c_getbyte(void)
{
uchar data = 0x00;
int i;
i2c_set(i2c.sda);
for (i=7; i >= 0; i--) {
timer_delay(US2TMR(3));
i2c_set(i2c.scl);
timer_delay(US2TMR(5));
if(GPIOREG->gplr & i2c.sda)
data |= 1<<i;
i2c_clear(i2c.scl);
timer_delay(US2TMR(3));
}
i2c_clear(i2c.sda);
return data;
}
static int
i2c_start(void)
{
if ((GPIOREG->gplr & (i2c.sda | i2c.scl)) != (i2c.sda | i2c.scl))
print("I2C: Bus not clear when attempting start condition\n");
i2c_clear(i2c.sda);
timer_delay(US2TMR(5));
i2c_clear(i2c.scl);
timer_delay(US2TMR(3));
return 1;
}
static int
i2c_stop(void)
{
timer_delay(US2TMR(3));
i2c_set(i2c.scl);
timer_delay(US2TMR(5));
i2c_set(i2c.sda);
timer_delay(MS2TMR(1));
return 1;
}
int
i2c_write_byte(uchar addr, uchar data)
{
int rc = 0;
ilock(&i2c);
if(i2c_start() < 0)
rc = -1;
i2c_putbyte(addr & 0xfe);
if (i2c_getack() < 0)
rc = -2;
i2c_putbyte(data);
if (i2c_getack() < 0)
rc = -3;
if (i2c_stop() < 0)
rc = -4;
iunlock(&i2c);
return rc;
}
int
i2c_read_byte(uchar addr, uchar *data)
{
int rc = 0;
ilock(&i2c);
if(i2c_start() < 0)
rc = -1;
i2c_putbyte(addr | 0x01);
if(i2c_getack() < 0)
rc = -2;
*data = i2c_getbyte();
i2c_putack();
if (i2c_stop() < 0)
rc = -4;
iunlock(&i2c);
return rc;
}
void
i2c_reset(void)
{
i2c.sda = (1 << gpio_i2c_sda);
i2c.scl = (1 << gpio_i2c_scl);
i2c_set(i2c.sda);
i2c_set(i2c.scl);
timer_delay(MS2TMR(5));
}
uchar i2c_iactl[2] = { 0xff, 0xff };
int
i2c_setpin(int b)
{
int i = b>>3;
ilock(&i2c);
i2c_iactl[i] |= (1 << (b&7));
iunlock(&i2c);
return i2c_write_byte(0x40 | (i << 1), i2c_iactl[i]);
}
int
i2c_clrpin(int b)
{
int i = b>>3;
ilock(&i2c);
i2c_iactl[i] &= ~(1 << (b&7));
iunlock(&i2c);
return i2c_write_byte(0x40 | (i << 1), i2c_iactl[i]);
}
int
i2c_getpin(int b)
{
return (i2c_iactl[(b>>3)&1] & (1<<(b&7))) != 0;
}