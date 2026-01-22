#ifndef I2C_H
#define I2C_H
#define I2C_BUS_MAX       4
#define I2C_DRIVER_MAX    8
#define I2C_DEVICE_MAX    8
struct i2c_bus;
struct i2c_driver;
struct i2c_device;
#define I2C_DRIVERID_MSP3400    	 1
#define I2C_DRIVERID_TUNER      	 2
#define I2C_DRIVERID_VIDEOTEXT		 3
#define I2C_DRIVERID_VIDEODECODER	 4
#define I2C_DRIVERID_VIDEOENCODER	 5
#define I2C_BUSID_BT848		1
#define I2C_BUSID_BUZ		3
#define I2C_BUSID_ZORAN		4
#define I2C_BUSID_SGIVWFB	5
struct i2c_driver
{
char           name[32];
int            id;
unsigned char  addr_l, addr_h;
int (*attach)(struct i2c_device *device);
int (*detach)(struct i2c_device *device);
int (*command)(struct i2c_device *device,unsigned int cmd, void *arg);
struct i2c_device   *devices[I2C_DEVICE_MAX];
int                 devcount;
};
#include <linux/version.h>
#if LINUX_VERSION_CODE >= 0x020100
# if 0
#  define LOCK_FLAGS unsigned long flags;
#  define LOCK_I2C_BUS(bus)    spin_lock_irqsave(&(bus->bus_lock),flags);
#  define UNLOCK_I2C_BUS(bus)  spin_unlock_irqrestore(&(bus->bus_lock),flags);
# else
#  define LOCK_FLAGS
#  define LOCK_I2C_BUS(bus)    spin_lock(&(bus->bus_lock));
#  define UNLOCK_I2C_BUS(bus)  spin_unlock(&(bus->bus_lock));
# endif
#else
# define LOCK_FLAGS unsigned long flags;
# define LOCK_I2C_BUS(bus)    { save_flags(flags); cli(); }
# define UNLOCK_I2C_BUS(bus)  { restore_flags(flags);     }
#endif
struct i2c_bus
{
char  name[32];
int   id;
void  *data;
#if LINUX_VERSION_CODE >= 0x020100
spinlock_t bus_lock;
#endif
void    (*attach_inform)(struct i2c_bus *bus, int id);
void    (*detach_inform)(struct i2c_bus *bus, int id);
void    (*i2c_setlines)(struct i2c_bus *bus, int ctrl, int data);
int     (*i2c_getdataline)(struct i2c_bus *bus);
int     (*i2c_read)(struct i2c_bus *bus, unsigned char addr);
int     (*i2c_write)(struct i2c_bus *bus, unsigned char addr,
unsigned char b1, unsigned char b2, int both);
struct i2c_device   *devices[I2C_DEVICE_MAX];
int                 devcount;
};
struct i2c_device
{
char           name[32];
void           *data;
unsigned char  addr;
struct i2c_bus     *bus;
struct i2c_driver  *driver;
};
int i2c_register_bus(struct i2c_bus *bus);
int i2c_unregister_bus(struct i2c_bus *bus);
int i2c_register_driver(struct i2c_driver *driver);
int i2c_unregister_driver(struct i2c_driver *driver);
int i2c_control_device(struct i2c_bus *bus, int id,
unsigned int cmd, void *arg);
void    i2c_start(struct i2c_bus *bus);
void    i2c_stop(struct i2c_bus *bus);
void    i2c_one(struct i2c_bus *bus);
void    i2c_zero(struct i2c_bus *bus);
int     i2c_ack(struct i2c_bus *bus);
int     i2c_sendbyte(struct i2c_bus *bus,unsigned char data,int wait_for_ack);
unsigned char i2c_readbyte(struct i2c_bus *bus,int last);
int     i2c_read(struct i2c_bus *bus, unsigned char addr);
int     i2c_write(struct i2c_bus *bus, unsigned char addr,
unsigned char b1, unsigned char b2, int both);
int	i2c_init(void);
#endif