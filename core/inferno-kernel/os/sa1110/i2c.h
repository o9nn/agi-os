int i2c_write_byte(uchar addr, uchar data);
int i2c_read_byte(uchar addr, uchar *data);
void i2c_reset(void);
extern unsigned char i2c_iactl[];
int i2c_setpin(int b);
int i2c_clrpin(int b);
int i2c_getpin(int b);
extern int gpio_i2c_sda;
extern int gpio_i2c_scl;