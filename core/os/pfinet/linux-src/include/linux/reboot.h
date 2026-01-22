#ifndef _LINUX_REBOOT_H
#define _LINUX_REBOOT_H
#define LINUX_REBOOT_MAGIC1 0xfee1dead
#define LINUX_REBOOT_MAGIC2 672274793
#define LINUX_REBOOT_MAGIC2A 85072278
#define LINUX_REBOOT_MAGIC2B 369367448
#define LINUX_REBOOT_CMD_RESTART 0x01234567
#define LINUX_REBOOT_CMD_HALT 0xCDEF0123
#define LINUX_REBOOT_CMD_CAD_ON 0x89ABCDEF
#define LINUX_REBOOT_CMD_CAD_OFF 0x00000000
#define LINUX_REBOOT_CMD_POWER_OFF 0x4321FEDC
#define LINUX_REBOOT_CMD_RESTART2 0xA1B2C3D4
#ifdef __KERNEL__
#include <linux/notifier.h>
extern struct notifier_block *reboot_notifier_list;
extern int register_reboot_notifier(struct notifier_block *);
extern int unregister_reboot_notifier(struct notifier_block *);
extern void machine_restart(char *cmd);
extern void machine_halt(void);
extern void machine_power_off(void);
#endif
#endif