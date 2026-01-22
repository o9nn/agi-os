#ifndef _LINUX_APM_H
#define _LINUX_APM_H
typedef unsigned short apm_event_t;
typedef unsigned short apm_eventinfo_t;
#ifdef __KERNEL__
#define APM_40 0x40
#define APM_CS (APM_40 + 8)
#define APM_CS_16 (APM_CS + 8)
#define APM_DS (APM_CS_16 + 8)
struct apm_bios_info {
unsigned short version;
unsigned short cseg;
unsigned long offset;
unsigned short cseg_16;
unsigned short dseg;
unsigned short flags;
unsigned short cseg_len;
unsigned short cseg_16_len;
unsigned short dseg_len;
};
#define APM_16_BIT_SUPPORT 0x0001
#define APM_32_BIT_SUPPORT 0x0002
#define APM_IDLE_SLOWS_CLOCK 0x0004
#define APM_BIOS_DISABLED 0x0008
#define APM_BIOS_DISENGAGED 0x0010
#define APM_MAX_EVENTS 20
struct apm_bios_struct {
int magic;
struct apm_bios_struct * next;
int suser;
int suspends_pending;
int standbys_pending;
int suspends_read;
int standbys_read;
int event_head;
int event_tail;
apm_event_t events[APM_MAX_EVENTS];
};
#define APM_BIOS_MAGIC 0x4101
extern struct apm_bios_info apm_bios_info;
extern void apm_bios_init(void);
extern void apm_setup(char *, int *);
extern int apm_register_callback(int (*callback)(apm_event_t));
extern void apm_unregister_callback(int (*callback)(apm_event_t));
extern void apm_power_off(void);
extern int apm_display_blank(void);
extern int apm_display_unblank(void);
#endif
#define APM_STATE_READY 0x0000
#define APM_STATE_STANDBY 0x0001
#define APM_STATE_SUSPEND 0x0002
#define APM_STATE_OFF 0x0003
#define APM_STATE_BUSY 0x0004
#define APM_STATE_REJECT 0x0005
#define APM_SYS_STANDBY 0x0001
#define APM_SYS_SUSPEND 0x0002
#define APM_NORMAL_RESUME 0x0003
#define APM_CRITICAL_RESUME 0x0004
#define APM_LOW_BATTERY 0x0005
#define APM_POWER_STATUS_CHANGE 0x0006
#define APM_UPDATE_TIME 0x0007
#define APM_CRITICAL_SUSPEND 0x0008
#define APM_USER_STANDBY 0x0009
#define APM_USER_SUSPEND 0x000a
#define APM_STANDBY_RESUME 0x000b
#define APM_CAPABILITY_CHANGE 0x000c
#define APM_SUCCESS 0x00
#define APM_DISABLED 0x01
#define APM_CONNECTED 0x02
#define APM_NOT_CONNECTED 0x03
#define APM_16_CONNECTED 0x05
#define APM_16_UNSUPPORTED 0x06
#define APM_32_CONNECTED 0x07
#define APM_32_UNSUPPORTED 0x08
#define APM_BAD_DEVICE 0x09
#define APM_BAD_PARAM 0x0a
#define APM_NOT_ENGAGED 0x0b
#define APM_BAD_FUNCTION 0x0c
#define APM_RESUME_DISABLED 0x0d
#define APM_NO_ERROR 0x53
#define APM_BAD_STATE 0x60
#define APM_NO_EVENTS 0x80
#define APM_NOT_PRESENT 0x86
#include <linux/ioctl.h>
#define APM_IOC_STANDBY _IO('A', 1)
#define APM_IOC_SUSPEND _IO('A', 2)
#endif