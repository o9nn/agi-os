#ifndef _INPUTDEV_H_
#define _INPUTDEV_H_ 1
#include <trans.h>
typedef u_short kev_type;
struct mouse_motion {
short mm_deltaX;
short mm_deltaY;
};
typedef u_char Scancode;
typedef struct {
kev_type type;
struct timeval time;
union {
boolean_t up;
Scancode sc;
struct mouse_motion mmotion;
} value;
} kd_event;
#define m_deltaX mmotion.mm_deltaX
#define m_deltaY mmotion.mm_deltaY
#define MOUSE_LEFT 1
#define MOUSE_MIDDLE 2
#define MOUSE_RIGHT 3
#define MOUSE_MOTION 4
#define KEYBD_EVENT 5
#define IOCPARM_MASK 0x1fff
#define IOC_OUT 0x40000000
#define IOC_IN 0x80000000U
#ifndef _IOC
#define _IOC(inout,group,num,len) \
(inout | ((len & IOCPARM_MASK) << 16) | ((group) << 8) | (num))
#endif
#ifndef _IOR
#define _IOR(g,n,t) _IOC(IOC_OUT, (g), (n), sizeof(t))
#endif
#ifndef _IOW
#define _IOW(g,n,t) _IOC(IOC_IN, (g), (n), sizeof(t))
#endif
#define KDSKBDMODE _IOW('K', 1, int)
#define KB_EVENT 1
#define KB_ASCII 2
#define KDGKBDTYPE _IOR('K', 2, int)
#define KB_VANILLAKB 0
#define KDSETLEDS _IOW('K', 5, int)
#define MOUSE_SYSTEM_MOUSE 0
#define MICROSOFT_MOUSE 1
#define IBM_MOUSE 2
#define NO_MOUSE 3
#define LOGITECH_TRACKMAN 4
#define MICROSOFT_MOUSE7 5
#define DEV_COM0 "com0"
#define DEV_COM1 "com1"
extern int kbd_repeater_opened;
void kbd_repeat_key (kd_event *key);
error_t kbd_setrepeater (const char *nodename, consnode_t *node);
#endif