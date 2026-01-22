#ifndef _DEVICE_INPUT_H
#define _DEVICE_INPUT_H
#include <mach/boolean.h>
#include <mach/time_value.h>
#define	IOCPARM_MASK	0x1fff
#define	IOC_VOID	0x20000000
#define	IOC_OUT		0x40000000
#define	IOC_IN		0x80000000U
#define	IOC_INOUT	(IOC_IN|IOC_OUT)
#define _IOC(inout,group,num,len) \
(inout | ((len & IOCPARM_MASK) << 16) | ((group) << 8) | (num))
#define	_IO(g,n)	_IOC(IOC_VOID,	(g), (n), 0)
#define	_IOR(g,n,t)	_IOC(IOC_OUT,	(g), (n), sizeof(t))
#define	_IOW(g,n,t)	_IOC(IOC_IN,	(g), (n), sizeof(t))
#define	_IOWR(g,n,t)	_IOC(IOC_INOUT,	(g), (n), sizeof(t))
typedef uint8_t Scancode;
typedef uint16_t kev_type;
struct mouse_motion {
short mm_deltaX;
short mm_deltaY;
};
typedef struct {
kev_type type;
struct rpc_time_value unused_time;
union {
boolean_t up;
Scancode sc;
struct mouse_motion mmotion;
} value;
} kd_event;
#define m_deltaX	mmotion.mm_deltaX
#define m_deltaY	mmotion.mm_deltaY
#define MOUSE_LEFT	1
#define MOUSE_MIDDLE	2
#define MOUSE_RIGHT	3
#define MOUSE_MOTION	4
#define KEYBD_EVENT	5
#define KDSKBDMODE	_IOW('K', 1, int)
#define KB_EVENT	1
#define KB_ASCII	2
#define KDGKBDTYPE	_IOR('K', 2, int)
#define KB_VANILLAKB	0
#define KDSETLEDS	_IOW('K', 5, int)
#endif