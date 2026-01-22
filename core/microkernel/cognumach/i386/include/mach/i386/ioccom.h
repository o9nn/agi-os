#ifndef __sys_ioccom_h
#define __sys_ioccom_h
#define _IOCPARM_MASK 0xff
#define _IOC_VOID 0x20000000
#define _IOC_OUT 0x40000000
#define _IOC_IN 0x80000000
#define _IOC_INOUT (_IOC_IN|_IOC_OUT)
#define _IO(x,y) (_IOC_VOID|('x'<<8)|y)
#define _IOR(x,y,t) (_IOC_OUT|((sizeof(t)&_IOCPARM_MASK)<<16)|('x'<<8)|y)
#define _IORN(x,y,t) (_IOC_OUT|(((t)&_IOCPARM_MASK)<<16)|('x'<<8)|y)
#define _IOW(x,y,t) (_IOC_IN|((sizeof(t)&_IOCPARM_MASK)<<16)|('x'<<8)|y)
#define _IOWN(x,y,t) (_IOC_IN|(((t)&_IOCPARM_MASK)<<16)|('x'<<8)|y)
#define _IOWR(x,y,t) (_IOC_INOUT|((sizeof(t)&_IOCPARM_MASK)<<16)|('x'<<8)|y)
#define _IOWRN(x,y,t) (_IOC_INOUT|(((t)&_IOCPARM_MASK)<<16)|('x'<<8)|y)
#endif