#ifndef _LINUX_INIT_H
#define _LINUX_INIT_H
#ifndef MODULE
#include <asm/init.h>
#else
#define __init
#define __initdata
#define __initfunc(__arginit) __arginit
#define __INIT
#define __FINIT
#define __INITDATA
#endif
#if __GNUC__ >= 2 && __GNUC_MINOR__ >= 8
#define __initlocaldata __initdata
#else
#define __initlocaldata
#endif
#endif