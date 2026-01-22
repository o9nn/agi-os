#ifndef	_IPC_PORT_H_
#define _IPC_PORT_H_
#include <kern/debug.h>
#include <mach/port.h>
#if PORT_GENERATIONS
#define	MACH_PORT_INDEX(name)		((name) >> 8)
#define	MACH_PORT_GEN(name)		(((name) & 0xff) << 24)
#define	MACH_PORT_MAKE(index, gen)	(((index) << 8) | ((gen) >> 24))
#else
#define	MACH_PORT_INDEX(name)		(name)
#define	MACH_PORT_GEN(name)		0
#define	MACH_PORT_MAKE(index, gen)	(index)
#endif
#define	MACH_PORT_NGEN(name)		MACH_PORT_MAKE(0, MACH_PORT_GEN(name))
#define	MACH_PORT_MAKEB(index, bits)	MACH_PORT_MAKE(index, IE_BITS_GEN(bits))
typedef mach_port_name_t mach_port_gen_t;
#define	MACH_PORT_UREFS_MAX	((mach_port_urefs_t) ((1 << 16) - 1))
#define	MACH_PORT_UREFS_OVERFLOW(urefs, delta)				\
(((delta) > 0) &&					\
((((urefs) + (delta)) <= (urefs)) ||			\
(((urefs) + (delta)) > MACH_PORT_UREFS_MAX)))
#define	MACH_PORT_UREFS_UNDERFLOW(urefs, delta)				\
(((delta) < 0) && (-(delta) > (urefs)))
static inline mach_port_t invalid_name_to_port(mach_port_name_t name)
{
if (name == MACH_PORT_NAME_NULL)
return MACH_PORT_NULL;
if (name == MACH_PORT_NAME_DEAD)
return MACH_PORT_DEAD;
panic("invalid_name_to_port() called with a valid port");
}
static inline mach_port_name_t invalid_port_to_name(mach_port_t port)
{
if (port == MACH_PORT_NULL)
return MACH_PORT_NAME_NULL;
if (port == MACH_PORT_DEAD)
return MACH_PORT_NAME_DEAD;
panic("invalid_port_to_name() called with a valid name");
}
#endif