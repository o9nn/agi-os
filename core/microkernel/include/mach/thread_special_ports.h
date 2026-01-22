#ifndef	_MACH_THREAD_SPECIAL_PORTS_H_
#define _MACH_THREAD_SPECIAL_PORTS_H_
#define THREAD_KERNEL_PORT	1
#define THREAD_EXCEPTION_PORT	3
#define thread_get_kernel_port(thread, port)	\
(thread_get_special_port((thread), THREAD_KERNEL_PORT, (port)))
#define thread_set_kernel_port(thread, port)	\
(thread_set_special_port((thread), THREAD_KERNEL_PORT, (port)))
#define thread_get_exception_port(thread, port)	\
(thread_get_special_port((thread), THREAD_EXCEPTION_PORT, (port)))
#define thread_set_exception_port(thread, port)	\
(thread_set_special_port((thread), THREAD_EXCEPTION_PORT, (port)))
#endif