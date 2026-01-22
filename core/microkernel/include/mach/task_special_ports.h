#ifndef	_MACH_TASK_SPECIAL_PORTS_H_
#define _MACH_TASK_SPECIAL_PORTS_H_
#define TASK_KERNEL_PORT	1
#define TASK_EXCEPTION_PORT	3
#define TASK_BOOTSTRAP_PORT	4
#define task_get_kernel_port(task, port)	\
(task_get_special_port((task), TASK_KERNEL_PORT, (port)))
#define task_set_kernel_port(task, port)	\
(task_set_special_port((task), TASK_KERNEL_PORT, (port)))
#define task_get_exception_port(task, port)	\
(task_get_special_port((task), TASK_EXCEPTION_PORT, (port)))
#define task_set_exception_port(task, port)	\
(task_set_special_port((task), TASK_EXCEPTION_PORT, (port)))
#define task_get_bootstrap_port(task, port)	\
(task_get_special_port((task), TASK_BOOTSTRAP_PORT, (port)))
#define task_set_bootstrap_port(task, port)	\
(task_set_special_port((task), TASK_BOOTSTRAP_PORT, (port)))
#endif