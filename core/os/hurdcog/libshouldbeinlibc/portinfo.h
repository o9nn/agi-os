#ifndef __PORTINFO_H__
#define __PORTINFO_H__
#include <stdio.h>
#include <errno.h>
#include <mach.h>
#include <portxlate.h>
#define PORTINFO_DETAILS	0x1
#define PORTINFO_MEMBERS	0x4
#define PORTINFO_HEX_NAMES	0x8
error_t print_port_info (mach_port_t name, mach_port_type_t type, task_t task,
unsigned show, FILE *stream);
error_t print_task_ports_info (task_t task, mach_port_type_t only,
unsigned show, FILE *stream);
error_t print_xlated_port_info (mach_port_t name, mach_port_type_t type,
struct port_name_xlator *x,
unsigned show, FILE *stream);
error_t print_xlated_task_ports_info (struct port_name_xlator *x,
mach_port_type_t only,
unsigned show, FILE *stream);
#endif