#ifndef __PORTXLATE_H__
#define __PORTXLATE_H__
#include <errno.h>
#include <mach.h>
struct port_name_xlator
{
mach_port_t from_task;
mach_port_t to_task;
int from_is_receive;
mach_port_t *to_names;
mach_msg_type_number_t to_names_len;
mach_port_type_t *to_types;
mach_msg_type_number_t to_types_len;
mach_port_t *ports;
};
error_t port_name_xlator_create (mach_port_t from_task, mach_port_t to_task,
struct port_name_xlator **xlator);
void port_name_xlator_free (struct port_name_xlator *x);
error_t port_name_xlator_xlate (struct port_name_xlator *x,
mach_port_t from, mach_port_type_t from_type,
mach_port_t *to, mach_port_type_t *to_type);
#endif