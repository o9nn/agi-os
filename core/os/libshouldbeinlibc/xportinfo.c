#include <sys/types.h>
#include <sys/mman.h>
#include "portinfo.h"
error_t
print_xlated_port_info (mach_port_t name, mach_port_type_t type,
struct port_name_xlator *x,
unsigned show, FILE *stream)
{
mach_port_t old_name = name;
error_t err = port_name_xlator_xlate (x, name, type, &name, &type);
if (! err)
{
fprintf (stream, (show & PORTINFO_HEX_NAMES) ? "%#6x => " : "%6u => ",
old_name);
err = print_port_info (name, type, x->to_task, show, stream);
}
return err;
}
error_t
print_xlated_task_ports_info (struct port_name_xlator *x,
mach_port_type_t only,
unsigned show, FILE *stream)
{
mach_port_t *names = 0;
mach_port_type_t *types = 0;
mach_msg_type_number_t names_len = 0, types_len = 0, i;
error_t err =
mach_port_names (x->from_task, &names, &names_len, &types, &types_len);
if (err)
return err;
for (i = 0; i < names_len; i++)
if (types[i] & only)
print_xlated_port_info (names[i], types[i], x, show, stream);
munmap ((caddr_t) names, names_len * sizeof *names);
munmap ((caddr_t) types, types_len * sizeof *types);
return 0;
}