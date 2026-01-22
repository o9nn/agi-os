#include "trivfs.h"
error_t
trivfs_create_control (mach_port_t underlying,
struct port_class *control_class,
struct port_bucket *control_bucket,
struct port_class *protid_class,
struct port_bucket *protid_bucket,
struct trivfs_control **control)
{
error_t err;
err = trivfs_add_control_port_class (&control_class);
if (! err)
err = trivfs_add_protid_port_class (&protid_class);
else
protid_class = 0;
if (! err)
err = trivfs_add_port_bucket (&control_bucket);
else
control_bucket = 0;
if (! err)
{
if (! protid_bucket)
protid_bucket = control_bucket;
err = trivfs_add_port_bucket (&protid_bucket);
}
else
protid_bucket = 0;
if (! err)
err = ports_create_port (control_class, control_bucket,
sizeof (struct trivfs_control), control);
if (! err)
{
(*control)->underlying = underlying;
(*control)->protid_class = protid_class;
(*control)->protid_bucket = protid_bucket;
err = mach_port_allocate (mach_task_self (), MACH_PORT_RIGHT_RECEIVE,
&(*control)->filesys_id);
if (err)
{
ports_port_deref (*control);
goto out;
}
err = mach_port_allocate (mach_task_self (), MACH_PORT_RIGHT_RECEIVE,
&(*control)->file_id);
if (err)
{
mach_port_destroy (mach_task_self (), (*control)->filesys_id);
ports_port_deref (*control);
goto out;
}
(*control)->hook = 0;
}
out:
if (err)
{
trivfs_remove_control_port_class (control_class);
trivfs_remove_protid_port_class (protid_class);
trivfs_remove_port_bucket (control_bucket);
trivfs_remove_port_bucket (protid_bucket);
}
return err;
}