#include <string.h>
#include <sys/mman.h>
#include "store.h"
void
store_enc_init (struct store_enc *enc,
mach_port_t *ports, mach_msg_type_number_t num_ports,
int *ints, mach_msg_type_number_t num_ints,
off_t *offsets, mach_msg_type_number_t num_offsets,
char *data, mach_msg_type_number_t data_len)
{
memset (enc, 0, sizeof(*enc));
enc->ports = enc->init_ports = ports;
enc->num_ports = num_ports;
enc->ints = enc->init_ints = ints;
enc->num_ints = num_ints;
enc->offsets = enc->init_offsets = offsets;
enc->num_offsets = num_offsets;
enc->data = enc->init_data = data;
enc->data_len = data_len;
}
void
store_enc_dealloc (struct store_enc *enc)
{
if (enc->ports && enc->num_ports > 0)
{
while (enc->cur_port < enc->num_ports)
{
mach_port_t port = enc->ports[enc->cur_port++];
if (port != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), port);
}
if (enc->ports != enc->init_ports)
munmap ((caddr_t) enc->ports, enc->num_ports * sizeof (*enc->ports));
}
if (enc->ints && enc->num_ints > 0 && enc->ints != enc->init_ints)
munmap ((caddr_t) enc->ints, enc->num_ints * sizeof (*enc->ints));
if (enc->offsets && enc->num_offsets > 0
&& enc->offsets != enc->init_offsets)
munmap ((caddr_t) enc->offsets, enc->num_offsets * sizeof (*enc->offsets));
if (enc->data && enc->data_len > 0 && enc->data != enc->init_data)
munmap (enc->data, enc->data_len);
memset (enc, 0, sizeof(*enc));
}
void
store_enc_return (struct store_enc *enc,
mach_port_t **ports, mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
off_t **offsets, mach_msg_type_number_t *num_offsets,
char **data, mach_msg_type_number_t *data_len)
{
*ports = enc->ports;
*num_ports = enc->num_ports;
*ints = enc->ints;
*num_ints = enc->num_ints;
*offsets = enc->offsets;
*num_offsets = enc->num_offsets;
*data = enc->data;
*data_len = enc->data_len;
}