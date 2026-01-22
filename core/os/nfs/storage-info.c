#include "nfs.h"
#include <hurd/netfs.h>
#include <stdio.h>
error_t
netfs_file_get_storage_info (struct iouser *cred,
struct node *np,
mach_port_t **ports,
mach_msg_type_name_t *ports_type,
mach_msg_type_number_t *num_ports,
int **ints,
mach_msg_type_number_t *num_ints,
off_t **offsets,
mach_msg_type_number_t *num_offsets,
char **data,
mach_msg_type_number_t *data_len)
{
int name_len, fhpos;
error_t err;
inline int fmt (size_t buflen)
{
return snprintf (*data, buflen,
"nfsv%u:
protocol_version, mounted_hostname, mounted_nfs_port,
&fhpos, (int) (np->nn->handle.size * 2),
'X',
read_size, write_size);
}
err = netfs_validate_stat (np, cred);
if (err)
return err;
name_len = fmt (*data_len);
if (name_len < 0)
return errno;
++name_len;
if (name_len <= *data_len)
*data_len = name_len;
else
{
*data = mmap (0, name_len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*data == MAP_FAILED)
return errno;
*data_len = fmt (name_len) + 1;
assert_backtrace (*data_len == name_len);
}
{
static const char hexdigits[] = "0123456789abcdef";
size_t i;
for (i = 0; i < np->nn->handle.size; ++i)
{
(*data)[fhpos++] = hexdigits[(uint8_t)np->nn->handle.data[i] >> 4];
(*data)[fhpos++] = hexdigits[(uint8_t)np->nn->handle.data[i] & 0xf];
}
}
*num_ports = 0;
*ports_type = MACH_MSG_TYPE_COPY_SEND;
assert_backtrace (*num_offsets >= 2);
*num_offsets = 2;
(*offsets)[0] = 0;
(*offsets)[1] = np->nn_stat.st_size;
assert_backtrace (*num_ints >= 6);
*num_ints = 1;
(*ints)[0] = STORAGE_NETWORK;
(*ints)[1] = 0;
(*ints)[2] = 1;
(*ints)[3] = 1;
(*ints)[4] = name_len;
(*ints)[5] = 0;
return 0;
}