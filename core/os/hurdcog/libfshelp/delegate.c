#include <errno.h>
#include <string.h>
#include <alloca.h>
#include <stdlib.h>
#include <argz.h>
#include <hurd.h>
#include <hurd/fsys.h>
#include <hurd/paths.h>
error_t
fshelp_delegate_translation (const char *server_name,
mach_port_t requestor, char **argv)
{
error_t err;
file_t server;
if (! server_name)
{
char *buf = alloca (strlen (argv[0]) + sizeof (_SERVERS));
strcpy (buf, _SERVERS);
strcat (buf, argv[0]);
server_name = buf;
}
server = file_name_lookup (server_name, 0, 0);
if (server != MACH_PORT_NULL)
{
char *argz;
size_t argz_len;
err = argz_create (argv, &argz, &argz_len);
if (!err)
{
err = fsys_forward (server,
requestor, MACH_MSG_TYPE_COPY_SEND,
argz, argz_len);
free (argz);
}
}
else
err = errno;
return err;
}