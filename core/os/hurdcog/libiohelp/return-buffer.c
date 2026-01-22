#include <string.h>
#include <mach.h>
#include <sys/mman.h>
#include <stdlib.h>
#include "iohelp.h"
error_t
iohelp_return_malloced_buffer (char *buf, size_t len,
char **rbuf, mach_msg_type_number_t *rlen)
{
error_t err = 0;
if (*rlen < len)
{
*rbuf = mmap (0, len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
err = (*rbuf == (char *) -1) ? errno : 0;
}
if (! err)
{
if (len)
memcpy (*rbuf, buf, len);
*rlen = len;
}
if (len > 0)
free (buf);
return err;
}