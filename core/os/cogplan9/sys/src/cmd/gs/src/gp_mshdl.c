#include "errno_.h"
#include "stdio_.h"
#include "string_.h"
#include "ctype_.h"
#include <io.h>
#include "gserror.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gxiodev.h"
private iodev_proc_fopen(mswin_handle_fopen);
private iodev_proc_fclose(mswin_handle_fclose);
const gx_io_device gs_iodev_handle = {
"%handle%", "FileSystem",
{iodev_no_init, iodev_no_open_device,
NULL  , mswin_handle_fopen, mswin_handle_fclose,
iodev_no_delete_file, iodev_no_rename_file, iodev_no_file_status,
iodev_no_enumerate_files, NULL, NULL,
iodev_no_get_params, iodev_no_put_params
}
};
#ifndef INVALID_HANDLE_VALUE
#define INVALID_HANDLE_VALUE (-1)
#endif
private long
get_os_handle(const char *name)
{
ulong hfile;
int i, ch;
for (i = 0; (ch = name[i]) != 0; ++i)
if (!isxdigit(ch))
return (long)INVALID_HANDLE_VALUE;
if (sscanf(name, "%lx", &hfile) != 1)
return (long)INVALID_HANDLE_VALUE;
return (long)hfile;
}
private int
mswin_handle_fopen(gx_io_device * iodev, const char *fname, const char *access,
FILE ** pfile, char *rfname, uint rnamelen)
{
int fd;
long hfile;
errno = 0;
if ((hfile = get_os_handle(fname)) == (long)INVALID_HANDLE_VALUE)
return_error(gs_fopen_errno_to_code(EBADF));
fd = _open_osfhandle((long)hfile, 0);
if (fd == -1)
return_error(gs_fopen_errno_to_code(EBADF));
*pfile = fdopen(fd, (char *)access);
if (*pfile == NULL)
return_error(gs_fopen_errno_to_code(errno));
if (rfname != NULL)
strcpy(rfname, fname);
return 0;
}
private int
mswin_handle_fclose(gx_io_device * iodev, FILE * file)
{
fclose(file);
return 0;
}