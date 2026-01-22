#include "errno_.h"
#include "pipe_.h"
#include "stdio_.h"
#include "string_.h"
#include "gserror.h"
#include "gserrors.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gxiodev.h"
private iodev_proc_fopen(pipe_fopen);
private iodev_proc_fclose(pipe_fclose);
const gx_io_device gs_iodev_pipe = {
"%pipe%", "Special",
{iodev_no_init, iodev_no_open_device,
NULL  , pipe_fopen, pipe_fclose,
iodev_no_delete_file, iodev_no_rename_file, iodev_no_file_status,
iodev_no_enumerate_files, NULL, NULL,
iodev_no_get_params, iodev_no_put_params
}
};
private int
pipe_fopen(gx_io_device * iodev, const char *fname, const char *access,
FILE ** pfile, char *rfname, uint rnamelen)
{
errno = 0;
if (strchr(access, '+'))
return_error(gs_error_invalidfileaccess);
*pfile = popen((char *)fname, (char *)access);
if (*pfile == NULL)
return_error(gs_fopen_errno_to_code(errno));
if (rfname != NULL)
strcpy(rfname, fname);
return 0;
}
private int
pipe_fclose(gx_io_device * iodev, FILE * file)
{
pclose(file);
return 0;
}