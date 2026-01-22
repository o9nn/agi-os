#include "std.h"
#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gxiodev.h"
#include "stream.h"
private iodev_proc_init(iodev_rom_init);
private iodev_proc_open_file(iodev_rom_open_file);
const gx_io_device gs_iodev_rom =
{
"%rom%", "FileSystem",
{iodev_rom_init, iodev_no_open_device,
iodev_rom_open_file,
iodev_no_fopen, iodev_no_fclose,
iodev_no_delete_file, iodev_no_rename_file,
iodev_no_file_status,
iodev_no_enumerate_files, NULL, NULL,
iodev_no_get_params, iodev_no_put_params
}
};
typedef struct romfs_state_s {
char *image;
} romfs_state;
gs_private_st_simple(st_romfs_state, struct romfs_state_s, "romfs_state");
private int
iodev_rom_init(gx_io_device *iodev, gs_memory_t *mem)
{
romfs_state *state = gs_alloc_struct(mem, romfs_state,
&st_romfs_state,
"iodev_rom_init(state)");
if (!state)
return gs_error_VMerror;
state->image = NULL;
return 0;
}
private int
iodev_rom_open_file(gx_io_device *iodev, const char *fname, uint namelen,
const char *access, stream **ps, gs_memory_t *mem)
{
const char* dummy = "this came from the compressed romfs.";
byte *buf;
*ps = NULL;
buf = gs_alloc_string(mem, strlen(dummy), "romfs buffer");
if (buf == NULL) {
if_debug0('s', "%rom%: could not allocate buffer\n");
return_error(gs_error_VMerror);
}
memcpy(buf, dummy, strlen(dummy));
*ps = s_alloc(mem, "romfs");
sread_string(*ps, buf, strlen(dummy));
return 0;
}