#ifndef gxiodev_INCLUDED
# define gxiodev_INCLUDED
#include "stat_.h"
#ifndef gx_io_device_DEFINED
# define gx_io_device_DEFINED
typedef struct gx_io_device_s gx_io_device;
#endif
typedef struct gx_io_device_procs_s gx_io_device_procs;
#ifndef file_enum_DEFINED
# define file_enum_DEFINED
struct file_enum_s;
typedef struct file_enum_s file_enum;
#endif
#ifndef gs_param_list_DEFINED
# define gs_param_list_DEFINED
typedef struct gs_param_list_s gs_param_list;
#endif
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
struct gx_io_device_procs_s {
#define iodev_proc_init(proc)\
int proc(gx_io_device *iodev, gs_memory_t *mem)
iodev_proc_init((*init));
#define iodev_proc_open_device(proc)\
int proc(gx_io_device *iodev, const char *access, stream **ps,\
gs_memory_t *mem)
iodev_proc_open_device((*open_device));
#define iodev_proc_open_file(proc)\
int proc(gx_io_device *iodev, const char *fname, uint namelen,\
const char *access, stream **ps, gs_memory_t *mem)
iodev_proc_open_file((*open_file));
#define iodev_proc_fopen(proc)\
int proc(gx_io_device *iodev, const char *fname, const char *access,\
FILE **pfile, char *rfname, uint rnamelen)
iodev_proc_fopen((*fopen));
#define iodev_proc_fclose(proc)\
int proc(gx_io_device *iodev, FILE *file)
iodev_proc_fclose((*fclose));
#define iodev_proc_delete_file(proc)\
int proc(gx_io_device *iodev, const char *fname)
iodev_proc_delete_file((*delete_file));
#define iodev_proc_rename_file(proc)\
int proc(gx_io_device *iodev, const char *from, const char *to)
iodev_proc_rename_file((*rename_file));
#define iodev_proc_file_status(proc)\
int proc(gx_io_device *iodev, const char *fname, struct stat *pstat)
iodev_proc_file_status((*file_status));
#define iodev_proc_enumerate_files(proc)\
file_enum *proc(gx_io_device *iodev, const char *pat, uint patlen,\
gs_memory_t *mem)
iodev_proc_enumerate_files((*enumerate_files));
#define iodev_proc_enumerate_next(proc)\
uint proc(file_enum *pfen, char *ptr, uint maxlen)
iodev_proc_enumerate_next((*enumerate_next));
#define iodev_proc_enumerate_close(proc)\
void proc(file_enum *pfen)
iodev_proc_enumerate_close((*enumerate_close));
#define iodev_proc_get_params(proc)\
int proc(gx_io_device *iodev, gs_param_list *plist)
iodev_proc_get_params((*get_params));
#define iodev_proc_put_params(proc)\
int proc(gx_io_device *iodev, gs_param_list *plist)
iodev_proc_put_params((*put_params));
};
typedef iodev_proc_fopen((*iodev_proc_fopen_t));
iodev_proc_init(iodev_no_init);
iodev_proc_open_device(iodev_no_open_device);
iodev_proc_open_file(iodev_no_open_file);
iodev_proc_fopen(iodev_no_fopen);
iodev_proc_fclose(iodev_no_fclose);
iodev_proc_delete_file(iodev_no_delete_file);
iodev_proc_rename_file(iodev_no_rename_file);
iodev_proc_file_status(iodev_no_file_status);
iodev_proc_enumerate_files(iodev_no_enumerate_files);
iodev_proc_get_params(iodev_no_get_params);
iodev_proc_put_params(iodev_no_put_params);
iodev_proc_fopen(iodev_os_fopen);
iodev_proc_fclose(iodev_os_fclose);
gx_io_device *gs_getiodevice(int);
#define iodev_default (gs_getiodevice(0))
gx_io_device *gs_findiodevice(const byte *, uint);
int gs_getdevparams(gx_io_device *, gs_param_list *);
int gs_putdevparams(gx_io_device *, gs_param_list *);
int gs_fopen_errno_to_code(int);
#define streq1(str, chr)\
((str)[0] == (chr) && (str)[1] == 0)
struct gx_io_device_s {
const char *dname;
const char *dtype;
gx_io_device_procs procs;
void *state;
};
#define private_st_io_device() \
gs_private_st_ptrs1(st_io_device, gx_io_device, "gx_io_device",\
io_device_enum_ptrs, io_device_reloc_ptrs, state)
#endif