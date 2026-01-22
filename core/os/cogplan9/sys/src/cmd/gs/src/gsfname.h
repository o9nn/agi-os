#ifndef gsfname_INCLUDED
# define gsfname_INCLUDED
#ifndef gx_io_device_DEFINED
# define gx_io_device_DEFINED
typedef struct gx_io_device_s gx_io_device;
#endif
typedef struct gs_parsed_file_name_s {
gs_memory_t *memory;
gx_io_device *iodev;
const char *fname;
uint len;
} gs_parsed_file_name_t;
int gs_parse_file_name(gs_parsed_file_name_t *, const char *, uint);
int gs_parse_real_file_name(gs_parsed_file_name_t *, const char *, uint,
gs_memory_t *, client_name_t);
int gs_terminate_file_name(gs_parsed_file_name_t *, gs_memory_t *,
client_name_t);
void gs_free_file_name(gs_parsed_file_name_t *, client_name_t);
#endif