#ifndef gscdefs_INCLUDED
# define gscdefs_INCLUDED
#include "gconfigv.h"
#if SYSTEM_CONSTANTS_ARE_WRITABLE
# define CONFIG_CONST
#else
# define CONFIG_CONST const
#endif
extern CONFIG_CONST long gs_buildtime;
extern const char *CONFIG_CONST gs_copyright;
extern const char *CONFIG_CONST gs_product;
extern const char *CONFIG_CONST gs_productfamily;
extern CONFIG_CONST long gs_revision;
extern CONFIG_CONST long gs_revisiondate;
extern CONFIG_CONST long gs_serialnumber;
extern const char *const gs_doc_directory;
extern const char *const gs_lib_default_path;
extern const char *const gs_init_file;
#define extern_gx_device_halftone_list()\
typedef DEVICE_HALFTONE_RESOURCE_PROC((*gx_dht_proc));\
extern const gx_dht_proc gx_device_halftone_list[]
#define extern_gx_image_class_table()\
extern const gx_image_class_t gx_image_class_table[]
extern const unsigned gx_image_class_table_count;
#define extern_gx_image_type_table()\
extern const gx_image_type_t * const gx_image_type_table[]
extern const unsigned gx_image_type_table_count;
#define extern_gx_init_table()\
typedef init_proc((*gx_init_proc));\
extern const gx_init_proc gx_init_table[]
#define extern_gx_io_device_table()\
extern const gx_io_device * const gx_io_device_table[]
extern const unsigned gx_io_device_table_count;
#define extern_gs_lib_device_list()\
int gs_lib_device_list(const gx_device * const **plist,\
gs_memory_struct_type_t **pst)
#define extern_gs_find_compositor() \
const gs_composite_type_t * gs_find_compositor(int comp_id)
#endif