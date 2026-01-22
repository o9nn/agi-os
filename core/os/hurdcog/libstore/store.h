#ifndef __STORE_H__
#define __STORE_H__
#include <sys/types.h>
#include <fcntl.h>
#include <mach.h>
#include <device/device.h>
#include <hurd/hurd_types.h>
#include <features.h>
#ifdef STORE_DEFINE_EI
#define STORE_EI
#else
#define STORE_EI __extern_inline
#endif
typedef off64_t store_offset_t;
struct store_run
{
store_offset_t start, length;
};
struct store
{
file_t source;
struct store_run *runs;
size_t num_runs;
store_offset_t end;
store_offset_t wrap_src;
store_offset_t wrap_dst;
char *name;
mach_port_t port;
size_t block_size;
store_offset_t blocks;
store_offset_t size;
unsigned log2_block_size;
unsigned log2_blocks_per_page;
int flags;
void *misc;
size_t misc_len;
const struct store_class *class;
struct store **children;
size_t num_children;
void *hook;
};
#define STORE_IMMUTABLE_FLAGS 0x00FF
#define STORE_READONLY 0x0100
#define STORE_NO_FILEIO 0x0200
#define STORE_GENERIC_FLAGS (STORE_READONLY | STORE_NO_FILEIO)
#define STORE_HARD_READONLY 0x1000
#define STORE_ENFORCED 0x2000
#define STORE_INACTIVE 0x4000
#define STORE_INNOCUOUS 0x8000
#define STORE_BACKEND_SPEC_BASE 0x10000
#define STORE_BACKEND_FLAGS (STORE_HARD_READONLY | STORE_ENFORCED \
| STORE_INACTIVE \
| ~(STORE_BACKEND_SPEC_BASE - 1))
typedef error_t (*store_write_meth_t)(struct store *store,
store_offset_t addr, size_t index,
const void *buf, size_t len,
size_t *amount);
typedef error_t (*store_read_meth_t)(struct store *store,
store_offset_t addr, size_t index,
size_t amount,
void **buf, size_t *len);
typedef error_t (*store_set_size_meth_t)(struct store *store,
size_t newsize);
struct store_enc;
struct store_class
{
enum file_storage_class id;
const char *name;
store_read_meth_t read;
store_write_meth_t write;
store_set_size_meth_t set_size;
error_t (*allocate_encoding)(const struct store *store,
struct store_enc *enc);
error_t (*encode) (const struct store *store, struct store_enc *enc);
error_t (*decode) (struct store_enc *enc,
const struct store_class *const *classes,
struct store **store);
error_t (*set_flags) (struct store *store, int flags);
error_t (*clear_flags) (struct store *store, int flags);
void (*cleanup) (struct store *store);
error_t (*clone) (const struct store *from, struct store *to);
error_t (*remap) (struct store *source,
const struct store_run *runs, size_t num_runs,
struct store **store);
error_t (*open) (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t (*validate_name) (const char *name,
const struct store_class *const *classes);
error_t (*map) (const struct store *store, vm_prot_t prot, mach_port_t *memobj);
};
error_t store_create (file_t source, int flags,
const struct store_class *const *classes,
struct store **store);
void store_free (struct store *store);
error_t store_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t
_store_create (const struct store_class *class, mach_port_t port,
int flags, size_t block_size,
const struct store_run *runs, size_t num_runs,
store_offset_t end, struct store **store);
error_t store_set_runs (struct store *store,
const struct store_run *runs, size_t num_runs);
error_t store_set_children (struct store *store,
struct store *const *children, size_t num_children);
error_t store_children_name (const struct store *store, char **name);
error_t store_set_name (struct store *store, const char *name);
error_t store_set_flags (struct store *store, int flags);
error_t store_clear_flags (struct store *store, int flags);
error_t store_set_child_flags (struct store *store, int flags);
error_t store_clear_child_flags (struct store *store, int flags);
extern int store_is_securely_returnable (struct store *store, int open_flags);
#if defined(__USE_EXTERN_INLINES) || defined(STORE_DEFINE_EI)
STORE_EI int
store_is_securely_returnable (struct store *store, int open_flags)
{
int flags = store->flags;
return
(flags & (STORE_INNOCUOUS | STORE_INACTIVE))
|| ((flags & STORE_ENFORCED)
&& (((open_flags & O_ACCMODE) == O_RDWR)
|| (flags & STORE_HARD_READONLY)));
}
#endif
void _store_derive (struct store *store);
error_t store_clone (struct store *from, struct store **to);
error_t store_remap (struct store *source,
const struct store_run *runs, size_t num_runs,
struct store **store);
error_t store_write (struct store *store,
store_offset_t addr, const void *buf, size_t len,
size_t *amount);
error_t store_read (struct store *store,
store_offset_t addr, size_t amount, void **buf, size_t *len);
error_t store_set_size (struct store *store, size_t newsize);
void store_close_source (struct store *store);
error_t store_map (const struct store *store, vm_prot_t prot,
mach_port_t *memobj);
#if 0
error_t store_create_pager (struct store *store, vm_prot_t prot, ...,
mach_port_t *memobj)
#endif
error_t store_zero_create (store_offset_t size, int flags, struct store **store);
error_t store_device_create (device_t device, int flags, struct store **store);
error_t _store_device_create (device_t device, int flags, size_t block_size,
const struct store_run *runs, size_t num_runs,
struct store **store);
error_t store_device_open (const char *name, int flags, struct store **store);
error_t store_part_create (struct store *source, int index, int flags,
struct store **store);
error_t store_part_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t store_file_create (file_t file, int flags, struct store **store);
error_t _store_file_create (file_t file, int flags, size_t block_size,
const struct store_run *runs, size_t num_runs,
struct store **store);
error_t store_file_open (const char *name, int flags, struct store **store);
error_t store_task_create (task_t task, int flags, struct store **store);
error_t _store_task_create (task_t task, int flags, size_t block_size,
const struct store_run *runs, size_t num_runs,
struct store **store);
error_t store_task_open (const char *name, int flags, struct store **store);
error_t store_memobj_create (memory_object_t memobj, int flags,
size_t block_size,
const struct store_run *runs, size_t num_runs,
struct store **store);
error_t store_nbd_open (const char *name, int flags, struct store **store);
error_t _store_nbd_create (mach_port_t port, int flags, size_t block_size,
const struct store_run *runs, size_t num_runs,
struct store **store);
error_t store_unknown_decode (struct store_enc *enc,
const struct store_class *const *classes,
struct store **store);
error_t store_ileave_create (struct store * const *stripes, size_t num_stripes,
store_offset_t interleave, int flags,
struct store **store);
error_t store_concat_create (struct store * const *stores, size_t num_stores,
int flags, struct store **store);
error_t store_concat_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t store_remap_create (struct store *source,
const struct store_run *runs, size_t num_runs,
int flags, struct store **store);
error_t store_copy_create (struct store *from, int flags, struct store **store);
error_t store_copy_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t store_buffer_create (void *buf, size_t buf_len, int flags,
struct store **store);
error_t store_gunzip_create (struct store *from, int flags,
struct store **store);
error_t store_gunzip_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t store_bunzip2_create (struct store *from, int flags,
struct store **store);
error_t store_bunzip2_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t store_mvol_create (struct store *phys,
error_t (*swap_vols) (struct store *store, size_t new_vol,
ssize_t old_vol),
int flags,
struct store **store);
const struct store_class *
store_find_class (const char *name,
const char *clname_end,
const struct store_class *const *classes);
error_t store_module_find_class (const char *name,
const char *clname_end,
const struct store_class **classp);
error_t store_typed_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t store_url_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t store_url_decode (struct store_enc *enc,
const struct store_class *const *classes,
struct store **store);
error_t store_module_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store);
error_t store_module_decode (struct store_enc *enc,
const struct store_class *const *classes,
struct store **store);
error_t store_open_children (const char *name, int flags,
const struct store_class *const *classes,
struct store ***stores, size_t *num_stores);
extern const struct store_class store_device_class;
extern const struct store_class store_part_class;
extern const struct store_class store_file_class;
extern const struct store_class store_task_class;
extern const struct store_class store_nbd_class;
extern const struct store_class store_memobj_class;
extern const struct store_class store_zero_class;
extern const struct store_class store_ileave_class;
extern const struct store_class store_concat_class;
extern const struct store_class store_remap_class;
extern const struct store_class store_query_class;
extern const struct store_class store_copy_class;
extern const struct store_class store_gunzip_class;
extern const struct store_class store_bunzip2_class;
extern const struct store_class store_typed_open_class;
extern const struct store_class store_url_open_class;
extern const struct store_class store_module_open_class;
extern const struct store_class store_unknown_class;
extern const struct store_class store_mvol_class;
#define STORE_STD_CLASS(name) \
static const struct store_class *const store_std_classes_##name[] \
__attribute_used__ __attribute__ ((section ("store_std_classes"))) \
= { &store_##name##_class }
extern const struct store_class *const __start_store_std_classes[] __attribute__ ((weak));
extern const struct store_class *const __stop_store_std_classes[] __attribute__ ((weak));
struct store_enc
{
mach_port_t *ports;
int *ints;
loff_t *offsets;
char *data;
mach_msg_type_number_t num_ports, num_ints, num_offsets, data_len;
size_t cur_port, cur_int, cur_offset, cur_data;
mach_port_t *init_ports;
int *init_ints;
loff_t *init_offsets;
char *init_data;
};
void store_enc_init (struct store_enc *enc,
mach_port_t *ports, mach_msg_type_number_t num_ports,
int *ints, mach_msg_type_number_t num_ints,
loff_t *offsets, mach_msg_type_number_t num_offsets,
char *data, mach_msg_type_number_t data_len);
void store_enc_dealloc (struct store_enc *enc);
void store_enc_return (struct store_enc *enc,
mach_port_t **ports, mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
loff_t **offsets, mach_msg_type_number_t *num_offsets,
char **data, mach_msg_type_number_t *data_len);
error_t store_return (const struct store *store,
mach_port_t **ports, mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
loff_t **offsets, mach_msg_type_number_t *num_offsets,
char **data, mach_msg_type_number_t *data_len);
error_t store_encode (const struct store *store, struct store_enc *enc);
error_t store_decode (struct store_enc *enc,
const struct store_class *const *classes,
struct store **store);
error_t store_allocate_child_encodings (const struct store *store,
struct store_enc *enc);
error_t store_encode_children (const struct store *store,
struct store_enc *enc);
error_t store_decode_children (struct store_enc *enc, int num_children,
const struct store_class *const *classes,
struct store **children);
error_t store_with_decoded_runs (struct store_enc *enc, size_t num_runs,
error_t (*fun) (const struct store_run *runs,
size_t num_runs));
error_t store_std_leaf_allocate_encoding (const struct store *store,
struct store_enc *enc);
error_t store_std_leaf_encode (const struct store *store,
struct store_enc *enc);
typedef error_t (*store_std_leaf_create_t)(mach_port_t port,
int flags,
size_t block_size,
const struct store_run *runs,
size_t num_runs,
struct store **store);
error_t store_std_leaf_decode (struct store_enc *enc,
store_std_leaf_create_t create,
struct store **store);
extern struct argp store_argp;
struct store_argp_params
{
struct store_parsed *result;
const char *default_type;
const struct store_class *const *classes;
int store_optional;
};
struct store_parsed;
void store_parsed_free (struct store_parsed *parsed);
error_t store_parsed_open (const struct store_parsed *parsed, int flags,
struct store **store);
error_t store_parsed_append_args (const struct store_parsed *parsed,
char **argz, size_t *argz_len);
error_t store_parsed_name (const struct store_parsed *parsed, char **name);
#endif