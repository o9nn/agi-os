#ifndef FS_API_H
#define FS_API_H
struct stat;
struct fs;
struct fs_file;
struct fs_lock;
struct hash_method;
#define FS_METADATA_INTERNAL_PREFIX ":/X-Dovecot-fs-api-"
#define FS_METADATA_OBJECTID FS_METADATA_INTERNAL_PREFIX"ObjectID"
#define FS_METADATA_WRITE_FNAME FS_METADATA_INTERNAL_PREFIX"WriteFilename"
#define FS_METADATA_ORIG_PATH FS_METADATA_INTERNAL_PREFIX"OrigPath"
#define FS_METADATA_FILE_SIZE FS_METADATA_INTERNAL_PREFIX"Size"
enum fs_properties {
FS_PROPERTY_METADATA = 0x01,
FS_PROPERTY_LOCKS = 0x02,
FS_PROPERTY_FASTCOPY = 0x04,
FS_PROPERTY_RENAME = 0x08,
FS_PROPERTY_STAT = 0x10,
FS_PROPERTY_ITER = 0x20,
FS_PROPERTY_RELIABLEITER= 0x40,
FS_PROPERTY_DIRECTORIES = 0x80,
FS_PROPERTY_WRITE_HASH_MD5 = 0x100,
FS_PROPERTY_WRITE_HASH_SHA256 = 0x200,
FS_PROPERTY_COPY_METADATA = 0x400,
FS_PROPERTY_ASYNC = 0x800,
FS_PROPERTY_OBJECTIDS = 0x1000,
FS_PROPERTY_FASTCOPY_CHANGED_METADATA = 0x2000,
};
enum fs_open_mode {
FS_OPEN_MODE_READONLY,
FS_OPEN_MODE_CREATE,
FS_OPEN_MODE_CREATE_UNIQUE_128,
FS_OPEN_MODE_REPLACE,
FS_OPEN_MODE_APPEND
#define FS_OPEN_MODE_MASK 0x0f
};
enum fs_open_flags {
FS_OPEN_FLAG_FSYNC = 0x10,
FS_OPEN_FLAG_ASYNC = 0x20,
FS_OPEN_FLAG_SEEKABLE = 0x40,
FS_OPEN_FLAG_ASYNC_NOQUEUE = 0x80
};
enum fs_iter_flags {
FS_ITER_FLAG_DIRS = 0x01,
FS_ITER_FLAG_ASYNC = 0x02,
FS_ITER_FLAG_OBJECTIDS = 0x04,
FS_ITER_FLAG_NOCACHE = 0x08
};
enum fs_op {
FS_OP_WAIT,
FS_OP_METADATA,
FS_OP_PREFETCH,
FS_OP_READ,
FS_OP_WRITE,
FS_OP_LOCK,
FS_OP_EXISTS,
FS_OP_STAT,
FS_OP_COPY,
FS_OP_RENAME,
FS_OP_DELETE,
FS_OP_ITER,
FS_OP_COUNT
};
struct fs_settings {
const char *username;
const char *session_id;
const char *base_dir;
const char *temp_dir;
const struct ssl_iostream_settings *ssl_client_set;
const char *root_path;
const char *temp_file_prefix;
struct dns_client *dns_client;
struct event *event_parent;
bool debug;
bool enable_timing;
};
struct fs_stats {
unsigned int prefetch_count;
unsigned int read_count;
unsigned int lookup_metadata_count;
unsigned int stat_count;
unsigned int write_count;
unsigned int exists_count;
unsigned int delete_count;
unsigned int copy_count;
unsigned int rename_count;
unsigned int iter_count;
uint64_t write_bytes;
struct stats_dist *timings[FS_OP_COUNT];
};
struct fs_metadata {
const char *key;
const char *value;
};
ARRAY_DEFINE_TYPE(fs_metadata, struct fs_metadata);
typedef void fs_file_async_callback_t(void *context);
int fs_init(const char *driver, const char *args,
const struct fs_settings *set,
struct fs **fs_r, const char **error_r);
int fs_init_from_string(const char *str, const struct fs_settings *set,
struct fs **fs_r, const char **error_r);
void fs_deinit(struct fs **fs);
void fs_ref(struct fs *fs);
void fs_unref(struct fs **fs);
struct fs *fs_get_parent(struct fs *fs);
const char *fs_get_driver(struct fs *fs);
const char *fs_get_root_driver(struct fs *fs);
struct event *fs_get_event(struct fs *fs);
struct fs_file *fs_file_init(struct fs *fs, const char *path, int mode_flags);
struct fs_file *fs_file_init_with_event(struct fs *fs, struct event *event,
const char *path, int mode_flags);
void fs_file_deinit(struct fs_file **file);
void fs_file_set_flags(struct fs_file *file,
enum fs_open_flags add_flags,
enum fs_open_flags remove_flags);
void fs_file_close(struct fs_file *file);
enum fs_properties fs_get_properties(struct fs *fs);
void fs_set_metadata(struct fs_file *file, const char *key, const char *value);
int fs_get_metadata(struct fs_file *file,
const ARRAY_TYPE(fs_metadata) **metadata_r);
int fs_lookup_metadata(struct fs_file *file, const char *key,
const char **value_r);
const char *fs_lookup_loaded_metadata(struct fs_file *file, const char *key);
const char *fs_file_path(struct fs_file *file);
struct fs *fs_file_fs(struct fs_file *file);
struct event *fs_file_event(struct fs_file *file);
const char *fs_file_last_error(struct fs_file *file);
bool fs_prefetch(struct fs_file *file, uoff_t length);
ssize_t fs_read(struct fs_file *file, void *buf, size_t size);
struct istream *fs_read_stream(struct fs_file *file, size_t max_buffer_size);
int fs_write(struct fs_file *file, const void *data, size_t size);
struct ostream *fs_write_stream(struct fs_file *file);
int fs_write_stream_finish(struct fs_file *file, struct ostream **output);
int fs_write_stream_finish_async(struct fs_file *file);
void fs_write_stream_abort_error(struct fs_file *file, struct ostream **output, const char *error_fmt, ...) ATTR_FORMAT(3, 4);
void fs_write_set_hash(struct fs_file *file, const struct hash_method *method,
const void *digest);
void fs_file_set_async_callback(struct fs_file *file,
fs_file_async_callback_t *callback,
void *context);
#define fs_file_set_async_callback(file, callback, context) \
fs_file_set_async_callback(file, (fs_file_async_callback_t *)(callback), \
1 ? (context) : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))))
void fs_wait_async(struct fs *fs);
bool fs_switch_ioloop(struct fs *fs) ATTR_NOWARN_UNUSED_RESULT;
int fs_exists(struct fs_file *file);
int fs_delete(struct fs_file *file);
int fs_stat(struct fs_file *file, struct stat *st_r);
int fs_get_nlinks(struct fs_file *file, nlink_t *nlinks_r);
int fs_copy(struct fs_file *src, struct fs_file *dest);
int fs_copy_finish_async(struct fs_file *dest);
int fs_rename(struct fs_file *src, struct fs_file *dest);
int fs_lock(struct fs_file *file, unsigned int secs, struct fs_lock **lock_r);
void fs_unlock(struct fs_lock **lock);
struct fs_iter *
fs_iter_init(struct fs *fs, const char *path, enum fs_iter_flags flags);
struct fs_iter *
fs_iter_init_with_event(struct fs *fs, struct event *event,
const char *path, enum fs_iter_flags flags);
int fs_iter_deinit(struct fs_iter **iter, const char **error_r);
const char *fs_iter_next(struct fs_iter *iter);
void fs_iter_set_async_callback(struct fs_iter *iter,
fs_file_async_callback_t *callback,
void *context);
#define fs_iter_set_async_callback(iter, callback, context) \
fs_iter_set_async_callback(iter, (fs_file_async_callback_t *)(callback), \
1 ? (context) : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))))
bool fs_iter_have_more(struct fs_iter *iter);
const struct fs_stats *fs_get_stats(struct fs *fs);
uint64_t fs_stats_get_read_usecs(const struct fs_stats *stats);
uint64_t fs_stats_get_write_usecs(const struct fs_stats *stats);
#endif