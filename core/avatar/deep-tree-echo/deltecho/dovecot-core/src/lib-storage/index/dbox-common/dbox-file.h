#ifndef DBOX_FILE_H
#define DBOX_FILE_H
#define DBOX_VERSION 2
#define DBOX_MAGIC_PRE "\001\002"
#define DBOX_MAGIC_POST "\n\001\003\n"
#ifdef HAVE_FLOCK
# define DBOX_FILE_LOCK_METHOD_FLOCK
#endif
struct dbox_file;
struct stat;
enum dbox_header_key {
DBOX_HEADER_MSG_HEADER_SIZE = 'M',
DBOX_HEADER_CREATE_STAMP = 'C',
DBOX_HEADER_OLDV1_APPEND_OFFSET = 'A'
};
enum dbox_metadata_key {
DBOX_METADATA_GUID = 'G',
DBOX_METADATA_POP3_UIDL = 'P',
DBOX_METADATA_POP3_ORDER = 'O',
DBOX_METADATA_RECEIVED_TIME = 'R',
DBOX_METADATA_PHYSICAL_SIZE = 'Z',
DBOX_METADATA_VIRTUAL_SIZE = 'V',
DBOX_METADATA_EXT_REF = 'X',
DBOX_METADATA_ORIG_MAILBOX = 'B',
DBOX_METADATA_OLDV1_EXPUNGED = 'E',
DBOX_METADATA_OLDV1_FLAGS = 'F',
DBOX_METADATA_OLDV1_KEYWORDS = 'K',
DBOX_METADATA_OLDV1_SAVE_TIME = 'S',
DBOX_METADATA_OLDV1_SPACE = ' '
};
enum dbox_message_type {
DBOX_MESSAGE_TYPE_NORMAL = 'N'
};
struct dbox_message_header {
unsigned char magic_pre[2];
unsigned char type;
unsigned char space1;
unsigned char oldv1_uid_hex[8];
unsigned char space2;
unsigned char message_size_hex[16];
unsigned char save_lf;
};
struct dbox_metadata_header {
unsigned char magic_post[sizeof(DBOX_MAGIC_POST)-1];
};
struct dbox_file {
struct dbox_storage *storage;
int refcount;
time_t create_time;
unsigned int file_version;
unsigned int file_header_size;
unsigned int msg_header_size;
const char *cur_path;
char *primary_path, *alt_path;
int fd;
struct istream *input;
#ifdef DBOX_FILE_LOCK_METHOD_FLOCK
struct file_lock *lock;
#else
struct dotlock *lock;
#endif
uoff_t cur_offset;
uoff_t cur_physical_size;
pool_t metadata_pool;
ARRAY(const char *) metadata;
uoff_t metadata_read_offset;
bool appending:1;
bool corrupted:1;
bool fixing:1;
};
struct dbox_file_append_context {
struct dbox_file *file;
uoff_t first_append_offset, last_checkpoint_offset, last_flush_offset;
struct ostream *output;
};
#define dbox_file_is_open(file) ((file)->fd != -1)
#define dbox_file_is_in_alt(file) ((file)->cur_path == (file)->alt_path)
void dbox_file_init(struct dbox_file *file);
void dbox_file_unref(struct dbox_file **file);
int dbox_file_open(struct dbox_file *file, bool *deleted_r);
int dbox_file_open_primary(struct dbox_file *file, bool *notfound_r);
void dbox_file_close(struct dbox_file *file);
int dbox_file_stat(struct dbox_file *file, struct event *event, struct stat *st_r);
int dbox_file_try_lock(struct dbox_file *file);
void dbox_file_unlock(struct dbox_file *file);
int dbox_file_seek(struct dbox_file *file, uoff_t offset);
void dbox_file_seek_rewind(struct dbox_file *file);
int dbox_file_seek_next(struct dbox_file *file, uoff_t *offset_r, bool *last_r);
struct dbox_file_append_context *dbox_file_append_init(struct dbox_file *file);
int dbox_file_append_commit(struct dbox_file_append_context **ctx);
void dbox_file_append_rollback(struct dbox_file_append_context **ctx);
int dbox_file_get_append_stream(struct dbox_file_append_context *ctx,
struct ostream **output_r);
void dbox_file_append_checkpoint(struct dbox_file_append_context *ctx);
int dbox_file_append_flush(struct dbox_file_append_context *ctx);
int dbox_file_metadata_read(struct dbox_file *file);
const char *dbox_file_metadata_get(struct dbox_file *file,
enum dbox_metadata_key key);
uoff_t dbox_file_get_plaintext_size(struct dbox_file *file);
int dbox_file_fix(struct dbox_file *file, uoff_t start_offset);
int dbox_file_unlink(struct dbox_file *file);
void dbox_msg_header_fill(struct dbox_message_header *dbox_msg_hdr,
uoff_t message_size);
void dbox_file_set_syscall_error(struct dbox_file *file, const char *function);
void dbox_file_set_corrupted(struct dbox_file *file, const char *reason, ...)
ATTR_FORMAT(2, 3);
const char *dbox_generate_tmp_filename(void);
void dbox_file_free(struct dbox_file *file);
int dbox_file_header_write(struct dbox_file *file, struct ostream *output);
int dbox_file_read_mail_header(struct dbox_file *file, uoff_t *physical_size_r);
int dbox_file_metadata_skip_header(struct dbox_file *file);
#endif