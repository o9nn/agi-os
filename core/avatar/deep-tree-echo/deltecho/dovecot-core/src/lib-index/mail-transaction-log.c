#include "lib.h"
#include "ioloop.h"
#include "buffer.h"
#include "file-dotlock.h"
#include "nfs-workarounds.h"
#include "mmap-util.h"
#include "mail-index-private.h"
#include "mail-transaction-log-private.h"
#include <stdio.h>
#include <sys/stat.h>
static void
mail_transaction_log_set_head(struct mail_transaction_log *log,
struct mail_transaction_log_file *file)
{
i_assert(log->head != file);
file->refcount++;
log->head = file;
i_assert(log->files != NULL);
i_assert(log->files->next != NULL || log->files == file);
}
struct mail_transaction_log *
mail_transaction_log_alloc(struct mail_index *index)
{
struct mail_transaction_log *log;
log = i_new(struct mail_transaction_log, 1);
log->index = index;
return log;
}
static void mail_transaction_log_2_unlink_old(struct mail_transaction_log *log)
{
struct stat st;
uint32_t log2_rotate_time = log->index->map->hdr.log2_rotate_time;
if (MAIL_INDEX_IS_IN_MEMORY(log->index))
return;
if (log2_rotate_time == 0) {
if (nfs_safe_stat(log->filepath2, &st) == 0)
log2_rotate_time = st.st_mtime;
else if (errno == ENOENT)
log2_rotate_time = (uint32_t)-1;
else {
mail_index_set_error(log->index,
"stat(%s) failed: %m", log->filepath2);
return;
}
}
if (log2_rotate_time != (uint32_t)-1 &&
ioloop_time - (time_t)log2_rotate_time >= (time_t)log->index->optimization_set.log.log2_max_age_secs &&
!log->index->readonly) {
i_unlink_if_exists(log->filepath2);
log2_rotate_time = (uint32_t)-1;
}
if (log2_rotate_time != log->index->map->hdr.log2_rotate_time) {
log->index->hdr_log2_rotate_time_delayed_update =
log2_rotate_time;
}
}
int mail_transaction_log_open(struct mail_transaction_log *log)
{
struct mail_transaction_log_file *file;
const char *reason;
int ret;
i_free(log->filepath);
i_free(log->filepath2);
log->filepath = i_strconcat(log->index->filepath,
MAIL_TRANSACTION_LOG_SUFFIX, NULL);
log->filepath2 = i_strconcat(log->filepath, ".2", NULL);
if (log->open_file != NULL)
mail_transaction_log_file_free(&log->open_file);
if (MAIL_INDEX_IS_IN_MEMORY(log->index))
return 0;
file = mail_transaction_log_file_alloc(log, log->filepath);
if ((ret = mail_transaction_log_file_open(file, &reason)) <= 0) {
log->open_file = file;
return ret;
}
mail_transaction_log_set_head(log, file);
return 1;
}
int mail_transaction_log_create(struct mail_transaction_log *log, bool reset)
{
struct mail_transaction_log_file *file;
if (MAIL_INDEX_IS_IN_MEMORY(log->index)) {
file = mail_transaction_log_file_alloc_in_memory(log);
mail_transaction_log_set_head(log, file);
return 0;
}
file = mail_transaction_log_file_alloc(log, log->filepath);
if (log->open_file != NULL) {
file->st_ino = log->open_file->st_ino;
file->st_dev = log->open_file->st_dev;
file->last_size = log->open_file->last_size;
file->last_mtime = log->open_file->last_mtime;
mail_transaction_log_file_free(&log->open_file);
}
if (mail_transaction_log_file_create(file, reset) < 0) {
mail_transaction_log_file_free(&file);
return -1;
}
mail_transaction_log_set_head(log, file);
return 1;
}
void mail_transaction_log_close(struct mail_transaction_log *log)
{
i_assert(log->views == NULL);
if (log->open_file != NULL)
mail_transaction_log_file_free(&log->open_file);
if (log->head != NULL)
log->head->refcount--;
mail_transaction_logs_clean(log);
i_assert(log->files == NULL);
}
void mail_transaction_log_free(struct mail_transaction_log **_log)
{
struct mail_transaction_log *log = *_log;
*_log = NULL;
mail_transaction_log_close(log);
log->index->log = NULL;
i_free(log->filepath);
i_free(log->filepath2);
i_free(log);
}
int mail_transaction_log_move_to_memory(struct mail_transaction_log *log)
{
struct mail_transaction_log_file *file;
if (!log->index->initial_mapped && log->files != NULL &&
log->files->hdr.prev_file_seq != 0) {
mail_transaction_log_close(log);
}
i_free(log->filepath);
i_free(log->filepath2);
log->filepath = i_strconcat(log->index->filepath,
MAIL_TRANSACTION_LOG_SUFFIX, NULL);
log->filepath2 = i_strconcat(log->filepath, ".2", NULL);
if (log->head != NULL)
return mail_transaction_log_file_move_to_memory(log->head);
else {
file = mail_transaction_log_file_alloc_in_memory(log);
mail_transaction_log_set_head(log, file);
return 0;
}
}
void mail_transaction_log_indexid_changed(struct mail_transaction_log *log)
{
struct mail_transaction_log_file *file;
mail_transaction_logs_clean(log);
for (file = log->files; file != NULL; file = file->next) {
if (file->hdr.indexid != log->index->indexid) {
mail_transaction_log_file_set_corrupted(file,
"indexid changed: %u -> %u",
file->hdr.indexid, log->index->indexid);
}
}
if (log->head != NULL &&
log->head->hdr.indexid != log->index->indexid) {
struct mail_transaction_log_file *old_head = log->head;
(void)mail_transaction_log_create(log, FALSE);
if (--old_head->refcount == 0) {
if (old_head == log->head) {
log->head = NULL;
}
mail_transaction_log_file_free(&old_head);
}
}
}
void mail_transaction_logs_clean(struct mail_transaction_log *log)
{
struct mail_transaction_log_file *file, *next;
for (file = log->files; file != NULL; file = next) {
next = file->next;
i_assert(file->refcount >= 0);
if (file->refcount > 0)
break;
mail_transaction_log_file_free(&file);
}
for (; file != NULL; file = file->next) {
i_assert(!file->locked || file->refcount > 0);
}
i_assert(log->head == NULL || log->files != NULL);
}
bool mail_transaction_log_want_rotate(struct mail_transaction_log *log,
const char **reason_r)
{
struct mail_transaction_log_file *file = log->head;
if (file->need_rotate != NULL) {
*reason_r = t_strdup(file->need_rotate);
return TRUE;
}
if (file->hdr.major_version < MAIL_TRANSACTION_LOG_MAJOR_VERSION ||
(file->hdr.major_version == MAIL_TRANSACTION_LOG_MAJOR_VERSION &&
file->hdr.minor_version < MAIL_TRANSACTION_LOG_MINOR_VERSION)) {
*reason_r = t_strdup_printf(
".log file format version %u.%u is too old",
file->hdr.major_version, file->hdr.minor_version);
return TRUE;
}
if (file->sync_offset > log->index->optimization_set.log.max_size) {
*reason_r = t_strdup_printf(
".log file size %"PRIuUOFF_T" > max_size %"PRIuUOFF_T,
file->sync_offset, log->index->optimization_set.log.max_size);
return TRUE;
}
if (file->sync_offset < log->index->optimization_set.log.min_size) {
return FALSE;
}
if (file->hdr.create_stamp <
ioloop_time - log->index->optimization_set.log.min_age_secs) {
*reason_r = t_strdup_printf(
".log create_stamp %u is older than %u secs",
file->hdr.create_stamp,
log->index->optimization_set.log.min_age_secs);
return TRUE;
}
return FALSE;
}
int mail_transaction_log_rotate(struct mail_transaction_log *log, bool reset)
{
struct mail_transaction_log_file *file, *old_head;
const char *path = log->head->filepath;
struct stat st;
int ret;
i_assert(log->head->locked);
if (MAIL_INDEX_IS_IN_MEMORY(log->index)) {
file = mail_transaction_log_file_alloc_in_memory(log);
if (reset) {
file->hdr.prev_file_seq = 0;
file->hdr.prev_file_offset = 0;
}
} else {
if (fstat(log->head->fd, &st) < 0) {
mail_index_file_set_syscall_error(log->index,
log->head->filepath, "fstat()");
return -1;
}
file = mail_transaction_log_file_alloc(log, path);
file->st_dev = st.st_dev;
file->st_ino = st.st_ino;
file->last_mtime = st.st_mtime;
file->last_size = st.st_size;
if ((ret = mail_transaction_log_file_create(file, reset)) < 0) {
mail_transaction_log_file_free(&file);
return -1;
}
if (ret == 0) {
mail_index_set_error(log->index,
"Transaction log %s was recreated while we had it locked - "
"locking is broken (lock_method=%s)", path,
file_lock_method_to_str(log->index->set.lock_method));
mail_transaction_log_file_free(&file);
return -1;
}
i_assert(file->locked);
}
old_head = log->head;
mail_transaction_log_set_head(log, file);
e_debug(log->index->event, "Rotated transaction log %s (seq=%u, reset=%s)",
file->filepath, file->hdr.file_seq, reset ? "yes" : "no");
mail_transaction_log_file_unlock(old_head,
!log->index->log_sync_locked ? "rotating" :
"rotating while syncing");
if (--old_head->refcount == 0)
mail_transaction_logs_clean(log);
return 0;
}
static int
mail_transaction_log_refresh(struct mail_transaction_log *log, bool nfs_flush,
const char **reason_r)
{
struct mail_transaction_log_file *file;
struct stat st;
i_assert(log->head != NULL);
if (MAIL_TRANSACTION_LOG_FILE_IN_MEMORY(log->head)) {
*reason_r = "Log is in memory";
return 0;
}
if (nfs_flush &&
(log->index->flags & MAIL_INDEX_OPEN_FLAG_NFS_FLUSH) != 0)
nfs_flush_file_handle_cache(log->filepath);
if (nfs_safe_stat(log->filepath, &st) < 0) {
if (errno != ENOENT) {
mail_index_file_set_syscall_error(log->index,
log->filepath,
"stat()");
*reason_r = t_strdup_printf("stat(%s) failed: %m", log->filepath);
return -1;
}
log->index->index_deleted = TRUE;
*reason_r = "Transaction log lost while it was open";
return -1;
} else if (log->head->st_ino == st.st_ino &&
CMP_DEV_T(log->head->st_dev, st.st_dev)) {
*reason_r = "Log inode is unchanged";
return 0;
}
file = mail_transaction_log_file_alloc(log, log->filepath);
if (mail_transaction_log_file_open(file, reason_r) <= 0) {
*reason_r = t_strdup_printf(
"Failed to refresh main transaction log: %s", *reason_r);
mail_transaction_log_file_free(&file);
return -1;
}
i_assert(!file->locked);
struct mail_transaction_log_file *old_head = log->head;
mail_transaction_log_set_head(log, file);
if (--old_head->refcount == 0)
mail_transaction_logs_clean(log);
*reason_r = "Log reopened";
return 0;
}
void mail_transaction_log_get_mailbox_sync_pos(struct mail_transaction_log *log,
uint32_t *file_seq_r,
uoff_t *file_offset_r)
{
*file_seq_r = log->head->hdr.file_seq;
*file_offset_r = log->head->max_tail_offset;
}
void mail_transaction_log_set_mailbox_sync_pos(struct mail_transaction_log *log,
uint32_t file_seq,
uoff_t file_offset)
{
i_assert(file_seq == log->head->hdr.file_seq);
i_assert(file_offset >= log->head->last_read_hdr_tail_offset);
if (file_offset >= log->head->max_tail_offset)
log->head->max_tail_offset = file_offset;
}
int mail_transaction_log_find_file(struct mail_transaction_log *log,
uint32_t file_seq, bool nfs_flush,
struct mail_transaction_log_file **file_r,
const char **reason_r)
{
struct mail_transaction_log_file *file;
const char *reason;
int ret;
if (file_seq > log->head->hdr.file_seq) {
if (log->head->locked) {
*reason_r = "Log is locked - newer log can't exist";
return 0;
}
if (mail_transaction_log_refresh(log, FALSE, &reason) < 0) {
*reason_r = reason;
return -1;
}
if (file_seq > log->head->hdr.file_seq) {
if (!nfs_flush ||
(log->index->flags & MAIL_INDEX_OPEN_FLAG_NFS_FLUSH) == 0) {
*reason_r = t_strdup_printf(
"Requested newer log than exists: %s", reason);
return 0;
}
if (mail_transaction_log_refresh(log, TRUE, &reason) < 0) {
*reason_r = t_strdup_printf(
"Log refresh with NFS flush failed: %s", reason);
return -1;
}
if (file_seq > log->head->hdr.file_seq) {
*reason_r = t_strdup_printf(
"Requested newer log than exists - "
"still after NFS flush: %s", reason);
return 0;
}
}
}
for (file = log->files; file != NULL; file = file->next) {
if (file->hdr.file_seq == file_seq) {
*file_r = file;
return 1;
}
if (file->hdr.file_seq > file_seq &&
file->hdr.prev_file_seq == 0) {
*reason_r = "Log was reset after requested file_seq";
return 0;
}
}
if (MAIL_INDEX_IS_IN_MEMORY(log->index)) {
*reason_r = "Logs are only in memory";
return 0;
}
file = mail_transaction_log_file_alloc(log, log->filepath2);
if ((ret = mail_transaction_log_file_open(file, reason_r)) <= 0) {
*reason_r = t_strdup_printf(
"Not found from .log.2: %s", *reason_r);
mail_transaction_log_file_free(&file);
return ret;
}
if (file->hdr.file_seq != file_seq) {
*reason_r = t_strdup_printf(".log.2 contains file_seq=%u",
file->hdr.file_seq);
return 0;
}
*file_r = file;
return 1;
}
int mail_transaction_log_lock_head(struct mail_transaction_log *log,
const char *lock_reason)
{
struct mail_transaction_log_file *file;
time_t lock_wait_started, lock_secs = 0;
const char *reason;
int ret = 0;
lock_wait_started = time(NULL);
for (;;) {
file = log->head;
if (mail_transaction_log_file_lock(file) < 0)
return -1;
file->refcount++;
ret = mail_transaction_log_refresh(log, TRUE, &reason);
if (--file->refcount == 0) {
mail_transaction_log_file_unlock(file, t_strdup_printf(
"trying to lock head for %s", lock_reason));
mail_transaction_logs_clean(log);
file = NULL;
}
if (ret == 0 && log->head == file) {
i_assert(file != NULL);
lock_secs = file->lock_create_time - lock_wait_started;
break;
}
if (file != NULL) {
mail_transaction_log_file_unlock(file, t_strdup_printf(
"trying to lock head for %s", lock_reason));
}
if (ret < 0)
break;
}
if (lock_secs > MAIL_TRANSACTION_LOG_LOCK_WARN_SECS) {
e_warning(log->index->event,
"Locking transaction log file %s took %ld seconds (%s)",
log->head->filepath, (long)lock_secs, lock_reason);
}
i_assert(ret < 0 || log->head != NULL);
return ret;
}
int mail_transaction_log_sync_lock(struct mail_transaction_log *log,
const char *lock_reason,
uint32_t *file_seq_r, uoff_t *file_offset_r)
{
const char *reason;
i_assert(!log->index->log_sync_locked);
if (!log->log_2_unlink_checked) {
log->log_2_unlink_checked = TRUE;
mail_transaction_log_2_unlink_old(log);
}
if (mail_transaction_log_lock_head(log, lock_reason) < 0)
return -1;
if (mail_transaction_log_file_map(log->head, log->head->sync_offset,
UOFF_T_MAX, &reason) <= 0) {
mail_index_set_error(log->index,
"Failed to map transaction log %s at "
"sync_offset=%"PRIuUOFF_T" after locking: %s",
log->head->filepath, log->head->sync_offset, reason);
mail_transaction_log_file_unlock(log->head, t_strdup_printf(
"%s - map failed", lock_reason));
return -1;
}
log->index->log_sync_locked = TRUE;
*file_seq_r = log->head->hdr.file_seq;
*file_offset_r = log->head->sync_offset;
return 0;
}
void mail_transaction_log_sync_unlock(struct mail_transaction_log *log,
const char *lock_reason)
{
i_assert(log->index->log_sync_locked);
log->index->log_sync_locked = FALSE;
mail_transaction_log_file_unlock(log->head, lock_reason);
}
void mail_transaction_log_get_head(struct mail_transaction_log *log,
uint32_t *file_seq_r, uoff_t *file_offset_r)
{
*file_seq_r = log->head->hdr.file_seq;
*file_offset_r = log->head->sync_offset;
}
void mail_transaction_log_get_tail(struct mail_transaction_log *log,
uint32_t *file_seq_r)
{
struct mail_transaction_log_file *tail, *file = log->files;
for (tail = file; file->next != NULL; file = file->next) {
if (file->hdr.file_seq + 1 != file->next->hdr.file_seq)
tail = file->next;
}
*file_seq_r = tail->hdr.file_seq;
}
bool mail_transaction_log_is_head_prev(struct mail_transaction_log *log,
uint32_t file_seq, uoff_t file_offset)
{
return log->head->hdr.prev_file_seq == file_seq &&
log->head->hdr.prev_file_offset == file_offset;
}
int mail_transaction_log_unlink(struct mail_transaction_log *log)
{
if (unlink(log->filepath) < 0 &&
errno != ENOENT && errno != ESTALE) {
mail_index_file_set_syscall_error(log->index, log->filepath,
"unlink()");
return -1;
}
return 0;
}
void mail_transaction_log_get_dotlock_set(struct mail_transaction_log *log,
struct dotlock_settings *set_r)
{
struct mail_index *index = log->index;
i_zero(set_r);
set_r->timeout = I_MIN(MAIL_TRANSACTION_LOG_LOCK_TIMEOUT,
index->set.max_lock_timeout_secs);
set_r->stale_timeout = MAIL_TRANSACTION_LOG_DOTLOCK_CHANGE_TIMEOUT;
set_r->nfs_flush = (index->flags & MAIL_INDEX_OPEN_FLAG_NFS_FLUSH) != 0;
set_r->use_excl_lock =
(index->flags & MAIL_INDEX_OPEN_FLAG_DOTLOCK_USE_EXCL) != 0;
}