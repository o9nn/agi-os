#include "lib.h"
#include "ioloop.h"
#include "array.h"
#include "buffer.h"
#include "hash.h"
#include "str.h"
#include "eacces-error.h"
#include "nfs-workarounds.h"
#include "maildir-storage.h"
#include "maildir-uidlist.h"
#include "maildir-filename.h"
#include "maildir-sync.h"
#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#define MAILDIR_RENAME_RESCAN_COUNT 5
#define MAILDIR_SCAN_DIR_MAX_COUNT 5
#define DUPE_LINKS_DELETE_SECS 30
enum maildir_scan_why {
WHY_FORCED	= 0x01,
WHY_FIRSTSYNC	= 0x02,
WHY_NEWCHANGED	= 0x04,
WHY_CURCHANGED	= 0x08,
WHY_DROPRECENT	= 0x10,
WHY_FINDRECENT	= 0x20,
WHY_DELAYEDNEW	= 0x40,
WHY_DELAYEDCUR	= 0x80
};
struct maildir_sync_context {
struct maildir_mailbox *mbox;
const char *new_dir, *cur_dir;
enum mailbox_sync_flags flags;
time_t last_touch, last_notify;
struct maildir_uidlist_sync_ctx *uidlist_sync_ctx;
struct maildir_index_sync_context *index_sync_ctx;
bool partial:1;
bool locked:1;
bool racing:1;
};
void maildir_sync_set_racing(struct maildir_sync_context *ctx)
{
ctx->racing = TRUE;
}
void maildir_sync_notify(struct maildir_sync_context *ctx)
{
time_t now;
if (ctx == NULL) {
return;
}
now = time(NULL);
if (now - ctx->last_touch > MAILDIR_LOCK_TOUCH_SECS && ctx->locked) {
(void)maildir_uidlist_lock_touch(ctx->mbox->uidlist);
ctx->last_touch = now;
}
if (now - ctx->last_notify > MAIL_STORAGE_NOTIFY_INTERVAL_SECS) {
struct mailbox *box = &ctx->mbox->box;
if (box->storage->callbacks.notify_progress != NULL) T_BEGIN {
struct mail_storage_progress_details dtl = {};
box->storage->callbacks.notify_progress(
box, &dtl, box->storage->callback_context);
} T_END;
ctx->last_notify = now;
}
}
static struct maildir_sync_context *
maildir_sync_context_new(struct maildir_mailbox *mbox,
enum mailbox_sync_flags flags)
{
struct maildir_sync_context *ctx;
ctx = t_new(struct maildir_sync_context, 1);
ctx->mbox = mbox;
ctx->new_dir = t_strconcat(mailbox_get_path(&mbox->box), "/new", NULL);
ctx->cur_dir = t_strconcat(mailbox_get_path(&mbox->box), "/cur", NULL);
ctx->last_touch = ioloop_time;
ctx->last_notify = ioloop_time;
ctx->flags = flags;
return ctx;
}
static void maildir_sync_deinit(struct maildir_sync_context *ctx)
{
if (ctx->uidlist_sync_ctx != NULL)
(void)maildir_uidlist_sync_deinit(&ctx->uidlist_sync_ctx, FALSE);
if (ctx->index_sync_ctx != NULL)
maildir_sync_index_rollback(&ctx->index_sync_ctx);
if (ctx->mbox->storage->storage.rebuild_list_index)
(void)mail_storage_list_index_rebuild_and_set_uncorrupted(&ctx->mbox->storage->storage);
}
static int maildir_fix_duplicate(struct maildir_sync_context *ctx,
const char *dir, const char *fname2)
{
struct event *event = ctx->mbox->box.event;
const char *fname1, *path1, *path2;
const char *new_fname, *new_path;
struct stat st1, st2;
uoff_t size;
fname1 = maildir_uidlist_sync_get_full_filename(ctx->uidlist_sync_ctx,
fname2);
i_assert(fname1 != NULL);
path1 = t_strconcat(dir, "/", fname1, NULL);
path2 = t_strconcat(dir, "/", fname2, NULL);
if (stat(path1, &st1) < 0 || stat(path2, &st2) < 0) {
return 0;
}
if (st1.st_ino == st2.st_ino &&
CMP_DEV_T(st1.st_dev, st2.st_dev)) {
if (st1.st_nlink > 1 && st2.st_nlink == st1.st_nlink &&
st1.st_ctime == st2.st_ctime &&
st1.st_ctime < ioloop_time - DUPE_LINKS_DELETE_SECS) {
if (i_unlink(path2) == 0)
e_warning(event, "Unlinked a duplicate: %s", path2);
}
return 0;
}
new_fname = maildir_filename_generate();
if (maildir_filename_get_size(fname2, MAILDIR_EXTRA_FILE_SIZE, &size)) {
new_fname = t_strdup_printf("%s,%c=%"PRIuUOFF_T,
new_fname, MAILDIR_EXTRA_FILE_SIZE, size);
}
if (maildir_filename_get_size(fname2, MAILDIR_EXTRA_VIRTUAL_SIZE, &size)) {
new_fname = t_strdup_printf("%s,%c=%"PRIuUOFF_T,
new_fname, MAILDIR_EXTRA_VIRTUAL_SIZE, size);
}
new_path = t_strconcat(mailbox_get_path(&ctx->mbox->box),
"/new/", new_fname, NULL);
if (rename(path2, new_path) == 0)
e_warning(event, "Fixed a duplicate: %s -> %s", path2, new_fname);
else if (errno != ENOENT) {
mailbox_set_critical(&ctx->mbox->box,
"Couldn't fix a duplicate: rename(%s, %s) failed: %m",
path2, new_path);
return -1;
}
return 0;
}
static int
maildir_rename_empty_basename(struct maildir_sync_context *ctx,
const char *dir, const char *fname)
{
struct event *event = ctx->mbox->box.event;
const char *old_path, *new_fname, *new_path;
old_path = t_strconcat(dir, "/", fname, NULL);
new_fname = maildir_filename_generate();
new_path = t_strconcat(mailbox_get_path(&ctx->mbox->box),
"/new/", new_fname, NULL);
if (rename(old_path, new_path) == 0)
e_warning(event, "Fixed broken filename: %s -> %s",
old_path, new_fname);
else if (errno != ENOENT) {
mailbox_set_critical(&ctx->mbox->box,
"Couldn't fix a broken filename: rename(%s, %s) failed: %m",
old_path, new_path);
return -1;
}
return 0;
}
static int
maildir_stat(struct maildir_mailbox *mbox, const char *path, struct stat *st_r)
{
struct mailbox *box = &mbox->box;
int i;
for (i = 0;; i++) {
if (nfs_safe_stat(path, st_r) == 0)
return 0;
if (errno != ENOENT || i == MAILDIR_DELETE_RETRY_COUNT)
break;
if (!maildir_set_deleted(box))
return -1;
}
mailbox_set_critical(box, "stat(%s) failed: %m", path);
return -1;
}
static int
maildir_scan_dir(struct maildir_sync_context *ctx, bool new_dir, bool final,
enum maildir_scan_why why)
{
struct event *event = ctx->mbox->box.event;
const char *path;
DIR *dirp;
string_t *src, *dest;
struct dirent *dp;
struct stat st;
enum maildir_uidlist_rec_flag flags;
unsigned int time_diff, i, readdir_count = 0, move_count = 0;
time_t start_time;
int ret = 1;
bool move_new, dir_changed = FALSE;
path = new_dir ? ctx->new_dir : ctx->cur_dir;
for (i = 0;; i++) {
dirp = opendir(path);
if (dirp != NULL)
break;
if (errno != ENOENT || i == MAILDIR_DELETE_RETRY_COUNT) {
if (ENOACCESS(errno)) {
mailbox_set_critical(&ctx->mbox->box, "%s",
eacces_error_get("opendir", path));
} else {
mailbox_set_critical(&ctx->mbox->box,
"opendir(%s) failed: %m", path);
}
return -1;
}
if (!maildir_set_deleted(&ctx->mbox->box))
return -1;
}
#ifdef HAVE_DIRFD
if (fstat(dirfd(dirp), &st) < 0) {
mailbox_set_critical(&ctx->mbox->box,
"fstat(%s) failed: %m", path);
(void)closedir(dirp);
return -1;
}
#else
if (maildir_stat(ctx->mbox, path, &st) < 0) {
(void)closedir(dirp);
return -1;
}
#endif
start_time = time(NULL);
if (new_dir) {
ctx->mbox->maildir_hdr.new_check_time = time_to_uint32(start_time);
ctx->mbox->maildir_hdr.new_mtime = st.st_mtime;
ctx->mbox->maildir_hdr.new_mtime_nsecs = ST_MTIME_NSEC(st);
} else {
ctx->mbox->maildir_hdr.cur_check_time = time_to_uint32(start_time);
ctx->mbox->maildir_hdr.cur_mtime = st.st_mtime;
ctx->mbox->maildir_hdr.cur_mtime_nsecs = ST_MTIME_NSEC(st);
}
src = t_str_new(1024);
dest = t_str_new(1024);
move_new = new_dir && ctx->locked &&
((ctx->mbox->box.flags & MAILBOX_FLAG_DROP_RECENT) != 0 ||
ctx->mbox->storage->set->maildir_empty_new);
errno = 0;
for (; (dp = readdir(dirp)) != NULL; errno = 0) {
if (dp->d_name[0] == '.')
continue;
if (dp->d_name[0] == MAILDIR_INFO_SEP) {
if (maildir_rename_empty_basename(ctx, path,
dp->d_name) < 0)
break;
continue;
}
flags = 0;
if (move_new) {
i_assert(dp->d_name[0] != '\0');
str_truncate(src, 0);
str_truncate(dest, 0);
str_printfa(src, "%s/%s", ctx->new_dir, dp->d_name);
str_printfa(dest, "%s/%s", ctx->cur_dir, dp->d_name);
if (strchr(dp->d_name, MAILDIR_INFO_SEP) == NULL) {
str_append(dest, MAILDIR_FLAGS_FULL_SEP);
}
if (rename(str_c(src), str_c(dest)) == 0) {
dir_changed = TRUE;
move_count++;
flags |= MAILDIR_UIDLIST_REC_FLAG_MOVED |
MAILDIR_UIDLIST_REC_FLAG_RECENT;
} else if (ENOTFOUND(errno)) {
dir_changed = TRUE;
move_count++;
flags |= MAILDIR_UIDLIST_REC_FLAG_MOVED |
MAILDIR_UIDLIST_REC_FLAG_RECENT;
} else if (ENOSPACE(errno) || ENOACCESS(errno)) {
flags |= MAILDIR_UIDLIST_REC_FLAG_NEW_DIR;
move_new = FALSE;
} else {
flags |= MAILDIR_UIDLIST_REC_FLAG_NEW_DIR;
mailbox_set_critical(&ctx->mbox->box,
"rename(%s, %s) failed: %m",
str_c(src), str_c(dest));
}
if ((move_count % MAILDIR_SLOW_MOVE_COUNT) == 0)
maildir_sync_notify(ctx);
} else if (new_dir) {
flags |= MAILDIR_UIDLIST_REC_FLAG_NEW_DIR |
MAILDIR_UIDLIST_REC_FLAG_RECENT;
}
readdir_count++;
if ((readdir_count % MAILDIR_SLOW_CHECK_COUNT) == 0)
maildir_sync_notify(ctx);
ret = maildir_uidlist_sync_next(ctx->uidlist_sync_ctx,
dp->d_name, flags);
if (ret <= 0) {
if (ret < 0)
break;
T_BEGIN {
ret = maildir_fix_duplicate(ctx, path,
dp->d_name);
} T_END;
if (ret < 0)
break;
}
}
#ifdef __APPLE__
if (errno == EINVAL && move_count > 0 && !final) {
move_count = MAILDIR_RENAME_RESCAN_COUNT + 1;
} else
#endif
if (errno != 0) {
mailbox_set_critical(&ctx->mbox->box,
"readdir(%s) failed: %m", path);
ret = -1;
}
if (closedir(dirp) < 0) {
mailbox_set_critical(&ctx->mbox->box,
"closedir(%s) failed: %m", path);
ret = -1;
}
if (dir_changed) {
if (stat(ctx->new_dir, &st) == 0) {
ctx->mbox->maildir_hdr.new_check_time =
I_MAX(st.st_mtime, start_time);
ctx->mbox->maildir_hdr.new_mtime = st.st_mtime;
ctx->mbox->maildir_hdr.new_mtime_nsecs =
ST_MTIME_NSEC(st);
}
if (stat(ctx->cur_dir, &st) == 0) {
ctx->mbox->maildir_hdr.new_check_time =
I_MAX(st.st_mtime, start_time);
ctx->mbox->maildir_hdr.cur_mtime = st.st_mtime;
ctx->mbox->maildir_hdr.cur_mtime_nsecs =
ST_MTIME_NSEC(st);
}
}
time_diff = time(NULL) - start_time;
if (time_diff >= MAILDIR_SYNC_TIME_WARN_SECS) {
e_warning(event,
"Scanning %s took %u seconds "
"(%u readdir()s, %u rename()s to cur/, why=0x%x)",
path, time_diff, readdir_count, move_count, why);
}
return ret < 0 ? -1 :
(move_count <= MAILDIR_RENAME_RESCAN_COUNT || final ? 0 : 1);
}
static void maildir_sync_get_header(struct maildir_mailbox *mbox)
{
const void *data;
size_t data_size;
mail_index_get_header_ext(mbox->box.view, mbox->maildir_ext_id,
&data, &data_size);
if (data_size == 0) {
} else {
memcpy(&mbox->maildir_hdr, data,
I_MIN(sizeof(mbox->maildir_hdr), data_size));
}
}
int maildir_sync_header_refresh(struct maildir_mailbox *mbox)
{
if (mail_index_refresh(mbox->box.index) < 0) {
mailbox_set_index_error(&mbox->box);
return -1;
}
maildir_sync_get_header(mbox);
return 0;
}
static int maildir_sync_quick_check(struct maildir_mailbox *mbox, bool undirty,
const char *new_dir, const char *cur_dir,
bool *new_changed_r, bool *cur_changed_r,
enum maildir_scan_why *why_r)
{
#define DIR_DELAYED_REFRESH(hdr, name) \
((hdr)->name ## _check_time <= \
(hdr)->name ## _mtime + MAILDIR_SYNC_SECS && \
(undirty || \
(time_t)(hdr)->name ## _check_time < ioloop_time - MAILDIR_SYNC_SECS))
#define DIR_MTIME_CHANGED(st, hdr, name) \
((st).st_mtime != (time_t)(hdr)->name ## _mtime || \
!ST_NTIMES_EQUAL(ST_MTIME_NSEC(st), (hdr)->name ## _mtime_nsecs))
struct maildir_index_header *hdr = &mbox->maildir_hdr;
struct stat new_st, cur_st;
bool refreshed = FALSE, check_new = FALSE, check_cur = FALSE;
*why_r = 0;
if (mbox->maildir_hdr.new_mtime == 0) {
maildir_sync_get_header(mbox);
if (mbox->maildir_hdr.new_mtime == 0) {
*why_r |= WHY_FIRSTSYNC;
*new_changed_r = *cur_changed_r = TRUE;
return 0;
}
}
*new_changed_r = *cur_changed_r = FALSE;
if (DIR_DELAYED_REFRESH(hdr, new) ||
(DIR_DELAYED_REFRESH(hdr, cur) &&
!mbox->storage->set->maildir_very_dirty_syncs)) {
if (maildir_sync_header_refresh(mbox) < 0)
return -1;
refreshed = TRUE;
if (DIR_DELAYED_REFRESH(hdr, new)) {
*why_r |= WHY_DELAYEDNEW;
*new_changed_r = TRUE;
}
if (DIR_DELAYED_REFRESH(hdr, cur) &&
!mbox->storage->set->maildir_very_dirty_syncs) {
*why_r |= WHY_DELAYEDCUR;
*cur_changed_r = TRUE;
}
if (*new_changed_r && *cur_changed_r)
return 0;
}
if (!*new_changed_r) {
if (maildir_stat(mbox, new_dir, &new_st) < 0)
return -1;
check_new = TRUE;
}
if (!*cur_changed_r) {
if (maildir_stat(mbox, cur_dir, &cur_st) < 0)
return -1;
check_cur = TRUE;
}
for (;;) {
if (check_new) {
*new_changed_r = DIR_MTIME_CHANGED(new_st, hdr, new);
if (*new_changed_r)
*why_r |= WHY_NEWCHANGED;
}
if (check_cur) {
*cur_changed_r = DIR_MTIME_CHANGED(cur_st, hdr, cur);
if (*cur_changed_r)
*why_r |= WHY_CURCHANGED;
}
if ((!*new_changed_r && !*cur_changed_r) || refreshed)
break;
if (maildir_sync_header_refresh(mbox) < 0)
return -1;
refreshed = TRUE;
}
return 0;
}
static void maildir_sync_update_next_uid(struct maildir_mailbox *mbox)
{
const struct mail_index_header *hdr;
uint32_t uid_validity;
hdr = mail_index_get_header(mbox->box.view);
if (hdr->uid_validity == 0)
return;
uid_validity = maildir_uidlist_get_uid_validity(mbox->uidlist);
if (uid_validity == hdr->uid_validity || uid_validity == 0) {
maildir_uidlist_set_uid_validity(mbox->uidlist,
hdr->uid_validity);
maildir_uidlist_set_next_uid(mbox->uidlist,
hdr->next_uid, FALSE);
}
}
static bool
have_recent_messages(struct maildir_sync_context *ctx, bool seen_changes)
{
const struct mail_index_header *hdr;
uint32_t next_uid;
hdr = mail_index_get_header(ctx->mbox->box.view);
if (!seen_changes) {
next_uid = hdr->next_uid;
} else {
(void)maildir_uidlist_refresh(ctx->mbox->uidlist);
next_uid = maildir_uidlist_get_next_uid(ctx->mbox->uidlist);
}
return hdr->first_recent_uid < next_uid;
}
static int maildir_sync_get_changes(struct maildir_sync_context *ctx,
bool *new_changed_r, bool *cur_changed_r,
enum maildir_scan_why *why_r)
{
struct maildir_mailbox *mbox = ctx->mbox;
enum mail_index_sync_flags flags = 0;
bool undirty = (ctx->flags & MAILBOX_SYNC_FLAG_FULL_READ) != 0;
*why_r = 0;
if (maildir_sync_quick_check(mbox, undirty, ctx->new_dir, ctx->cur_dir,
new_changed_r, cur_changed_r, why_r) < 0)
return -1;
if ((mbox->box.flags & MAILBOX_FLAG_DROP_RECENT) != 0) {
if (!*new_changed_r && have_recent_messages(ctx, FALSE)) {
*new_changed_r = TRUE;
*why_r |= WHY_DROPRECENT;
}
} else if (*new_changed_r) {
if (!*cur_changed_r && have_recent_messages(ctx, TRUE)) {
*cur_changed_r = TRUE;
*why_r |= WHY_FINDRECENT;
}
}
if (*new_changed_r || *cur_changed_r)
return 1;
if ((mbox->box.flags & MAILBOX_FLAG_DROP_RECENT) != 0)
flags |= MAIL_INDEX_SYNC_FLAG_DROP_RECENT;
if (mbox->synced) {
mail_index_refresh(mbox->box.index);
}
return mail_index_sync_have_any(mbox->box.index, flags) ? 1 : 0;
}
static int ATTR_NULL(3)
maildir_sync_context(struct maildir_sync_context *ctx, bool forced,
uint32_t *find_uid, bool *lost_files_r)
{
enum maildir_uidlist_sync_flags sync_flags;
enum maildir_uidlist_rec_flag flags;
bool new_changed, cur_changed, lock_failure;
const char *fname;
enum maildir_scan_why why;
int ret;
*lost_files_r = FALSE;
if (forced) {
new_changed = cur_changed = TRUE;
why = WHY_FORCED;
} else {
ret = maildir_sync_get_changes(ctx, &new_changed, &cur_changed,
&why);
if (ret <= 0)
return ret;
}
if (!cur_changed) {
ctx->partial = TRUE;
sync_flags = MAILDIR_UIDLIST_SYNC_PARTIAL;
} else {
ctx->partial = FALSE;
sync_flags = 0;
if (forced)
sync_flags |= MAILDIR_UIDLIST_SYNC_FORCE;
if ((ctx->flags & MAILBOX_SYNC_FLAG_FAST) != 0)
sync_flags |= MAILDIR_UIDLIST_SYNC_TRYLOCK;
}
ret = maildir_uidlist_sync_init(ctx->mbox->uidlist, sync_flags,
&ctx->uidlist_sync_ctx);
lock_failure = ret <= 0;
if (ret <= 0) {
struct mail_storage *storage = ctx->mbox->box.storage;
if (ret == 0) {
return 0;
}
if (forced) {
return -1;
}
ret = maildir_uidlist_sync_init(ctx->mbox->uidlist, sync_flags |
MAILDIR_UIDLIST_SYNC_NOLOCK,
&ctx->uidlist_sync_ctx);
if (ret <= 0) {
i_assert(ret != 0);
return -1;
}
if (storage->callbacks.notify_no != NULL) {
storage->callbacks.notify_no(&ctx->mbox->box,
"Internal mailbox synchronization failure, "
"showing only old mails.",
storage->callback_context);
}
}
ctx->locked = maildir_uidlist_is_locked(ctx->mbox->uidlist);
if (!ctx->locked)
ctx->partial = TRUE;
if (!ctx->mbox->syncing_commit && (ctx->locked || lock_failure)) {
if (maildir_sync_index_begin(ctx->mbox, ctx,
&ctx->index_sync_ctx) < 0)
return -1;
}
if (new_changed || cur_changed) {
unsigned int count = 0;
bool final = FALSE;
while ((ret = maildir_scan_dir(ctx, TRUE, final, why)) > 0) {
if (++count >= MAILDIR_SCAN_DIR_MAX_COUNT)
final = TRUE;
}
if (ret < 0)
return -1;
if (cur_changed) {
if (maildir_scan_dir(ctx, FALSE, TRUE, why) < 0)
return -1;
}
maildir_sync_update_next_uid(ctx->mbox);
maildir_uidlist_sync_finish(ctx->uidlist_sync_ctx);
}
if (!ctx->locked) {
ctx->mbox->maildir_hdr.new_mtime = 0;
ctx->mbox->maildir_hdr.cur_mtime = 0;
}
if (ctx->index_sync_ctx != NULL) {
ret = maildir_sync_index(ctx->index_sync_ctx, ctx->partial);
if (ret < 0)
maildir_sync_index_rollback(&ctx->index_sync_ctx);
else if (maildir_sync_index_commit(&ctx->index_sync_ctx) < 0)
return -1;
if (ret < 0)
return -1;
if (ret == 0)
*lost_files_r = TRUE;
i_assert(maildir_uidlist_is_locked(ctx->mbox->uidlist) ||
lock_failure);
}
if (find_uid != NULL && *find_uid != 0) {
ret = maildir_uidlist_lookup(ctx->mbox->uidlist,
*find_uid, &flags, &fname);
if (ret < 0)
return -1;
if (ret == 0) {
*find_uid = 0;
} else if ((flags & MAILDIR_UIDLIST_REC_FLAG_NONSYNCED) == 0) {
*find_uid = 0;
} else {
}
}
return maildir_uidlist_sync_deinit(&ctx->uidlist_sync_ctx, TRUE);
}
int maildir_sync_lookup(struct maildir_mailbox *mbox, uint32_t uid,
enum maildir_uidlist_rec_flag *flags_r,
const char **fname_r)
{
int ret;
ret = maildir_uidlist_lookup(mbox->uidlist, uid, flags_r, fname_r);
if (ret != 0)
return ret;
if (maildir_uidlist_is_open(mbox->uidlist)) {
if (mbox->sync_uidlist_refreshed) {
return ret;
}
mbox->sync_uidlist_refreshed = TRUE;
if (maildir_uidlist_refresh(mbox->uidlist) < 0)
return -1;
} else {
if (maildir_storage_sync_force(mbox, uid) < 0)
return -1;
}
return maildir_uidlist_lookup(mbox->uidlist, uid, flags_r, fname_r);
}
static int maildir_sync_run(struct maildir_mailbox *mbox,
enum mailbox_sync_flags flags, bool force_resync,
uint32_t *uid, bool *lost_files_r)
{
struct maildir_sync_context *ctx;
bool retry, lost_files;
int ret;
T_BEGIN {
ctx = maildir_sync_context_new(mbox, flags);
ret = maildir_sync_context(ctx, force_resync, uid, lost_files_r);
retry = ctx->racing;
maildir_sync_deinit(ctx);
} T_END;
if (retry) T_BEGIN {
ctx = maildir_sync_context_new(mbox, 0);
ret = maildir_sync_context(ctx, TRUE, NULL, &lost_files);
maildir_sync_deinit(ctx);
} T_END;
return ret;
}
int maildir_storage_sync_force(struct maildir_mailbox *mbox, uint32_t uid)
{
bool lost_files;
int ret;
ret = maildir_sync_run(mbox, MAILBOX_SYNC_FLAG_FAST,
TRUE, &uid, &lost_files);
if (uid != 0) {
ret = maildir_sync_run(mbox, 0, TRUE, NULL, &lost_files);
}
return ret;
}
int maildir_sync_refresh_flags_view(struct maildir_mailbox *mbox)
{
struct mail_index_view_sync_ctx *sync_ctx;
bool delayed_expunges;
mail_index_refresh(mbox->box.index);
if (mbox->flags_view == NULL)
mbox->flags_view = mail_index_view_open(mbox->box.index);
sync_ctx = mail_index_view_sync_begin(mbox->flags_view,
MAIL_INDEX_VIEW_SYNC_FLAG_FIX_INCONSISTENT);
if (mail_index_view_sync_commit(&sync_ctx, &delayed_expunges) < 0) {
mailbox_set_index_error(&mbox->box);
return -1;
}
if (mbox->flags_view->map->refcount > 1) {
struct mail_index_map *map;
map = mail_index_map_clone(mbox->flags_view->map);
mail_index_unmap(&mbox->flags_view->map);
mbox->flags_view->map = map;
}
mail_index_record_map_move_to_private(mbox->flags_view->map);
mail_index_map_move_to_memory(mbox->flags_view->map);
return 0;
}
struct mailbox_sync_context *
maildir_storage_sync_init(struct mailbox *box, enum mailbox_sync_flags flags)
{
struct maildir_mailbox *mbox = MAILDIR_MAILBOX(box);
bool lost_files, force_resync;
int ret = 0;
force_resync = (flags & MAILBOX_SYNC_FLAG_FORCE_RESYNC) != 0;
if (index_mailbox_want_full_sync(&mbox->box, flags)) {
ret = maildir_sync_run(mbox, flags, force_resync,
NULL, &lost_files);
i_assert(!maildir_uidlist_is_locked(mbox->uidlist) ||
(box->flags & MAILBOX_FLAG_KEEP_LOCKED) != 0);
if (lost_files) {
ret = maildir_storage_sync_force(mbox, 0);
}
}
if (mbox->storage->set->maildir_very_dirty_syncs) {
if (maildir_sync_refresh_flags_view(mbox) < 0)
ret = -1;
maildir_uidlist_set_all_nonsynced(mbox->uidlist);
}
mbox->synced = TRUE;
mbox->sync_uidlist_refreshed = FALSE;
return index_mailbox_sync_init(box, flags, ret < 0);
}
int maildir_sync_is_synced(struct maildir_mailbox *mbox)
{
bool new_changed, cur_changed;
enum maildir_scan_why why;
int ret;
T_BEGIN {
const char *box_path = mailbox_get_path(&mbox->box);
const char *new_dir, *cur_dir;
new_dir = t_strconcat(box_path, "/new", NULL);
cur_dir = t_strconcat(box_path, "/cur", NULL);
ret = maildir_sync_quick_check(mbox, FALSE, new_dir, cur_dir,
&new_changed, &cur_changed,
&why);
} T_END;
return ret < 0 ? -1 : (!new_changed && !cur_changed);
}