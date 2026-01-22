#include "lib.h"
#include "nfs-workarounds.h"
#include "mail-index-private.h"
int mail_index_lock_fd(struct mail_index *index, const char *path, int fd,
int lock_type, unsigned int timeout_secs,
struct file_lock **lock_r)
{
const char *error;
int ret;
if (fd == -1) {
i_assert(MAIL_INDEX_IS_IN_MEMORY(index));
return 1;
}
struct file_lock_settings lock_set = {
.lock_method = index->set.lock_method,
};
ret = file_wait_lock(fd, path, lock_type, &lock_set, timeout_secs,
lock_r, &error);
if (ret < 0)
e_error(index->event, "%s", error);
return ret;
}
void mail_index_flush_read_cache(struct mail_index *index, const char *path,
int fd, bool locked)
{
if ((index->flags & MAIL_INDEX_OPEN_FLAG_NFS_FLUSH) == 0)
return;
if (locked &&
(index->set.lock_method == FILE_LOCK_METHOD_FCNTL ||
index->set.lock_method == FILE_LOCK_METHOD_FLOCK)) {
nfs_flush_read_cache_locked(path, fd);
} else {
nfs_flush_read_cache_unlocked(path, fd);
}
}