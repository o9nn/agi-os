#include "lib.h"
#include "ioloop.h"
#include "str.h"
#include "read-full.h"
#include "write-full.h"
#include "eacces-error.h"
#include "mail-user.h"
#include "mailbox-list.h"
#include "mailbox-uidvalidity.h"
#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>
#define RETRY_COUNT 10
static uint32_t mailbox_uidvalidity_next_fallback(void)
{
static uint32_t uid_validity = 0;
if (uid_validity < ioloop_time32)
uid_validity = ioloop_time32;
else
uid_validity++;
if (uid_validity == 0)
uid_validity = 1;
return uid_validity;
}
static void mailbox_uidvalidity_write(struct mailbox_list *list,
const char *path, uint32_t uid_validity)
{
struct mail_user *user = mailbox_list_get_user(list);
char buf[8+1];
int fd;
struct mailbox_permissions perm;
mode_t old_mask;
mailbox_list_get_root_permissions(list, &perm);
old_mask = umask(0666 & ~perm.file_create_mode);
fd = open(path, O_RDWR | O_CREAT, 0666);
umask(old_mask);
if (fd == -1) {
e_error(user->event, "open(%s) failed: %m", path);
return;
}
if (perm.file_create_gid != (gid_t)-1 &&
fchown(fd, (uid_t)-1, perm.file_create_gid) < 0) {
if (errno == EPERM) {
e_error(user->event, "%s",
eperm_error_get_chgrp("fchown", path,
perm.file_create_gid,
perm.file_create_gid_origin));
} else {
e_error(mailbox_list_get_user(list)->event,
"fchown(%s, -1, %ld) failed: %m",
path, (long)perm.file_create_gid);
}
}
if (i_snprintf(buf, sizeof(buf), "%08x", uid_validity) < 0)
i_unreached();
if (pwrite_full(fd, buf, strlen(buf), 0) < 0)
e_error(user->event, "write(%s) failed: %m", path);
if (close(fd) < 0)
e_error(user->event, "close(%s) failed: %m", path);
}
static int
mailbox_uidvalidity_rename(struct mailbox_list *list, const char *path,
uint32_t *uid_validity, bool log_enoent)
{
string_t *src, *dest;
unsigned int i;
size_t prefix_len;
int ret;
src = t_str_new(256);
str_append(src, path);
dest = t_str_new(256);
str_append(dest, path);
prefix_len = str_len(src);
for (i = 0; i < RETRY_COUNT; i++) {
str_truncate(src, prefix_len);
str_truncate(dest, prefix_len);
str_printfa(src, ".%08x", *uid_validity);
*uid_validity += 1;
if (*uid_validity == 0)
*uid_validity += 1;
str_printfa(dest, ".%08x", *uid_validity);
if ((ret = rename(str_c(src), str_c(dest))) == 0 ||
errno != ENOENT)
break;
}
if (ret < 0 && (errno != ENOENT || log_enoent))
e_error(mailbox_list_get_user(list)->event,
"rename(%s, %s) failed: %m", str_c(src), str_c(dest));
return ret;
}
static uint32_t
mailbox_uidvalidity_next_rescan(struct mailbox_list *list, const char *path)
{
DIR *d;
struct dirent *dp;
const char *fname, *dir, *prefix, *tmp;
unsigned int i;
size_t prefix_len;
uint32_t cur_value, min_value, max_value;
mode_t old_mask;
int fd;
fname = strrchr(path, '/');
if (fname == NULL) {
dir = ".";
fname = path;
} else {
dir = t_strdup_until(path, fname);
fname++;
}
d = opendir(dir);
if (d == NULL && errno == ENOENT) {
(void)mailbox_list_mkdir_root(list, dir, MAILBOX_LIST_PATH_TYPE_CONTROL);
d = opendir(dir);
}
if (d == NULL) {
e_error(mailbox_list_get_user(list)->event,
"opendir(%s) failed: %m", dir);
return mailbox_uidvalidity_next_fallback();
}
prefix = t_strconcat(fname, ".", NULL);
prefix_len = strlen(prefix);
max_value = 0; min_value = (uint32_t)-1;
while ((dp = readdir(d)) != NULL) {
if (strncmp(dp->d_name, prefix, prefix_len) == 0) {
if (str_to_uint32_hex(dp->d_name + prefix_len, &cur_value) >= 0) {
if (min_value > cur_value)
min_value = cur_value;
if (max_value < cur_value)
max_value = cur_value;
}
}
}
if (closedir(d) < 0)
e_error(mailbox_list_get_user(list)->event,
"closedir(%s) failed: %m", dir);
if (max_value == 0) {
for (i = 0; i < RETRY_COUNT; i++) {
cur_value = mailbox_uidvalidity_next_fallback();
tmp = t_strdup_printf("%s.%08x", path, cur_value);
old_mask = umask(0);
fd = open(tmp, O_RDWR | O_CREAT | O_EXCL, 0444);
umask(old_mask);
if (fd != -1 || errno != EEXIST)
break;
}
if (fd == -1) {
e_error(mailbox_list_get_user(list)->event,
"creat(%s) failed: %m", tmp);
return cur_value;
}
i_close_fd(&fd);
mailbox_uidvalidity_write(list, path, cur_value);
return cur_value;
}
if (min_value != max_value) {
tmp = t_strdup_printf("%s.%08x", path, min_value);
i_unlink_if_exists(tmp);
}
cur_value = max_value;
if (mailbox_uidvalidity_rename(list, path, &cur_value, TRUE) < 0)
return mailbox_uidvalidity_next_fallback();
mailbox_uidvalidity_write(list, path, cur_value);
return cur_value;
}
uint32_t mailbox_uidvalidity_next(struct mailbox_list *list, const char *path)
{
struct mail_user *user = mailbox_list_get_user(list);
char buf[8+1];
uint32_t cur_value;
int fd, ret;
fd = open(path, O_RDWR);
if (fd == -1) {
if (errno != ENOENT)
e_error(user->event, "open(%s) failed: %m", path);
return mailbox_uidvalidity_next_rescan(list, path);
}
ret = read_full(fd, buf, sizeof(buf)-1);
if (ret < 0) {
e_error(user->event, "read(%s) failed: %m", path);
i_close_fd(&fd);
return mailbox_uidvalidity_next_rescan(list, path);
}
buf[sizeof(buf)-1] = 0;
if (ret == 0 || str_to_uint32_hex(buf, &cur_value) < 0 ||
cur_value == 0) {
i_close_fd(&fd);
return mailbox_uidvalidity_next_rescan(list, path);
}
if (mailbox_uidvalidity_rename(list, path, &cur_value, FALSE) < 0) {
i_close_fd(&fd);
return mailbox_uidvalidity_next_rescan(list, path);
}
if (i_snprintf(buf, sizeof(buf), "%08x", cur_value) < 0)
i_unreached();
if (pwrite_full(fd, buf, strlen(buf), 0) < 0)
e_error(user->event, "write(%s) failed: %m", path);
if (close(fd) < 0)
e_error(user->event, "close(%s) failed: %m", path);
return cur_value;
}