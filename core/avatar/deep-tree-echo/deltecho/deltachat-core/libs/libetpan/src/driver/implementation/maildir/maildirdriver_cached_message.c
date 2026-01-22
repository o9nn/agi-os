#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "maildirdriver_message.h"
#include "mailmessage_tools.h"
#include "maildirdriver.h"
#include "maildir.h"
#include "generic_cache.h"
#include "mail_cache_db.h"
#include "maildirdriver_tools.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#	include <sys/mman.h>
#endif
#ifdef WIN32
#	include "win_etpan.h"
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
static int get_flags(mailmessage * msg_info,
struct mail_flags ** result);
static int prefetch(mailmessage * msg_info);
static void prefetch_free(struct generic_message_t * msg);
static int initialize(mailmessage * msg_info);
static void check(mailmessage * msg_info);
static mailmessage_driver local_maildir_cached_message_driver = {
"maildir-cached",
initialize,
mailmessage_generic_uninitialize,
mailmessage_generic_flush,
check,
mailmessage_generic_fetch_result_free,
mailmessage_generic_fetch,
mailmessage_generic_fetch_header,
mailmessage_generic_fetch_header,
NULL,
mailmessage_generic_get_bodystructure,
mailmessage_generic_fetch_section,
mailmessage_generic_fetch_section_header,
mailmessage_generic_fetch_section_mime,
mailmessage_generic_fetch_section_body,
mailmessage_generic_fetch_envelope,
get_flags
};
mailmessage_driver * maildir_cached_message_driver =
&local_maildir_cached_message_driver;
struct maildir_msg_data {
int fd;
};
#if 0
static inline struct maildir_cached_session_state_data *
get_cached_session_data(mailmessage * msg)
{
return msg->session->data;
}
static inline mailsession * cached_session_get_ancestor(mailsession * session)
{
return get_data(session)->session;
}
static inline struct maildir_session_state_data *
cached_session_get_ancestor_data(mailsession * session)
{
return get_ancestor(session)->data;
}
static struct maildir * get_maildir_session(mailmessage * msg)
{
return cached_session_get_ancestor_data(msg->session)->session;
}
#endif
static inline struct maildir_cached_session_state_data *
get_cached_session_data(mailmessage * msg)
{
return msg->msg_session->sess_data;
}
static inline struct maildir_cached_session_state_data *
cached_session_get_data(mailsession * s)
{
return s->sess_data;
}
static inline mailsession * cached_session_get_ancestor(mailsession * s)
{
return cached_session_get_data(s)->md_ancestor;
}
static inline struct maildir_session_state_data *
cached_session_get_ancestor_data(mailsession * s)
{
return cached_session_get_ancestor(s)->sess_data;
}
static inline struct maildir_session_state_data *
get_session_ancestor_data(mailmessage * msg)
{
return cached_session_get_ancestor_data(msg->msg_session);
}
static inline struct maildir *
cached_session_get_maildir_session(mailsession * session)
{
return cached_session_get_ancestor_data(session)->md_session;
}
static inline struct maildir * get_maildir_session(mailmessage * msg)
{
return cached_session_get_maildir_session(msg->msg_session);
}
static int prefetch(mailmessage * msg_info)
{
struct generic_message_t * msg;
int res;
struct maildir_msg_data * data;
char * filename;
int fd;
char * mapping;
struct maildir * md;
md = get_maildir_session(msg_info);
filename = maildir_message_get(md, msg_info->msg_uid);
if (filename == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
fd = open(filename, O_RDONLY);
free(filename);
if (fd == -1) {
res = MAIL_ERROR_FILE;
goto err;
}
mapping = mmap(NULL, msg_info->msg_size, PROT_READ, MAP_PRIVATE, fd, 0);
if (mapping == (char *)MAP_FAILED) {
res = MAIL_ERROR_FILE;
goto close;
}
data = malloc(sizeof(* data));
if (data == NULL) {
res = MAIL_ERROR_MEMORY;
goto unmap;
}
data->fd = fd;
msg = msg_info->msg_data;
msg->msg_data = data;
msg->msg_message = mapping;
msg->msg_length = msg_info->msg_size;
return MAIL_NO_ERROR;
unmap:
munmap(mapping, msg_info->msg_size);
close:
close(fd);
err:
return res;
}
static void prefetch_free(struct generic_message_t * msg)
{
if (msg->msg_message != NULL) {
struct maildir_msg_data * data;
munmap(msg->msg_message, msg->msg_length);
msg->msg_message = NULL;
data = msg->msg_data;
close(data->fd);
free(data);
}
}
static int initialize(mailmessage * msg_info)
{
struct generic_message_t * msg;
int r;
r = mailmessage_generic_initialize(msg_info);
if (r != MAIL_NO_ERROR)
return r;
msg = msg_info->msg_data;
msg->msg_prefetch = prefetch;
msg->msg_prefetch_free = prefetch_free;
return MAIL_NO_ERROR;
}
static void check(mailmessage * msg_info)
{
if (msg_info->msg_flags != NULL) {
mail_flags_store_set(get_session_ancestor_data(msg_info)->md_flags_store, msg_info);
mail_flags_store_set(get_cached_session_data(msg_info)->md_flags_store, msg_info);
}
}
#define FLAGS_NAME "flags.db"
static int get_flags(mailmessage * msg_info,
struct mail_flags ** result)
{
struct mail_cache_db * cache_db_flags;
chashdatum key;
chashdatum value;
struct maildir * md;
struct mail_flags * flags;
struct maildir_cached_session_state_data * data;
struct maildir_msg * md_msg;
int r;
uint32_t driver_flags;
char filename_flags[PATH_MAX];
char keyname[PATH_MAX];
MMAPString * mmapstr;
if (msg_info->msg_flags != NULL) {
* result = msg_info->msg_flags;
return MAIL_NO_ERROR;
}
data = get_cached_session_data(msg_info);
flags = mail_flags_store_get(data->md_flags_store,
msg_info->msg_index);
if (flags != NULL) {
msg_info->msg_flags = flags;
* result = msg_info->msg_flags;
return MAIL_NO_ERROR;
}
snprintf(filename_flags, PATH_MAX, "%s%c%s%c%s",
data->md_flags_directory, MAIL_DIR_SEPARATOR, data->md_quoted_mb,
MAIL_DIR_SEPARATOR, FLAGS_NAME);
r = mail_cache_db_open_lock(filename_flags, &cache_db_flags);
if (r < 0)
return MAIL_ERROR_FILE;
snprintf(keyname, PATH_MAX, "%s-flags", msg_info->msg_uid);
mmapstr = mmap_string_new("");
if (mmapstr == NULL) {
mail_cache_db_close_unlock(filename_flags, cache_db_flags);
return MAIL_ERROR_MEMORY;
}
r = generic_cache_flags_read(cache_db_flags, mmapstr, keyname, &flags);
mmap_string_free(mmapstr);
mail_cache_db_close_unlock(filename_flags, cache_db_flags);
if (r != MAIL_NO_ERROR) {
flags = mail_flags_new_empty();
if (flags == NULL)
return MAIL_ERROR_MEMORY;
}
md = get_maildir_session(msg_info);
if (md == NULL)
return MAIL_ERROR_BAD_STATE;
key.data = msg_info->msg_uid;
key.len = (unsigned int) strlen(msg_info->msg_uid);
r = chash_get(md->mdir_msg_hash, &key, &value);
if (r < 0)
return MAIL_ERROR_MSG_NOT_FOUND;
md_msg = value.data;
driver_flags = maildirdriver_maildir_flags_to_flags(md_msg->msg_flags);
flags->fl_flags = driver_flags;
msg_info->msg_flags = flags;
* result = msg_info->msg_flags;
return MAIL_NO_ERROR;
}