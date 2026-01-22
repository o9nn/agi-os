#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "maildirdriver_message.h"
#include "maildirdriver_tools.h"
#include "mailmessage_tools.h"
#include "maildirdriver.h"
#include "maildir.h"
#include "generic_cache.h"
#ifdef HAVE_UNISTD_H
#	include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#	include <sys/mman.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#ifdef WIN32
#	include "win_etpan.h"
#endif
static int get_flags(mailmessage * msg_info,
struct mail_flags ** result);
static int prefetch(mailmessage * msg_info);
static void prefetch_free(struct generic_message_t * msg);
static int initialize(mailmessage * msg_info);
static void check(mailmessage * msg_info);
static mailmessage_driver local_maildir_message_driver = {
"maildir",
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
mailmessage_driver * maildir_message_driver = &local_maildir_message_driver;
struct maildir_msg_data {
int fd;
};
static inline struct maildir_session_state_data *
get_session_data(mailmessage * msg)
{
return msg->msg_session->sess_data;
}
static struct maildir * get_maildir_session(mailmessage * msg)
{
return get_session_data(msg)->md_session;
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
if (msg_info->msg_uid == NULL) {
res = MAIL_ERROR_INVAL;
goto err;
}
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
mail_flags_store_set(get_session_data(msg_info)->md_flags_store,
msg_info);
}
}
static int get_flags(mailmessage * msg_info,
struct mail_flags ** result)
{
chashdatum key;
chashdatum value;
struct maildir * md;
struct mail_flags * flags;
struct maildir_session_state_data * data;
struct maildir_msg * md_msg;
int r;
uint32_t driver_flags;
clist * ext;
if (msg_info->msg_flags != NULL) {
* result = msg_info->msg_flags;
return MAIL_NO_ERROR;
}
data = get_session_data(msg_info);
flags = mail_flags_store_get(data->md_flags_store,
msg_info->msg_index);
if (flags != NULL) {
msg_info->msg_flags = flags;
* result = msg_info->msg_flags;
return MAIL_NO_ERROR;
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
ext = clist_new();
if (ext == NULL)
return MAIL_ERROR_MEMORY;
msg_info->msg_flags = mail_flags_new(driver_flags, ext);
* result = msg_info->msg_flags;
return MAIL_NO_ERROR;
}