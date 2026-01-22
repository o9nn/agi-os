#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mboxdriver_message.h"
#include "mailmessage_tools.h"
#include "mboxdriver_tools.h"
#include "mboxdriver.h"
#include "mailmbox.h"
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
static int mbox_prefetch(mailmessage * msg_info);
static void mbox_prefetch_free(struct generic_message_t * msg);
static int mbox_initialize(mailmessage * msg_info);
static int mbox_fetch_size(mailmessage * msg_info,
size_t * result);
static int mbox_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len);
static mailmessage_driver local_mbox_message_driver = {
"mbox",
mbox_initialize,
mailmessage_generic_uninitialize,
mailmessage_generic_flush,
NULL,
mailmessage_generic_fetch_result_free,
mailmessage_generic_fetch,
mbox_fetch_header,
mailmessage_generic_fetch_body,
mbox_fetch_size,
mailmessage_generic_get_bodystructure,
mailmessage_generic_fetch_section,
mailmessage_generic_fetch_section_header,
mailmessage_generic_fetch_section_mime,
mailmessage_generic_fetch_section_body,
mailmessage_generic_fetch_envelope,
NULL
};
mailmessage_driver * mbox_message_driver = &local_mbox_message_driver;
static inline struct mbox_session_state_data * get_data(mailmessage * msg)
{
return msg->msg_session->sess_data;
}
static inline struct mailmbox_folder * get_mbox_session(mailmessage * msg)
{
return get_data(msg)->mbox_folder;
}
static int mbox_prefetch(mailmessage * msg_info)
{
struct generic_message_t * msg;
int r;
char * msg_content;
size_t msg_length;
r = mboxdriver_fetch_msg(msg_info->msg_session, msg_info->msg_index,
&msg_content, &msg_length);
if (r != MAIL_NO_ERROR)
return r;
msg = msg_info->msg_data;
msg->msg_message = msg_content;
msg->msg_length = msg_length;
return MAIL_NO_ERROR;
}
static void mbox_prefetch_free(struct generic_message_t * msg)
{
if (msg->msg_message != NULL) {
mmap_string_unref(msg->msg_message);
msg->msg_message = NULL;
}
}
static int mbox_initialize(mailmessage * msg_info)
{
struct generic_message_t * msg;
int r;
char * uid;
char static_uid[PATH_MAX];
struct mailmbox_msg_info * info;
struct mailmbox_folder * folder;
int res;
chashdatum key;
chashdatum data;
folder = get_mbox_session(msg_info);
if (folder == NULL) {
res = MAIL_ERROR_BAD_STATE;
goto err;
}
key.data = &msg_info->msg_index;
key.len = sizeof(msg_info->msg_index);
r = chash_get(folder->mb_hash, &key, &data);
if (r < 0) {
res = MAIL_ERROR_MSG_NOT_FOUND;
goto err;
}
info = data.data;
snprintf(static_uid, PATH_MAX, "%u-%lu",
msg_info->msg_index, (unsigned long) info->msg_body_len);
uid = strdup(static_uid);
if (uid == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
r = mailmessage_generic_initialize(msg_info);
if (r != MAIL_NO_ERROR) {
free(uid);
res = r;
goto err;
}
msg = msg_info->msg_data;
msg->msg_prefetch = mbox_prefetch;
msg->msg_prefetch_free = mbox_prefetch_free;
msg_info->msg_uid = uid;
return MAIL_NO_ERROR;
err:
return res;
}
static int mbox_fetch_size(mailmessage * msg_info,
size_t * result)
{
int r;
size_t size;
r = mboxdriver_fetch_size(msg_info->msg_session,
msg_info->msg_index, &size);
if (r != MAIL_NO_ERROR)
return r;
* result = size;
return MAIL_NO_ERROR;
}
static int mbox_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len)
{
struct generic_message_t * msg;
int r;
char * msg_content;
size_t msg_length;
msg = msg_info->msg_data;
if (msg->msg_message != NULL) {
return mailmessage_generic_fetch_header(msg_info, result, result_len);
}
else {
r = mboxdriver_fetch_header(msg_info->msg_session, msg_info->msg_index,
&msg_content, &msg_length);
if (r != MAIL_NO_ERROR)
return r;
* result = msg_content;
* result_len = msg_length;
return MAIL_NO_ERROR;
}
}