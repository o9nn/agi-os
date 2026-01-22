#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "nntpdriver_cached_message.h"
#include <string.h>
#include <stdlib.h>
#include "mail_cache_db.h"
#include "mailmessage.h"
#include "mailmessage_tools.h"
#include "nntpdriver.h"
#include "nntpdriver_tools.h"
#include "nntpdriver_cached.h"
#include "nntpdriver_message.h"
#include "generic_cache.h"
static int nntp_prefetch(mailmessage * msg_info);
static void nntp_prefetch_free(struct generic_message_t * msg);
static int nntp_initialize(mailmessage * msg_info);
static int nntp_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len);
static int nntp_fetch_size(mailmessage * msg_info,
size_t * result);
static void nntp_uninitialize(mailmessage * msg_info);
static void nntp_flush(mailmessage * msg_info);
static void nntp_check(mailmessage * msg_info);
static int nntp_get_flags(mailmessage * msg_info,
struct mail_flags ** result);
static mailmessage_driver local_nntp_cached_message_driver = {
"nntp-cached",
nntp_initialize,
nntp_uninitialize,
nntp_flush,
nntp_check,
mailmessage_generic_fetch_result_free,
mailmessage_generic_fetch,
nntp_fetch_header,
mailmessage_generic_fetch_body,
nntp_fetch_size,
mailmessage_generic_get_bodystructure,
mailmessage_generic_fetch_section,
mailmessage_generic_fetch_section_header,
mailmessage_generic_fetch_section_mime,
mailmessage_generic_fetch_section_body,
mailmessage_generic_fetch_envelope,
nntp_get_flags
};
mailmessage_driver * nntp_cached_message_driver =
&local_nntp_cached_message_driver;
static inline struct nntp_cached_session_state_data *
get_cached_session_data(mailmessage * msg)
{
return msg->msg_session->sess_data;
}
static inline mailsession * get_ancestor_session(mailmessage * msg)
{
return get_cached_session_data(msg)->nntp_ancestor;
}
static inline struct nntp_session_state_data *
get_ancestor_session_data(mailmessage * msg)
{
return get_ancestor_session(msg)->sess_data;
}
static inline newsnntp *
get_nntp_session(mailmessage * msg)
{
return get_ancestor_session_data(msg)->nntp_session;
}
static int nntp_prefetch(mailmessage * msg_info)
{
char * msg_content;
size_t msg_length;
struct generic_message_t * msg;
int r;
struct nntp_cached_session_state_data * cached_data;
struct nntp_session_state_data * ancestor_data;
char filename[PATH_MAX];
cached_data = get_cached_session_data(msg_info);
ancestor_data = get_ancestor_session_data(msg_info);
snprintf(filename, PATH_MAX, "%s/%s/%i", cached_data->nntp_cache_directory,
ancestor_data->nntp_group_name, msg_info->msg_index);
r = generic_cache_read(filename, &msg_content, &msg_length);
if (r == MAIL_NO_ERROR) {
msg = msg_info->msg_data;
msg->msg_message = msg_content;
msg->msg_length = msg_length;
return MAIL_NO_ERROR;
}
r = nntpdriver_article(get_ancestor_session(msg_info),
msg_info->msg_index, &msg_content,
&msg_length);
if (r != MAIL_NO_ERROR)
return r;
generic_cache_store(filename, msg_content, msg_length);
msg = msg_info->msg_data;
msg->msg_message = msg_content;
msg->msg_length = msg_length;
return MAIL_NO_ERROR;
}
static void nntp_prefetch_free(struct generic_message_t * msg)
{
if (msg->msg_message != NULL) {
mmap_string_unref(msg->msg_message);
msg->msg_message = NULL;
}
}
static int nntp_initialize(mailmessage * msg_info)
{
struct generic_message_t * msg;
int r;
char * uid;
char static_uid[20];
snprintf(static_uid, 20, "%u", msg_info->msg_index);
uid = strdup(static_uid);
if (uid == NULL)
return MAIL_ERROR_MEMORY;
r = mailmessage_generic_initialize(msg_info);
if (r != MAIL_NO_ERROR) {
free(uid);
return r;
}
msg = msg_info->msg_data;
msg->msg_prefetch = nntp_prefetch;
msg->msg_prefetch_free = nntp_prefetch_free;
msg_info->msg_uid = uid;
return MAIL_NO_ERROR;
}
static void nntp_uninitialize(mailmessage * msg_info)
{
mailmessage_generic_uninitialize(msg_info);
}
#define FLAGS_NAME "flags.db"
static void nntp_flush(mailmessage * msg_info)
{
mailmessage_generic_flush(msg_info);
}
static void nntp_check(mailmessage * msg_info)
{
if (msg_info->msg_flags != NULL) {
mail_flags_store_set(get_cached_session_data(msg_info)->nntp_flags_store,
msg_info);
}
}
static int nntp_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len)
{
struct generic_message_t * msg;
char * headers;
size_t headers_length;
struct nntp_cached_session_state_data * cached_data;
struct nntp_session_state_data * ancestor_data;
int r;
char filename[PATH_MAX];
msg = msg_info->msg_data;
if (msg->msg_message != NULL)
return mailmessage_generic_fetch_header(msg_info,
result, result_len);
cached_data = get_cached_session_data(msg_info);
ancestor_data = get_ancestor_session_data(msg_info);
snprintf(filename, PATH_MAX, "%s/%s/%i-header",
cached_data->nntp_cache_directory,
ancestor_data->nntp_group_name, msg_info->msg_index);
r = generic_cache_read(filename, &headers, &headers_length);
if (r == MAIL_NO_ERROR) {
* result = headers;
* result_len = headers_length;
return MAIL_NO_ERROR;
}
r = nntpdriver_head(get_ancestor_session(msg_info), msg_info->msg_index,
&headers, &headers_length);
if (r != MAIL_NO_ERROR)
return r;
generic_cache_store(filename, headers, headers_length);
* result = headers;
* result_len = headers_length;
return MAIL_NO_ERROR;
}
static int nntp_fetch_size(mailmessage * msg_info,
size_t * result)
{
return nntpdriver_size(get_ancestor_session(msg_info),
msg_info->msg_index, result);
}
static int nntp_get_flags(mailmessage * msg_info,
struct mail_flags ** result)
{
int r;
struct mail_flags * flags;
struct mail_cache_db * cache_db_flags;
char filename_flags[PATH_MAX];
int res;
MMAPString * mmapstr;
if (msg_info->msg_flags != NULL) {
* result = msg_info->msg_flags;
return MAIL_NO_ERROR;
}
flags = mail_flags_store_get(get_cached_session_data(msg_info)->nntp_flags_store, msg_info->msg_index);
if (flags == NULL) {
struct nntp_cached_session_state_data * cached_data;
struct nntp_session_state_data * ancestor_data;
cached_data = get_cached_session_data(msg_info);
ancestor_data = get_ancestor_session_data(msg_info);
if (ancestor_data->nntp_group_name == NULL) {
res = MAIL_ERROR_BAD_STATE;
goto err;
}
snprintf(filename_flags, PATH_MAX, "%s/%s/%s",
cached_data->nntp_flags_directory,
ancestor_data->nntp_group_name, FLAGS_NAME);
r = mail_cache_db_open_lock(filename_flags, &cache_db_flags);
if (r < 0) {
res = MAIL_ERROR_FILE;
goto err;
}
mmapstr = mmap_string_new("");
if (mmapstr == NULL) {
res = MAIL_ERROR_MEMORY;
goto close_db_flags;
}
r = nntpdriver_get_cached_flags(cache_db_flags, mmapstr,
msg_info->msg_index, &flags);
if (r != MAIL_NO_ERROR) {
flags = mail_flags_new_empty();
if (flags == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_mmapstr;
}
}
mmap_string_free(mmapstr);
mail_cache_db_close_unlock(filename_flags, cache_db_flags);
}
msg_info->msg_flags = flags;
* result = flags;
return MAIL_NO_ERROR;
free_mmapstr:
mmap_string_free(mmapstr);
close_db_flags:
mail_cache_db_close_unlock(filename_flags, cache_db_flags);
err:
return res;
}