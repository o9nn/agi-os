#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "nntpdriver_message.h"
#include "mailmessage_tools.h"
#include "nntpdriver_tools.h"
#include "nntpdriver.h"
#include "newsnntp.h"
#include <string.h>
#include <stdlib.h>
static int nntp_prefetch(mailmessage * msg_info);
static void nntp_prefetch_free(struct generic_message_t * msg);
static int nntp_initialize(mailmessage * msg_info);
static int nntp_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len);
static int nntp_fetch_size(mailmessage * msg_info,
size_t * result);
static mailmessage_driver local_nntp_message_driver = {
"nntp",
nntp_initialize,
mailmessage_generic_uninitialize,
mailmessage_generic_flush,
NULL,
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
NULL
};
mailmessage_driver * nntp_message_driver = &local_nntp_message_driver;
static int nntp_prefetch(mailmessage * msg_info)
{
char * msg_content;
size_t msg_length;
struct generic_message_t * msg;
int r;
r = nntpdriver_article(msg_info->msg_session, msg_info->msg_index,
&msg_content, &msg_length);
if (r != MAIL_NO_ERROR)
return r;
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
static int nntp_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len)
{
struct generic_message_t * msg;
char * headers;
size_t headers_length;
int r;
msg = msg_info->msg_data;
if (msg->msg_message != NULL)
return mailmessage_generic_fetch_header(msg_info,
result, result_len);
r = nntpdriver_head(msg_info->msg_session, msg_info->msg_index,
&headers, &headers_length);
if (r != MAIL_NO_ERROR)
return r;
* result = headers;
* result_len = headers_length;
return MAIL_NO_ERROR;
}
static int nntp_fetch_size(mailmessage * msg_info,
size_t * result)
{
return nntpdriver_size(msg_info->msg_session, msg_info->msg_index, result);
}