#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "pop3driver_message.h"
#include "mailmessage_tools.h"
#include "pop3driver_tools.h"
#include "pop3driver.h"
#include "mailpop3.h"
#include <stdlib.h>
#include <string.h>
static int pop3_prefetch(mailmessage * msg_info);
static void pop3_prefetch_free(struct generic_message_t * msg);
static int pop3_initialize(mailmessage * msg_info);
static int pop3_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len);
static int pop3_fetch_size(mailmessage * msg_info,
size_t * result);
static mailmessage_driver local_pop3_message_driver = {
"pop3",
pop3_initialize,
mailmessage_generic_uninitialize,
mailmessage_generic_flush,
NULL,
mailmessage_generic_fetch_result_free,
mailmessage_generic_fetch,
pop3_fetch_header,
mailmessage_generic_fetch_body,
pop3_fetch_size,
mailmessage_generic_get_bodystructure,
mailmessage_generic_fetch_section,
mailmessage_generic_fetch_section_header,
mailmessage_generic_fetch_section_mime,
mailmessage_generic_fetch_section_body,
mailmessage_generic_fetch_envelope,
NULL
};
mailmessage_driver * pop3_message_driver = &local_pop3_message_driver;
static inline struct pop3_session_state_data *
get_data(mailsession * session)
{
return session->sess_data;
}
static mailpop3 * get_pop3_session(mailsession * session)
{
return get_data(session)->pop3_session;
}
static int pop3_prefetch(mailmessage * msg_info)
{
char * msg_content;
size_t msg_length;
struct generic_message_t * msg;
int r;
r = pop3driver_retr(msg_info->msg_session, msg_info->msg_index,
&msg_content, &msg_length);
if (r != MAIL_NO_ERROR)
return r;
msg = msg_info->msg_data;
msg->msg_message = msg_content;
msg->msg_length = msg_length;
return MAIL_NO_ERROR;
}
static void pop3_prefetch_free(struct generic_message_t * msg)
{
if (msg->msg_message != NULL) {
mmap_string_unref(msg->msg_message);
msg->msg_message = NULL;
}
}
static int pop3_initialize(mailmessage * msg_info)
{
struct generic_message_t * msg;
int r;
char * uid;
struct mailpop3_msg_info * info;
mailpop3 * pop3;
pop3 = get_pop3_session(msg_info->msg_session);
r = mailpop3_get_msg_info(pop3, msg_info->msg_index, &info);
switch (r) {
case MAILPOP3_NO_ERROR:
break;
default:
return pop3driver_pop3_error_to_mail_error(r);
}
uid = strdup(info->msg_uidl);
if (uid == NULL)
return MAIL_ERROR_MEMORY;
r = mailmessage_generic_initialize(msg_info);
if (r != MAIL_NO_ERROR) {
free(uid);
return r;
}
msg = msg_info->msg_data;
msg->msg_prefetch = pop3_prefetch;
msg->msg_prefetch_free = pop3_prefetch_free;
msg_info->msg_uid = uid;
return MAIL_NO_ERROR;
}
static int pop3_fetch_header(mailmessage * msg_info,
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
r = pop3driver_header(msg_info->msg_session, msg_info->msg_index,
&headers, &headers_length);
if (r != MAIL_NO_ERROR)
return r;
* result = headers;
* result_len = headers_length;
return MAIL_NO_ERROR;
}
static int pop3_fetch_size(mailmessage * msg_info,
size_t * result)
{
return pop3driver_size(msg_info->msg_session, msg_info->msg_index, result);
}