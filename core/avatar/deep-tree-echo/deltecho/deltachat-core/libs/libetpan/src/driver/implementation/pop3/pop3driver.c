#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "pop3driver.h"
#include <string.h>
#include <stdlib.h>
#include "pop3driver_message.h"
#include "maildriver_tools.h"
#include "pop3driver_tools.h"
#include "mailmessage.h"
static int pop3driver_initialize(mailsession * session);
static void pop3driver_uninitialize(mailsession * session);
static int pop3driver_parameters(mailsession * session,
int id, void * value);
static int pop3driver_connect_stream(mailsession * session, mailstream * s);
static int pop3driver_starttls(mailsession * session);
static int pop3driver_login(mailsession * session,
const char * userid, const char * password);
static int pop3driver_logout(mailsession * session);
static int pop3driver_noop(mailsession * session);
static int pop3driver_status_folder(mailsession * session, const char * mb,
uint32_t * result_messages, uint32_t * result_recent,
uint32_t * result_unseen);
static int pop3driver_messages_number(mailsession * session, const char * mb,
uint32_t * result);
static int pop3driver_remove_message(mailsession * session, uint32_t num);
static int pop3driver_get_messages_list(mailsession * session,
struct mailmessage_list ** result);
static int pop3driver_get_message(mailsession * session,
uint32_t num, mailmessage ** result);
static int pop3driver_get_message_by_uid(mailsession * session,
const char * uid, mailmessage ** result);
static int pop3driver_login_sasl(mailsession * session,
const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm);
static mailsession_driver local_pop3_session_driver = {
"pop3",
pop3driver_initialize,
pop3driver_uninitialize,
pop3driver_parameters,
pop3driver_connect_stream,
NULL,
pop3driver_starttls,
pop3driver_login,
pop3driver_logout,
pop3driver_noop,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
pop3driver_status_folder,
pop3driver_messages_number,
pop3driver_messages_number,
pop3driver_messages_number,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
pop3driver_get_message,
pop3driver_get_message_by_uid,
pop3driver_get_messages_list,
maildriver_generic_get_envelopes_list,
pop3driver_remove_message,
pop3driver_login_sasl
};
mailsession_driver * pop3_session_driver = &local_pop3_session_driver;
static inline struct pop3_session_state_data *
get_data(mailsession * session)
{
return session->sess_data;
}
static mailpop3 * get_pop3_session(mailsession * session)
{
return get_data(session)->pop3_session;
}
static int pop3driver_initialize(mailsession * session)
{
struct pop3_session_state_data * data;
mailpop3 * pop3;
pop3 = mailpop3_new(0, NULL);
if (session == NULL)
goto err;
data = malloc(sizeof(* data));
if (data == NULL)
goto free;
data->pop3_session = pop3;
data->pop3_auth_type = POP3DRIVER_AUTH_TYPE_PLAIN;
session->sess_data = data;
return MAIL_NO_ERROR;
free:
mailpop3_free(pop3);
err:
return MAIL_ERROR_MEMORY;
}
static void pop3driver_uninitialize(mailsession * session)
{
struct pop3_session_state_data * data;
data = get_data(session);
mailpop3_free(data->pop3_session);
free(data);
session->sess_data = NULL;
}
static int pop3driver_connect_stream(mailsession * session, mailstream * s)
{
int r;
r = mailpop3_connect(get_pop3_session(session), s);
switch (r) {
case MAILPOP3_NO_ERROR:
return MAIL_NO_ERROR_NON_AUTHENTICATED;
default:
return pop3driver_pop3_error_to_mail_error(r);
}
}
static int pop3driver_starttls(mailsession * session)
{
int r;
mailpop3 * pop3;
pop3 = get_pop3_session(session);
r = mailpop3_socket_starttls(pop3);
return pop3driver_pop3_error_to_mail_error(r);
}
static int pop3driver_parameters(mailsession * session,
int id, void * value)
{
struct pop3_session_state_data * data;
data = get_data(session);
switch (id) {
case POP3DRIVER_SET_AUTH_TYPE:
{
int * param;
param = value;
data->pop3_auth_type = * param;
return MAIL_NO_ERROR;
}
break;
case POP3DRIVER_CACHED_SET_SSL_CALLBACK:
data->pop3_ssl_callback = value;
break;
case POP3DRIVER_CACHED_SET_SSL_CALLBACK_DATA:
data->pop3_ssl_cb_data = value;
break;
}
return MAIL_ERROR_INVAL;
}
static int pop3driver_login(mailsession * session,
const char * userid, const char * password)
{
int r;
carray * msg_tab;
struct pop3_session_state_data * data;
data = get_data(session);
switch (data->pop3_auth_type) {
case POP3DRIVER_AUTH_TYPE_TRY_APOP:
r = mailpop3_login_apop(get_pop3_session(session), userid, password);
if (r != MAILPOP3_NO_ERROR)
r = mailpop3_login(get_pop3_session(session), userid, password);
break;
case POP3DRIVER_AUTH_TYPE_APOP:
r = mailpop3_login_apop(get_pop3_session(session), userid, password);
break;
default:
case POP3DRIVER_AUTH_TYPE_PLAIN:
r = mailpop3_login(get_pop3_session(session), userid, password);
break;
}
if (r != MAILPOP3_NO_ERROR)
return pop3driver_pop3_error_to_mail_error(r);
r = mailpop3_list(get_pop3_session(session), &msg_tab);
return pop3driver_pop3_error_to_mail_error(r);
}
static int pop3driver_logout(mailsession * session)
{
int r;
r = mailpop3_quit(get_pop3_session(session));
return pop3driver_pop3_error_to_mail_error(r);
}
static int pop3driver_noop(mailsession * session)
{
int r;
r = mailpop3_noop(get_pop3_session(session));
return pop3driver_pop3_error_to_mail_error(r);
}
static int pop3driver_status_folder(mailsession * session, const char * mb,
uint32_t * result_messages,
uint32_t * result_recent,
uint32_t * result_unseen)
{
uint32_t count;
int r;
count = 0;
r = pop3driver_messages_number(session, mb, &count);
if (r != MAIL_NO_ERROR)
return r;
* result_messages = count;
* result_recent = count;
* result_unseen = count;
return MAIL_NO_ERROR;
}
static int pop3driver_messages_number(mailsession * session, const char * mb,
uint32_t * result)
{
carray * msg_tab;
int r;
r = mailpop3_list(get_pop3_session(session), &msg_tab);
if (r != MAILPOP3_NO_ERROR) {
return pop3driver_pop3_error_to_mail_error(r);
}
* result = carray_count(msg_tab) -
get_pop3_session(session)->pop3_deleted_count;
return MAIL_NO_ERROR;
}
static int pop3driver_remove_message(mailsession * session, uint32_t num)
{
mailpop3 * pop3;
int r;
pop3 = get_pop3_session(session);
r = mailpop3_dele(pop3, num);
switch (r) {
case MAILPOP3_ERROR_BAD_STATE:
return MAIL_ERROR_BAD_STATE;
case MAILPOP3_ERROR_NO_SUCH_MESSAGE:
return MAIL_ERROR_MSG_NOT_FOUND;
case MAILPOP3_ERROR_STREAM:
return MAIL_ERROR_STREAM;
case MAILPOP3_NO_ERROR:
return MAIL_NO_ERROR;
default:
return MAIL_ERROR_REMOVE;
}
}
static int pop3driver_get_messages_list(mailsession * session,
struct mailmessage_list ** result)
{
mailpop3 * pop3;
pop3 = get_pop3_session(session);
return pop3_get_messages_list(pop3, session,
pop3_message_driver, result);
}
static int pop3driver_get_message(mailsession * session,
uint32_t num, mailmessage ** result)
{
mailmessage * msg_info;
int r;
msg_info = mailmessage_new();
if (msg_info == NULL)
return MAIL_ERROR_MEMORY;
r = mailmessage_init(msg_info, session, pop3_message_driver, num, 0);
if (r != MAIL_NO_ERROR) {
mailmessage_free(msg_info);
return r;
}
* result = msg_info;
return MAIL_NO_ERROR;
}
static int pop3driver_get_message_by_uid(mailsession * session,
const char * uid, mailmessage ** result)
{
mailpop3 * pop3;
struct mailpop3_msg_info * msg_info;
int found;
unsigned int i;
if (uid == NULL)
return MAIL_ERROR_INVAL;
pop3 = get_pop3_session(session);
found = 0;
for(i = 0 ; i < carray_count(pop3->pop3_msg_tab) ; i++) {
msg_info = carray_get(pop3->pop3_msg_tab, i);
if (msg_info == NULL)
continue;
if (msg_info->msg_deleted)
continue;
if (strcmp(msg_info->msg_uidl, uid) == 0) {
found = 1;
break;
}
}
if (!found)
return MAIL_ERROR_MSG_NOT_FOUND;
return pop3driver_get_message(session, msg_info->msg_index, result);
}
static int pop3driver_login_sasl(mailsession * session,
const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm)
{
int r;
r = mailpop3_auth(get_pop3_session(session),
auth_type, server_fqdn, local_ip_port, remote_ip_port,
login, auth_name, password, realm);
return pop3driver_pop3_error_to_mail_error(r);
}