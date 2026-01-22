#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailimap.h"
#include "mailimap_extension.h"
#include "acl.h"
#include "acl_types.h"
#include "acl_parser.h"
#include "acl_sender.h"
#include <stdlib.h>
LIBETPAN_EXPORT
struct mailimap_extension_api mailimap_extension_acl = {
"ACL",
MAILIMAP_EXTENSION_ACL,
mailimap_acl_parse,
mailimap_acl_free
};
LIBETPAN_EXPORT
int mailimap_acl_setacl(mailimap * session,
const char * mailbox,
const char * identifier,
const char * mod_rights)
{
struct mailimap_response * response;
int r;
int error_code;
if (session->imap_state != MAILIMAP_STATE_AUTHENTICATED)
return MAILIMAP_ERROR_BAD_STATE;
r = mailimap_send_current_tag(session);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_setacl_send(session->imap_stream,
mailbox, identifier, mod_rights);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_crlf_send(session->imap_stream);
if (r != MAILIMAP_NO_ERROR)
return r;
if (mailstream_flush(session->imap_stream) == -1)
return MAILIMAP_ERROR_STREAM;
if (mailimap_read_line(session) == NULL)
return MAILIMAP_ERROR_STREAM;
r = mailimap_parse_response(session, &response);
if (r != MAILIMAP_NO_ERROR)
return r;
error_code = response->rsp_resp_done->rsp_data.rsp_tagged->rsp_cond_state->rsp_type;
mailimap_response_free(response);
switch (error_code) {
case MAILIMAP_RESP_COND_STATE_OK:
return MAILIMAP_NO_ERROR;
default:
return MAILIMAP_ERROR_EXTENSION;
}
}
LIBETPAN_EXPORT
int mailimap_acl_deleteacl(mailimap * session,
const char * mailbox,
const char * identifier)
{
struct mailimap_response * response;
int r;
int error_code;
if (session->imap_state != MAILIMAP_STATE_AUTHENTICATED)
return MAILIMAP_ERROR_BAD_STATE;
r = mailimap_send_current_tag(session);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_deleteacl_send(session->imap_stream,
mailbox, identifier);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_crlf_send(session->imap_stream);
if (r != MAILIMAP_NO_ERROR)
return r;
if (mailstream_flush(session->imap_stream) == -1)
return MAILIMAP_ERROR_STREAM;
if (mailimap_read_line(session) == NULL)
return MAILIMAP_ERROR_STREAM;
r = mailimap_parse_response(session, &response);
if (r != MAILIMAP_NO_ERROR)
return r;
error_code = response->rsp_resp_done->rsp_data.rsp_tagged->rsp_cond_state->rsp_type;
mailimap_response_free(response);
switch (error_code) {
case MAILIMAP_RESP_COND_STATE_OK:
return MAILIMAP_NO_ERROR;
default:
return MAILIMAP_ERROR_EXTENSION;
}
}
LIBETPAN_EXPORT
int mailimap_acl_getacl(mailimap * session,
const char * mailbox,
clist ** result)
{
struct mailimap_response * response;
struct mailimap_extension_data * ext_data;
clistiter * cur;
int r;
int error_code;
if (session->imap_state != MAILIMAP_STATE_AUTHENTICATED)
return MAILIMAP_ERROR_BAD_STATE;
r = mailimap_send_current_tag(session);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_getacl_send(session->imap_stream,
mailbox);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_crlf_send(session->imap_stream);
if (r != MAILIMAP_NO_ERROR)
return r;
if (mailstream_flush(session->imap_stream) == -1)
return MAILIMAP_ERROR_STREAM;
if (mailimap_read_line(session) == NULL)
return MAILIMAP_ERROR_STREAM;
r = mailimap_parse_response(session, &response);
if (r != MAILIMAP_NO_ERROR)
return r;
* result = clist_new();
if (* result == NULL)
return MAILIMAP_ERROR_MEMORY;
for (cur = clist_begin(session->imap_response_info->rsp_extension_list);
cur != NULL; cur = clist_next(cur)) {
ext_data = (struct mailimap_extension_data *) clist_content(cur);
if (
ext_data->ext_extension->ext_id == MAILIMAP_EXTENSION_ACL &&
ext_data->ext_type == MAILIMAP_ACL_TYPE_ACL_DATA) {
r = clist_append((* result), ext_data->ext_data);
if (r != 0)
return MAILIMAP_ERROR_MEMORY;
ext_data->ext_data = NULL;
ext_data->ext_type = -1;
}
}
clist_foreach(session->imap_response_info->rsp_extension_list,
(clist_func) mailimap_extension_data_free, NULL);
clist_free(session->imap_response_info->rsp_extension_list);
session->imap_response_info->rsp_extension_list = NULL;
error_code = response->rsp_resp_done->rsp_data.rsp_tagged->rsp_cond_state->rsp_type;
mailimap_response_free(response);
switch (error_code) {
case MAILIMAP_RESP_COND_STATE_OK:
return MAILIMAP_NO_ERROR;
default:
return MAILIMAP_ERROR_EXTENSION;
}
}
LIBETPAN_EXPORT
int mailimap_acl_listrights(mailimap * session,
const char * mailbox,
const char * identifier,
struct mailimap_acl_listrights_data ** result)
{
struct mailimap_response * response;
struct mailimap_extension_data * ext_data;
clistiter * cur;
int r;
int error_code;
if (session->imap_state != MAILIMAP_STATE_AUTHENTICATED)
return MAILIMAP_ERROR_BAD_STATE;
r = mailimap_send_current_tag(session);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_listrights_send(session->imap_stream,
mailbox, identifier);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_crlf_send(session->imap_stream);
if (r != MAILIMAP_NO_ERROR)
return r;
if (mailstream_flush(session->imap_stream) == -1)
return MAILIMAP_ERROR_STREAM;
if (mailimap_read_line(session) == NULL)
return MAILIMAP_ERROR_STREAM;
r = mailimap_parse_response(session, &response);
if (r != MAILIMAP_NO_ERROR)
return r;
* result = NULL;
for (cur = clist_begin(session->imap_response_info->rsp_extension_list);
cur != NULL; cur = clist_next(cur)) {
ext_data = (struct mailimap_extension_data *) clist_content(cur);
if (
ext_data->ext_extension->ext_id == MAILIMAP_EXTENSION_ACL &&
ext_data->ext_type == MAILIMAP_ACL_TYPE_LISTRIGHTS_DATA) {
* result = (struct mailimap_acl_listrights_data *)ext_data->ext_data;
clist_delete(session->imap_response_info->rsp_extension_list, cur);
break;
}
}
clist_foreach(session->imap_response_info->rsp_extension_list,
(clist_func) mailimap_extension_data_free, NULL);
clist_free(session->imap_response_info->rsp_extension_list);
session->imap_response_info->rsp_extension_list = NULL;
error_code = response->rsp_resp_done->rsp_data.rsp_tagged->rsp_cond_state->rsp_type;
mailimap_response_free(response);
if (* result == NULL)
return MAILIMAP_ERROR_EXTENSION;
switch (error_code) {
case MAILIMAP_RESP_COND_STATE_OK:
return MAILIMAP_NO_ERROR;
default:
return MAILIMAP_ERROR_EXTENSION;
}
}
LIBETPAN_EXPORT
int mailimap_acl_myrights(mailimap * session,
const char * mailbox,
struct mailimap_acl_myrights_data ** result)
{
struct mailimap_response * response;
struct mailimap_extension_data * ext_data;
clistiter * cur;
int r;
int error_code;
if (session->imap_state != MAILIMAP_STATE_AUTHENTICATED)
return MAILIMAP_ERROR_BAD_STATE;
r = mailimap_send_current_tag(session);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_myrights_send(session->imap_stream, mailbox);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_crlf_send(session->imap_stream);
if (r != MAILIMAP_NO_ERROR)
return r;
if (mailstream_flush(session->imap_stream) == -1)
return MAILIMAP_ERROR_STREAM;
if (mailimap_read_line(session) == NULL)
return MAILIMAP_ERROR_STREAM;
r = mailimap_parse_response(session, &response);
if (r != MAILIMAP_NO_ERROR)
return r;
* result = NULL;
for (cur = clist_begin(session->imap_response_info->rsp_extension_list);
cur != NULL; cur = clist_next(cur)) {
ext_data = (struct mailimap_extension_data *) clist_content(cur);
if (
ext_data->ext_extension->ext_id == MAILIMAP_EXTENSION_ACL &&
ext_data->ext_type == MAILIMAP_ACL_TYPE_MYRIGHTS_DATA) {
* result = (struct mailimap_acl_myrights_data *)ext_data->ext_data;
ext_data->ext_data = NULL;
clist_delete(session->imap_response_info->rsp_extension_list, cur);
mailimap_extension_data_free(ext_data);
break;
}
}
clist_foreach(session->imap_response_info->rsp_extension_list,
(clist_func) mailimap_extension_data_free, NULL);
clist_free(session->imap_response_info->rsp_extension_list);
session->imap_response_info->rsp_extension_list = NULL;
error_code = response->rsp_resp_done->rsp_data.rsp_tagged->rsp_cond_state->rsp_type;
mailimap_response_free(response);
if (* result == NULL)
return MAILIMAP_ERROR_EXTENSION;
switch (error_code) {
case MAILIMAP_RESP_COND_STATE_OK:
return MAILIMAP_NO_ERROR;
default:
return MAILIMAP_ERROR_EXTENSION;
}
}
LIBETPAN_EXPORT
int mailimap_has_acl(mailimap * session)
{
return mailimap_has_extension(session, "ACL");
}