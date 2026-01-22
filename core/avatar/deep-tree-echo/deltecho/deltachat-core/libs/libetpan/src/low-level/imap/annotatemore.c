#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailimap.h"
#include "mailimap_extension.h"
#include "annotatemore.h"
#include "annotatemore_types.h"
#include "annotatemore_parser.h"
#include "annotatemore_sender.h"
#include <stdlib.h>
LIBETPAN_EXPORT
struct mailimap_extension_api mailimap_extension_annotatemore = {
"ANNOTATEMORE",
MAILIMAP_EXTENSION_ANNOTATEMORE,
mailimap_annotatemore_parse,
mailimap_annotatemore_free
};
LIBETPAN_EXPORT
int mailimap_annotatemore_getannotation(mailimap * session,
const char * list_mb,
struct mailimap_annotatemore_entry_match_list * entries,
struct mailimap_annotatemore_attrib_match_list * attribs,
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
r = mailimap_annotatemore_getannotation_send(session->imap_stream,
list_mb, entries, attribs);
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
ext_data->ext_extension->ext_id == MAILIMAP_EXTENSION_ANNOTATEMORE &&
ext_data->ext_type == MAILIMAP_ANNOTATEMORE_TYPE_ANNOTATE_DATA) {
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
int mailimap_annotatemore_setannotation(mailimap * session,
const char * list_mb,
struct mailimap_annotatemore_entry_att_list * en_att,
int * result)
{
struct mailimap_response * response;
int r;
int error_code;
clistiter * cur;
struct mailimap_extension_data * ext_data;
if (session->imap_state != MAILIMAP_STATE_AUTHENTICATED)
return MAILIMAP_ERROR_BAD_STATE;
r = mailimap_send_current_tag(session);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_annotatemore_setannotation_send(session->imap_stream,
list_mb, en_att);
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
break;
case MAILIMAP_RESP_COND_STATE_NO:
* result = MAILIMAP_ANNOTATEMORE_RESP_TEXT_CODE_UNSPECIFIED;
if (session->imap_response_info->rsp_extension_list != NULL) {
for (cur = clist_begin(session->imap_response_info->rsp_extension_list);
cur != NULL; cur = clist_next(cur)) {
ext_data = clist_content(cur);
if ((ext_data->ext_extension->ext_id ==
MAILIMAP_EXTENSION_ANNOTATEMORE) &&
(ext_data->ext_type ==
MAILIMAP_ANNOTATEMORE_TYPE_RESP_TEXT_CODE))
{
* result = * ((int *)ext_data->ext_data);
}
}
}
return MAILIMAP_ERROR_EXTENSION;
break;
default:
* result = MAILIMAP_ANNOTATEMORE_RESP_TEXT_CODE_UNSPECIFIED;
return MAILIMAP_ERROR_EXTENSION;
break;
}
}
LIBETPAN_EXPORT
int mailimap_has_annotatemore(mailimap * session)
{
return mailimap_has_extension(session, "ANNOTATEMORE");
}