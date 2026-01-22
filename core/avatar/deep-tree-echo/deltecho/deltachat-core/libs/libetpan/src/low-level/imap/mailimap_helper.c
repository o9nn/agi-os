#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailimap_helper.h"
#include <stdlib.h>
#include "mailimap.h"
LIBETPAN_EXPORT
int mailimap_fetch_rfc822(mailimap * session,
uint32_t msgid, char ** result)
{
int r;
clist * fetch_list;
struct mailimap_fetch_att * fetch_att;
struct mailimap_fetch_type * fetch_type;
struct mailimap_set * set;
struct mailimap_msg_att * msg_att;
struct mailimap_msg_att_item * item;
int res;
clistiter * cur;
fetch_att = mailimap_fetch_att_new_rfc822();
fetch_type = mailimap_fetch_type_new_fetch_att(fetch_att);
set = mailimap_set_new_single(msgid);
r = mailimap_fetch(session, set, fetch_type, &fetch_list);
mailimap_set_free(set);
mailimap_fetch_type_free(fetch_type);
if (r != MAILIMAP_NO_ERROR) {
res = r;
goto err;
}
if (clist_isempty(fetch_list)) {
res = MAILIMAP_ERROR_FETCH;
goto free;
}
msg_att = (struct mailimap_msg_att *) clist_begin(fetch_list)->data;
for(cur = clist_begin(msg_att->att_list) ; cur != NULL ; cur = clist_next(cur)) {
item = (struct mailimap_msg_att_item *) clist_content(cur);
if (item->att_type != MAILIMAP_MSG_ATT_ITEM_STATIC) {
continue;
}
if (item->att_data.att_static->att_type != MAILIMAP_MSG_ATT_RFC822) {
continue;
}
* result = item->att_data.att_static->att_data.att_rfc822.att_content;
item->att_data.att_static->att_data.att_rfc822.att_content = NULL;
mailimap_fetch_list_free(fetch_list);
return MAILIMAP_NO_ERROR;
}
res = MAILIMAP_ERROR_FETCH;
free:
mailimap_fetch_list_free(fetch_list);
err:
return res;
}
LIBETPAN_EXPORT
int mailimap_fetch_rfc822_header(mailimap * session,
uint32_t msgid, char ** result)
{
int r;
int res;
clist * fetch_list;
struct mailimap_fetch_att * fetch_att;
struct mailimap_fetch_type * fetch_type;
struct mailimap_set * set;
struct mailimap_msg_att * msg_att;
struct mailimap_msg_att_item * item;
clistiter * cur;
fetch_att = mailimap_fetch_att_new_rfc822_header();
fetch_type = mailimap_fetch_type_new_fetch_att(fetch_att);
set = mailimap_set_new_single(msgid);
r = mailimap_fetch(session, set, fetch_type, &fetch_list);
mailimap_set_free(set);
mailimap_fetch_type_free(fetch_type);
if (r != MAILIMAP_NO_ERROR) {
res = r;
goto err;
}
if (clist_isempty(fetch_list)) {
res = MAILIMAP_ERROR_FETCH;
goto free;
}
msg_att = (struct mailimap_msg_att *) clist_begin(fetch_list)->data;
for(cur = clist_begin(msg_att->att_list) ; cur != NULL ; cur = clist_next(cur)) {
item = (struct mailimap_msg_att_item *) clist_content(cur);
if (item->att_type != MAILIMAP_MSG_ATT_ITEM_STATIC) {
continue;
}
if (item->att_data.att_static->att_type != MAILIMAP_MSG_ATT_RFC822_HEADER) {
continue;
}
* result = item->att_data.att_static->att_data.att_rfc822_header.att_content;
item->att_data.att_static->att_data.att_rfc822_header.att_content = NULL;
mailimap_fetch_list_free(fetch_list);
return MAILIMAP_NO_ERROR;
}
res = MAILIMAP_ERROR_FETCH;
free:
mailimap_fetch_list_free(fetch_list);
err:
return res;
}
LIBETPAN_EXPORT
int mailimap_fetch_envelope(mailimap * session,
uint32_t first, uint32_t last,
clist ** result)
{
int r;
clist * fetch_list;
struct mailimap_fetch_att * fetch_att;
struct mailimap_fetch_type * fetch_type;
struct mailimap_set * set;
fetch_att = mailimap_fetch_att_new_envelope();
fetch_type = mailimap_fetch_type_new_fetch_att(fetch_att);
set = mailimap_set_new_interval(first, last);
r = mailimap_fetch(session, set, fetch_type, &fetch_list);
mailimap_set_free(set);
mailimap_fetch_type_free(fetch_type);
if (r != MAILIMAP_NO_ERROR)
return r;
* result = fetch_list;
return MAILIMAP_NO_ERROR;
}
LIBETPAN_EXPORT
int mailimap_append_simple(mailimap * session, const char * mailbox,
const char * content, size_t size)
{
return mailimap_append(session, mailbox, NULL, NULL, content, size);
}
LIBETPAN_EXPORT
int mailimap_login_simple(mailimap * session,
const char * userid, const char * password)
{
if (session->imap_state == MAILIMAP_STATE_NON_AUTHENTICATED)
return mailimap_login(session, userid, password);
else
return MAILIMAP_NO_ERROR;
}