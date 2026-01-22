#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include "mhdriver.h"
#include <stdio.h>
#include <sys/types.h>
#ifdef WIN32
# include "win_etpan.h"
#else
# include <dirent.h>
# include <unistd.h>
# include <sys/mman.h>
#endif
#include <sys/stat.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include "mailmh.h"
#include "maildriver_tools.h"
#include "mhdriver_tools.h"
#include "mhdriver_message.h"
#include "mailmessage.h"
static int mhdriver_initialize(mailsession * session);
static void mhdriver_uninitialize(mailsession * session);
static int mhdriver_connect_path(mailsession * session, const char * path);
static int mhdriver_logout(mailsession * session);
static int mhdriver_build_folder_name(mailsession * session, const char * mb,
const char * name, char ** result);
static int mhdriver_create_folder(mailsession * session, const char * mb);
static int mhdriver_delete_folder(mailsession * session, const char * mb);
static int mhdriver_rename_folder(mailsession * session, const char * mb,
const char * new_name);
static int mhdriver_select_folder(mailsession * session, const char * mb);
static int mhdriver_status_folder(mailsession * session, const char * mb,
uint32_t * result_messages, uint32_t * result_recent,
uint32_t * result_unseen);
static int mhdriver_messages_number(mailsession * session, const char * mb,
uint32_t * result);
static int mhdriver_list_folders(mailsession * session, const char * mb,
struct mail_list ** result);
static int mhdriver_lsub_folders(mailsession * session, const char * mb,
struct mail_list ** result);
static int mhdriver_subscribe_folder(mailsession * session, const char * mb);
static int mhdriver_unsubscribe_folder(mailsession * session, const char * mb);
static int mhdriver_append_message(mailsession * session,
const char * message, size_t size);
static int mhdriver_append_message_flags(mailsession * session,
const char * message, size_t size, struct mail_flags * flags);
static int mhdriver_copy_message(mailsession * session,
uint32_t num, const char * mb);
static int mhdriver_remove_message(mailsession * session, uint32_t num);
static int mhdriver_move_message(mailsession * session,
uint32_t num, const char * mb);
static int mhdriver_get_messages_list(mailsession * session,
struct mailmessage_list ** result);
static int mhdriver_get_message(mailsession * session,
uint32_t num, mailmessage ** result);
static int mhdriver_get_message_by_uid(mailsession * session,
const char * uid,
mailmessage ** result);
static mailsession_driver local_mh_session_driver = {
"mh",
mhdriver_initialize,
mhdriver_uninitialize,
NULL,
NULL,
mhdriver_connect_path,
NULL,
NULL,
mhdriver_logout,
NULL,
mhdriver_build_folder_name,
mhdriver_create_folder,
mhdriver_delete_folder,
mhdriver_rename_folder,
NULL,
NULL,
mhdriver_select_folder,
NULL,
mhdriver_status_folder,
mhdriver_messages_number,
mhdriver_messages_number,
mhdriver_messages_number,
mhdriver_list_folders,
mhdriver_lsub_folders,
mhdriver_subscribe_folder,
mhdriver_unsubscribe_folder,
mhdriver_append_message,
mhdriver_append_message_flags,
mhdriver_copy_message,
mhdriver_move_message,
mhdriver_get_message,
mhdriver_get_message_by_uid,
mhdriver_get_messages_list,
maildriver_generic_get_envelopes_list,
mhdriver_remove_message,
#if 0
maildriver_generic_search_messages,
#endif
NULL
};
mailsession_driver * mh_session_driver = &local_mh_session_driver;
static inline struct mh_session_state_data * get_data(mailsession * session)
{
return session->sess_data;
}
static inline struct mailmh * get_mh_session(mailsession * session)
{
return get_data(session)->mh_session;
}
static inline struct mailmh_folder * get_mh_cur_folder(mailsession * session)
{
return get_data(session)->mh_cur_folder;
}
static int add_to_list(mailsession * session, const char * mb)
{
char * new_mb;
struct mh_session_state_data * data;
int r;
data = get_data(session);
new_mb = strdup(mb);
if (new_mb == NULL)
return -1;
r = clist_append(data->mh_subscribed_list, new_mb);
if (r < 0) {
free(new_mb);
return -1;
}
return 0;
}
static int remove_from_list(mailsession * session, const char * mb)
{
clistiter * cur;
struct mh_session_state_data * data;
data = get_data(session);
for(cur = clist_begin(data->mh_subscribed_list) ;
cur != NULL ; cur = clist_next(cur)) {
char * cur_name;
cur_name = clist_content(cur);
if (strcmp(cur_name, mb) == 0) {
clist_delete(data->mh_subscribed_list, cur);
free(cur_name);
return 0;
}
}
return -1;
}
static int mhdriver_initialize(mailsession * session)
{
struct mh_session_state_data * data;
data = malloc(sizeof(* data));
if (data == NULL)
goto err;
data->mh_session = NULL;
data->mh_cur_folder = NULL;
data->mh_subscribed_list = clist_new();
if (data->mh_subscribed_list == NULL)
goto free;
session->sess_data = data;
return MAIL_NO_ERROR;
free:
free(data);
err:
return MAIL_ERROR_MEMORY;
}
static void mhdriver_uninitialize(mailsession * session)
{
struct mh_session_state_data * data;
data = get_data(session);
if (data->mh_session != NULL)
mailmh_free(data->mh_session);
clist_foreach(data->mh_subscribed_list, (clist_func) free, NULL);
clist_free(data->mh_subscribed_list);
free(data);
session->sess_data = NULL;
}
static int mhdriver_connect_path(mailsession * session, const char * path)
{
struct mailmh * mh;
if (get_mh_session(session) != NULL)
return MAIL_ERROR_BAD_STATE;
mh = mailmh_new(path);
if (mh == NULL)
return MAIL_ERROR_MEMORY;
get_data(session)->mh_session = mh;
return MAIL_NO_ERROR;
}
static int mhdriver_logout(mailsession * session)
{
struct mailmh * mh;
mh = get_mh_session(session);
if (mh == NULL)
return MAIL_ERROR_BAD_STATE;
mailmh_free(mh);
get_data(session)->mh_session = NULL;
return MAIL_NO_ERROR;
}
static int mhdriver_build_folder_name(mailsession * session, const char * mb,
const char * name, char ** result)
{
char * folder_name;
folder_name = malloc(strlen(mb) + 2 + strlen(name));
if (folder_name == NULL)
return MAIL_ERROR_MEMORY;
strcpy(folder_name, mb);
strcat(folder_name, "/");
strcat(folder_name, name);
* result = folder_name;
return MAIL_NO_ERROR;
}
static int get_parent(mailsession * session, const char * mb,
struct mailmh_folder ** result_folder,
const char ** result_name)
{
const char * name;
size_t length;
size_t i;
char * parent_name;
struct mailmh_folder * parent;
struct mailmh * mh;
mh = get_mh_session(session);
if (mh == NULL)
return MAIL_ERROR_BAD_STATE;
length = strlen(mb);
if (length > 0) {
i = length - 1;
while (1) {
if (i == 0)
return MAIL_ERROR_INVAL;
if (mb[i] == '/')
break;
i --;
}
name = mb + i + 1;
}
else {
return MAIL_ERROR_INVAL;
}
parent_name = malloc(i + 1);
if (parent_name == NULL)
return MAIL_ERROR_MEMORY;
strncpy(parent_name, mb, i);
parent_name[i] = '\0';
parent = mailmh_folder_find(mh->mh_main, parent_name);
free(parent_name);
if (parent == NULL)
return MAIL_ERROR_FOLDER_NOT_FOUND;
* result_folder = parent;
* result_name = name;
return MAIL_NO_ERROR;
}
static int mhdriver_create_folder(mailsession * session, const char * mb)
{
int r;
struct mailmh_folder * parent;
const char * name;
r = get_parent(session, mb, &parent, &name);
if (r != MAIL_NO_ERROR)
return r;
r = mailmh_folder_add_subfolder(parent, name);
return mhdriver_mh_error_to_mail_error(r);
}
static int mhdriver_delete_folder(mailsession * session, const char * mb)
{
int r;
struct mailmh_folder * folder;
struct mailmh * mh;
mh = get_mh_session(session);
if (mh == NULL)
return MAIL_ERROR_BAD_STATE;
folder = mailmh_folder_find(mh->mh_main, mb);
if (folder == NULL)
return MAIL_ERROR_FOLDER_NOT_FOUND;
if (get_mh_cur_folder(session) == folder)
get_data(session)->mh_cur_folder = NULL;
r = mailmh_folder_remove_subfolder(folder);
return mhdriver_mh_error_to_mail_error(r);
}
static int mhdriver_rename_folder(mailsession * session, const char * mb,
const char * new_name)
{
struct mailmh_folder * src_folder;
struct mailmh_folder * dst_folder;
const char * name;
struct mailmh * mh;
int r;
r = get_parent(session, new_name, &dst_folder, &name);
if (r != MAIL_NO_ERROR)
return r;
mh = get_mh_session(session);
if (mh == NULL)
return MAIL_ERROR_BAD_STATE;
src_folder = mailmh_folder_find(mh->mh_main, mb);
if (src_folder == NULL)
return MAIL_ERROR_FOLDER_NOT_FOUND;
if (get_mh_cur_folder(session) == src_folder)
get_data(session)->mh_cur_folder = NULL;
r = mailmh_folder_rename_subfolder(src_folder, dst_folder, name);
return mhdriver_mh_error_to_mail_error(r);
}
static int mhdriver_select_folder(mailsession * session, const char * mb)
{
struct mailmh_folder * folder;
struct mailmh * mh;
int r;
mh = get_mh_session(session);
if (mh == NULL)
return MAIL_ERROR_BAD_STATE;
r = mailmh_folder_update(mh->mh_main);
folder = mailmh_folder_find(mh->mh_main, mb);
if (folder == NULL)
return MAIL_ERROR_FOLDER_NOT_FOUND;
get_data(session)->mh_cur_folder = folder;
r = mailmh_folder_update(folder);
return mhdriver_mh_error_to_mail_error(r);
}
static int mhdriver_status_folder(mailsession * session, const char * mb,
uint32_t * result_messages, uint32_t * result_recent,
uint32_t * result_unseen)
{
uint32_t count;
int r;
r = mhdriver_messages_number(session, mb, &count);
if (r != MAIL_NO_ERROR)
return r;
* result_messages = count;
* result_recent = count;
* result_unseen = count;
return MAIL_NO_ERROR;
}
static int mhdriver_messages_number(mailsession * session, const char * mb,
uint32_t * result)
{
struct mailmh_folder * folder;
uint32_t count;
struct mailmh * mh;
unsigned int i;
mh = get_mh_session(session);
if (mh == NULL)
return MAIL_ERROR_BAD_STATE;
if (mb != NULL) {
folder = mailmh_folder_find(mh->mh_main, mb);
if (folder == NULL)
return MAIL_ERROR_FOLDER_NOT_FOUND;
}
else {
folder = get_mh_cur_folder(session);
if (folder == NULL)
return MAIL_ERROR_BAD_STATE;
}
mailmh_folder_update(folder);
count = 0;
for (i = 0 ; i < carray_count(folder->fl_msgs_tab) ; i ++) {
struct mailmh_msg_info * msg_info;
msg_info = carray_get(folder->fl_msgs_tab, i);
if (msg_info != NULL)
count ++;
}
* result = count;
return MAIL_NO_ERROR;
}
static int get_list_folders(struct mailmh_folder * folder, clist ** result)
{
unsigned int i;
clist * list;
char * new_filename;
int res;
int r;
list = * result;
new_filename = strdup(folder->fl_filename);
if (new_filename == NULL) {
res = MAIL_ERROR_MEMORY;
goto free;
}
r = mailmh_folder_update(folder);
switch (r) {
case MAILMH_NO_ERROR:
break;
default:
res = mhdriver_mh_error_to_mail_error(r);
free(new_filename);
goto free;
}
r = clist_append(list, new_filename);
if (r < 0) {
free(new_filename);
res = MAIL_ERROR_MEMORY;
goto free;
}
if (folder->fl_subfolders_tab != NULL) {
for(i = 0 ; i < carray_count(folder->fl_subfolders_tab) ; i++) {
struct mailmh_folder * subfolder;
subfolder = carray_get(folder->fl_subfolders_tab, i);
r = get_list_folders(subfolder, &list);
if (r != MAIL_NO_ERROR) {
free(new_filename);
res = MAIL_ERROR_MEMORY;
goto free;
}
}
}
* result = list;
free(new_filename);
return MAIL_NO_ERROR;
free:
clist_foreach(list, (clist_func) free, NULL);
clist_free(list);
return res;
}
static int mhdriver_list_folders(mailsession * session, const char * mb,
struct mail_list ** result)
{
clist * list;
int r;
struct mailmh * mh;
struct mail_list * ml;
mh = get_mh_session(session);
if (mh == NULL)
return MAIL_ERROR_BAD_STATE;
list = clist_new();
if (list == NULL)
return MAIL_ERROR_MEMORY;
r = get_list_folders(mh->mh_main, &list);
if (r != MAIL_NO_ERROR)
return r;
ml = mail_list_new(list);
if (ml == NULL)
goto free;
* result = ml;
return MAIL_NO_ERROR;
free:
clist_foreach(list, (clist_func) free, NULL);
clist_free(list);
return MAIL_ERROR_MEMORY;
}
static int mhdriver_lsub_folders(mailsession * session, const char * mb,
struct mail_list ** result)
{
clist * subscribed;
clist * lsub_result;
clistiter * cur;
struct mail_list * lsub;
size_t length;
int r;
length = strlen(mb);
subscribed = get_data(session)->mh_subscribed_list;
lsub_result = clist_new();
if (lsub_result == NULL)
return MAIL_ERROR_MEMORY;
for(cur = clist_begin(subscribed) ; cur != NULL ;
cur = clist_next(cur)) {
char * cur_mb;
char * new_mb;
cur_mb = clist_content(cur);
if (strncmp(mb, cur_mb, length) == 0) {
new_mb = strdup(cur_mb);
if (new_mb == NULL)
goto free_list;
r = clist_append(lsub_result, new_mb);
if (r < 0) {
free(new_mb);
goto free_list;
}
}
}
lsub = mail_list_new(lsub_result);
if (lsub == NULL)
goto free_list;
* result = lsub;
return MAIL_NO_ERROR;
free_list:
clist_foreach(lsub_result, (clist_func) free, NULL);
clist_free(lsub_result);
return MAIL_ERROR_MEMORY;
}
static int mhdriver_subscribe_folder(mailsession * session, const char * mb)
{
int r;
r = add_to_list(session, mb);
if (r < 0)
return MAIL_ERROR_SUBSCRIBE;
return MAIL_NO_ERROR;
}
static int mhdriver_unsubscribe_folder(mailsession * session, const char * mb)
{
int r;
r = remove_from_list(session, mb);
if (r < 0)
return MAIL_ERROR_UNSUBSCRIBE;
return MAIL_NO_ERROR;
}
static int mhdriver_append_message(mailsession * session,
const char * message, size_t size)
{
int r;
struct mailmh_folder * folder;
folder = get_mh_cur_folder(session);
if (folder == NULL)
return MAIL_ERROR_BAD_STATE;
r = mailmh_folder_add_message(folder, message, size);
switch (r) {
case MAILMH_ERROR_FILE:
return MAIL_ERROR_DISKSPACE;
default:
return mhdriver_mh_error_to_mail_error(r);
}
}
static int mhdriver_append_message_flags(mailsession * session,
const char * message, size_t size, struct mail_flags * flags)
{
return mhdriver_append_message(session, message, size);
}
static int mhdriver_copy_message(mailsession * session,
uint32_t num, const char * mb)
{
int fd;
int r;
struct mailmh_folder * folder;
struct mailmh * mh;
int res;
mh = get_mh_session(session);
if (mh == NULL) {
res = MAIL_ERROR_BAD_STATE;
goto err;
}
folder = get_mh_cur_folder(session);
if (folder == NULL) {
res = MAIL_ERROR_BAD_STATE;
goto err;
}
r = mailmh_folder_get_message_fd(folder, num, O_RDONLY, &fd);
if (r != MAIL_NO_ERROR) {
res = r;
goto err;
}
folder = mailmh_folder_find(mh->mh_main, mb);
if (folder == NULL) {
res = MAIL_ERROR_FOLDER_NOT_FOUND;
goto close;
}
r = mailmh_folder_add_message_file(folder, fd);
if (r != MAIL_NO_ERROR) {
res = MAIL_ERROR_COPY;
goto close;
}
close(fd);
return MAIL_NO_ERROR;
close:
close(fd);
err:
return res;
}
static int mhdriver_remove_message(mailsession * session, uint32_t num)
{
int r;
struct mailmh_folder * folder;
folder = get_mh_cur_folder(session);
if (folder == NULL)
return MAIL_ERROR_DELETE;
r = mailmh_folder_remove_message(folder, num);
return mhdriver_mh_error_to_mail_error(r);
}
static int mhdriver_move_message(mailsession * session,
uint32_t num, const char * mb)
{
int r;
struct mailmh_folder * src_folder;
struct mailmh_folder * dest_folder;
struct mailmh * mh;
mh = get_mh_session(session);
if (mh == NULL)
return MAIL_ERROR_BAD_STATE;
src_folder = get_mh_cur_folder(session);
if (src_folder == NULL)
return MAIL_ERROR_BAD_STATE;
dest_folder = mailmh_folder_find(mh->mh_main, mb);
if (dest_folder == NULL)
return MAIL_ERROR_FOLDER_NOT_FOUND;
r = mailmh_folder_move_message(dest_folder, src_folder, num);
return mhdriver_mh_error_to_mail_error(r);
}
static int mhdriver_get_messages_list(mailsession * session,
struct mailmessage_list ** result)
{
struct mailmh_folder * folder;
int res;
folder = get_mh_cur_folder(session);
if (folder == NULL) {
res = MAIL_ERROR_BAD_STATE;
goto err;
}
mailmh_folder_update(folder);
return mh_get_messages_list(folder, session, mh_message_driver, result);
err:
return res;
}
static int mhdriver_get_message(mailsession * session,
uint32_t num, mailmessage ** result)
{
mailmessage * msg_info;
int r;
msg_info = mailmessage_new();
if (msg_info == NULL)
return MAIL_ERROR_MEMORY;
r = mailmessage_init(msg_info, session, mh_message_driver, num, 0);
if (r != MAIL_NO_ERROR) {
mailmessage_free(msg_info);
return r;
}
* result = msg_info;
return MAIL_NO_ERROR;
}
static int mhdriver_get_message_by_uid(mailsession * session,
const char * uid,
mailmessage ** result)
{
uint32_t indx;
char *p;
struct mailmh_msg_info * mh_msg_info;
struct mh_session_state_data * mh_data;
chashdatum key;
chashdatum data;
int r;
time_t mtime;
char * mtime_p;
if (uid == NULL)
return MAIL_ERROR_INVAL;
indx = (uint32_t) strtoul(uid, &p, 10);
if (p == uid || * p != '-')
return MAIL_ERROR_INVAL;
mh_data = session->sess_data;
key.data = &indx;
key.len = sizeof(indx);
r = chash_get(mh_data->mh_cur_folder->fl_msgs_hash, &key, &data);
if (r < 0)
return MAIL_ERROR_MSG_NOT_FOUND;
mh_msg_info = data.data;
mtime_p = p + 1;
mtime = strtoul(mtime_p, &p, 10);
if ((* p == '-') && (mtime == mh_msg_info->msg_mtime)) {
size_t size;
char *size_p;
size_p = p + 1;
size = strtoul(size_p, &p, 10);
if ((* p == '\0') && (size == mh_msg_info->msg_size))
return mhdriver_get_message(session, indx, result);
}
else if (* p != '-') {
return MAIL_ERROR_INVAL;
}
return MAIL_ERROR_MSG_NOT_FOUND;
}