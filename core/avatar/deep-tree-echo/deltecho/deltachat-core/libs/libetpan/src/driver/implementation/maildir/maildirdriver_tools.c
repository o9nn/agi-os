#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailmessage.h"
#include "maildirdriver_tools.h"
#include "maildir.h"
#include "generic_cache.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
int maildirdriver_maildir_error_to_mail_error(int error)
{
switch (error) {
case MAILDIR_NO_ERROR:
return MAIL_NO_ERROR;
case MAILDIR_ERROR_CREATE:
return MAIL_ERROR_FILE;
case MAILDIR_ERROR_DIRECTORY:
return MAIL_ERROR_FILE;
case MAILDIR_ERROR_MEMORY:
return MAIL_ERROR_MEMORY;
case MAILDIR_ERROR_FILE:
return MAIL_ERROR_FILE;
case MAILDIR_ERROR_FOLDER:
return MAIL_ERROR_FOLDER;
case MAILDIR_ERROR_NOT_FOUND:
return MAIL_ERROR_MSG_NOT_FOUND;
default:
return MAIL_ERROR_INVAL;
}
}
uint32_t maildirdriver_maildir_flags_to_flags(uint32_t md_flags)
{
uint32_t flags;
flags = 0;
if ((md_flags & MAILDIR_FLAG_NEW) != 0)
flags |= MAIL_FLAG_NEW;
if ((md_flags & MAILDIR_FLAG_SEEN) != 0)
flags |= MAIL_FLAG_SEEN;
if ((md_flags & MAILDIR_FLAG_REPLIED) != 0)
flags |= MAIL_FLAG_ANSWERED;
if ((md_flags & MAILDIR_FLAG_FLAGGED) != 0)
flags |= MAIL_FLAG_FLAGGED;
if ((md_flags & MAILDIR_FLAG_TRASHED) != 0)
flags |= MAIL_FLAG_DELETED;
return flags;
}
uint32_t maildirdriver_flags_to_maildir_flags(uint32_t flags)
{
uint32_t md_flags;
md_flags = 0;
if ((flags & MAIL_FLAG_NEW) != 0)
md_flags |= MAILDIR_FLAG_NEW;
if ((flags & MAIL_FLAG_SEEN) != 0)
md_flags |= MAILDIR_FLAG_SEEN;
if ((flags & MAIL_FLAG_ANSWERED) != 0)
md_flags |= MAILDIR_FLAG_REPLIED;
if ((flags & MAIL_FLAG_FLAGGED) != 0)
md_flags |= MAILDIR_FLAG_FLAGGED;
if ((flags & MAIL_FLAG_DELETED) != 0)
md_flags |= MAILDIR_FLAG_TRASHED;
return md_flags;
}
int maildir_get_messages_list(mailsession * session, struct maildir * md,
mailmessage_driver * message_driver,
struct mailmessage_list ** result)
{
unsigned int i;
struct mailmessage_list * env_list;
int r;
carray * tab;
int res;
tab = carray_new(128);
if (tab == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
for(i = 0 ; i < carray_count(md->mdir_msg_list) ; i++) {
struct maildir_msg * md_msg;
mailmessage * msg;
char * filename;
struct stat stat_info;
md_msg = carray_get(md->mdir_msg_list, i);
filename = maildir_message_get(md, md_msg->msg_uid);
r = stat(filename, &stat_info);
free(filename);
if (r < 0)
continue;
msg = mailmessage_new();
if (msg == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_list;
}
r = mailmessage_init(msg, session, message_driver,
i + 1, stat_info.st_size);
if (r != MAIL_NO_ERROR) {
mailmessage_free(msg);
res = r;
goto free_list;
}
msg->msg_uid = strdup(md_msg->msg_uid);
if (msg->msg_uid == NULL) {
mailmessage_free(msg);
res = MAIL_ERROR_MEMORY;
goto free_list;
}
r = carray_add(tab, msg, NULL);
if (r < 0) {
mailmessage_free(msg);
res = MAIL_ERROR_MEMORY;
goto free_list;
}
}
env_list = mailmessage_list_new(tab);
if (env_list == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_list;
}
* result = env_list;
return MAIL_NO_ERROR;
free_list:
for(i = 0 ; i < carray_count(tab) ; i ++)
mailmessage_free(carray_get(tab, i));
carray_free(tab);
err:
return res;
}