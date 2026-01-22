#ifndef MAILDIR_H
#define MAILDIR_H
#include <libetpan/maildir_types.h>
struct maildir * maildir_new(const char * path);
void maildir_free(struct maildir * md);
int maildir_update(struct maildir * md);
int maildir_message_add_uid(struct maildir * md,
const char * message, size_t size,
char * uid, size_t max_uid_len);
int maildir_message_add(struct maildir * md,
const char * message, size_t size);
int maildir_message_add_file_uid(struct maildir * md, int fd,
char * uid, size_t max_uid_len);
int maildir_message_add_file(struct maildir * md, int fd);
char * maildir_message_get(struct maildir * md, const char * uid);
int maildir_message_remove(struct maildir * md, const char * uid);
int maildir_message_change_flags(struct maildir * md,
const char * uid, int new_flags);
#endif