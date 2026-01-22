#ifndef MAILFOLDER_H
#define MAILFOLDER_H
#include "mailstorage_types.h"
LIBETPAN_EXPORT
int mailfolder_noop(struct mailfolder * folder);
LIBETPAN_EXPORT
int mailfolder_check(struct mailfolder * folder);
LIBETPAN_EXPORT
int mailfolder_expunge(struct mailfolder * folder);
LIBETPAN_EXPORT
int mailfolder_status(struct mailfolder * folder,
uint32_t * result_messages, uint32_t * result_recent,
uint32_t * result_unseen);
LIBETPAN_EXPORT
int mailfolder_append_message(struct mailfolder * folder,
char * message, size_t size);
LIBETPAN_EXPORT
int mailfolder_append_message_flags(struct mailfolder * folder,
char * message, size_t size, struct mail_flags * flags);
LIBETPAN_EXPORT
int mailfolder_get_messages_list(struct mailfolder * folder,
struct mailmessage_list ** result);
LIBETPAN_EXPORT
int mailfolder_get_envelopes_list(struct mailfolder * folder,
struct mailmessage_list * result);
LIBETPAN_EXPORT
int mailfolder_get_message(struct mailfolder * folder,
uint32_t num, mailmessage ** result);
LIBETPAN_EXPORT
int mailfolder_get_message_by_uid(struct mailfolder * folder,
const char * uid, mailmessage ** result);
#endif