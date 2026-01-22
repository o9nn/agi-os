#ifndef MAILDIRDRIVER_TOOLS_H
#define MAILDIRDRIVER_TOOLS_H
#include "maildriver_types.h"
#include "maildir.h"
int maildirdriver_maildir_error_to_mail_error(int error);
uint32_t maildirdriver_maildir_flags_to_flags(uint32_t md_flags);
uint32_t maildirdriver_flags_to_maildir_flags(uint32_t flags);
int maildir_get_messages_list(mailsession * session, struct maildir * md,
mailmessage_driver * message_driver,
struct mailmessage_list ** result);
#endif