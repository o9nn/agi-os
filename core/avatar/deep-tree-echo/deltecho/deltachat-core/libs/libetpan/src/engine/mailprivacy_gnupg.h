#ifndef MAIL_PRIVACY_GNUPG_H
#define MAIL_PRIVACY_GNUPG_H
#include <libetpan/mailprivacy_types.h>
LIBETPAN_EXPORT
int mailprivacy_gnupg_init(struct mailprivacy * privacy);
LIBETPAN_EXPORT
void mailprivacy_gnupg_done(struct mailprivacy * privacy);
LIBETPAN_EXPORT
clist * mailprivacy_gnupg_encryption_id_list(struct mailprivacy * privacy,
mailmessage * msg);
LIBETPAN_EXPORT
void mailprivacy_gnupg_encryption_id_list_clear(struct mailprivacy * privacy,
mailmessage * msg);
LIBETPAN_EXPORT
int mailprivacy_gnupg_set_encryption_id(struct mailprivacy * privacy,
char * user_id, char * passphrase);
#endif