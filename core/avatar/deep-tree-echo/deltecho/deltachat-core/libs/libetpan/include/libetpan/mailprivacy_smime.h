#ifndef MAILPRIVACY_SMIME_H
#define MAILPRIVACY_SMIME_H
#include <libetpan/mailprivacy_types.h>
LIBETPAN_EXPORT
int mailprivacy_smime_init(struct mailprivacy * privacy);
LIBETPAN_EXPORT
void mailprivacy_smime_done(struct mailprivacy * privacy);
LIBETPAN_EXPORT
void mailprivacy_smime_set_cert_dir(struct mailprivacy * privacy,
char * directory);
LIBETPAN_EXPORT
void mailprivacy_smime_set_CA_dir(struct mailprivacy * privacy,
char * directory);
LIBETPAN_EXPORT
void mailprivacy_smime_set_CA_check(struct mailprivacy * privacy,
int enabled);
LIBETPAN_EXPORT
void mailprivacy_smime_set_store_cert(struct mailprivacy * privacy,
int enabled);
LIBETPAN_EXPORT
void mailprivacy_smime_set_private_keys_dir(struct mailprivacy * privacy,
char * directory);
LIBETPAN_EXPORT
clist * mailprivacy_smime_encryption_id_list(struct mailprivacy * privacy,
mailmessage * msg);
LIBETPAN_EXPORT
void mailprivacy_smime_encryption_id_list_clear(struct mailprivacy * privacy,
mailmessage * msg);
LIBETPAN_EXPORT
int mailprivacy_smime_set_encryption_id(struct mailprivacy * privacy,
char * user_id, char * passphrase);
#endif