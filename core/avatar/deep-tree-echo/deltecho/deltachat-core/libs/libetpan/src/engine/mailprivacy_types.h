#ifndef MAIL_PRIVACY_TYPES_H
#define MAIL_PRIVACY_TYPES_H
#include <libetpan/chash.h>
#include <libetpan/carray.h>
#include <libetpan/mailmessage.h>
#include <libetpan/mailmime.h>
struct mailprivacy {
char * tmp_dir;
chash * msg_ref;
chash * mmapstr;
chash * mime_ref;
carray * protocols;
int make_alternative;
};
struct mailprivacy_encryption {
char * name;
char * description;
int (* encrypt)(struct mailprivacy *,
mailmessage *,
struct mailmime *, struct mailmime **);
};
struct mailprivacy_protocol {
char * name;
char * description;
int (* is_encrypted)(struct mailprivacy *,
mailmessage *, struct mailmime *);
int (* decrypt)(struct mailprivacy *,
mailmessage *, struct mailmime *,
struct mailmime **);
int encryption_count;
struct mailprivacy_encryption * encryption_tab;
};
#endif