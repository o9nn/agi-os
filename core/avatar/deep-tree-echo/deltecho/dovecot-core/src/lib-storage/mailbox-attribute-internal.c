#include "lib.h"
#include "mail-storage-private.h"
#include "mailbox-attribute-internal.h"
static int
mailbox_attribute_specialuse_get(struct mailbox *box,
const char *key ATTR_UNUSED,
struct mail_attribute_value *value_r)
{
const struct mailbox_settings *set = box->set;
if (set == NULL || *set->special_use == '\0')
return 0;
value_r->value = set->special_use;
return 1;
}
static struct mailbox_attribute_internal
iattr_mbox_prv_special_use = {
.type = MAIL_ATTRIBUTE_TYPE_PRIVATE,
.key = MAILBOX_ATTRIBUTE_SPECIALUSE,
.rank = MAIL_ATTRIBUTE_INTERNAL_RANK_AUTHORITY,
.flags = MAIL_ATTRIBUTE_INTERNAL_FLAG_VALIDATED,
.get = mailbox_attribute_specialuse_get
};
static int
mailbox_attribute_comment_get(struct mailbox *box,
const char *key ATTR_UNUSED,
struct mail_attribute_value *value_r)
{
const struct mailbox_settings *set = box->set;
if (set == NULL || *set->comment == '\0')
return 0;
value_r->value = set->comment;
return 1;
}
static struct mailbox_attribute_internal
iattr_mbox_prv_comment = {
.type = MAIL_ATTRIBUTE_TYPE_PRIVATE,
.key = MAILBOX_ATTRIBUTE_COMMENT,
.rank = MAIL_ATTRIBUTE_INTERNAL_RANK_DEFAULT,
.get = mailbox_attribute_comment_get
};
static struct mailbox_attribute_internal
iattr_mbox_shd_comment = {
.type = MAIL_ATTRIBUTE_TYPE_SHARED,
.key = MAILBOX_ATTRIBUTE_COMMENT,
.rank = MAIL_ATTRIBUTE_INTERNAL_RANK_DEFAULT,
.get = mailbox_attribute_comment_get
};
static int
server_attribute_comment_get(struct mailbox *box,
const char *key ATTR_UNUSED,
struct mail_attribute_value *value_r)
{
const struct mail_storage_settings *set = box->storage->set;
if (*set->mail_server_comment == '\0')
return 0;
value_r->value = set->mail_server_comment;
return 1;
}
static struct mailbox_attribute_internal
iattr_serv_shd_comment = {
.type = MAIL_ATTRIBUTE_TYPE_SHARED,
.key = MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT_SERVER
MAIL_SERVER_ATTRIBUTE_COMMENT,
.rank = MAIL_ATTRIBUTE_INTERNAL_RANK_AUTHORITY,
.get = server_attribute_comment_get
};
static int
server_attribute_admin_get(struct mailbox *box,
const char *key ATTR_UNUSED,
struct mail_attribute_value *value_r)
{
const struct mail_storage_settings *set = box->storage->set;
if (*set->mail_server_admin == '\0')
return 0;
value_r->value = set->mail_server_admin;
return 1;
}
static struct mailbox_attribute_internal
iattr_serv_shd_admin = {
.type = MAIL_ATTRIBUTE_TYPE_SHARED,
.key = MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT_SERVER
MAIL_SERVER_ATTRIBUTE_ADMIN,
.rank = MAIL_ATTRIBUTE_INTERNAL_RANK_AUTHORITY,
.get = server_attribute_admin_get
};
void mailbox_attributes_internal_init(void)
{
mailbox_attribute_register_internal(&iattr_mbox_prv_special_use);
mailbox_attribute_register_internal(&iattr_mbox_prv_comment);
mailbox_attribute_register_internal(&iattr_mbox_shd_comment);
mailbox_attribute_register_internal(&iattr_serv_shd_comment);
mailbox_attribute_register_internal(&iattr_serv_shd_admin);
}