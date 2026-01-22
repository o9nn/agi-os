#ifndef ACL_API_H
#define ACL_API_H
#include <sys/stat.h>
struct mailbox_list;
struct mail_storage;
struct mailbox;
struct acl_object;
#define MAIL_ACL_LOOKUP "lookup"
#define MAIL_ACL_READ "read"
#define MAIL_ACL_WRITE "write"
#define MAIL_ACL_WRITE_SEEN "write-seen"
#define MAIL_ACL_WRITE_DELETED "write-deleted"
#define MAIL_ACL_INSERT "insert"
#define MAIL_ACL_POST "post"
#define MAIL_ACL_EXPUNGE "expunge"
#define MAIL_ACL_CREATE "create"
#define MAIL_ACL_DELETE "delete"
#define MAIL_ACL_ADMIN "admin"
#define MAILBOX_ATTRIBUTE_PREFIX_ACL \
MAILBOX_ATTRIBUTE_PREFIX_DOVECOT_PVT"acl/"
enum acl_id_type {
ACL_ID_ANYONE,
ACL_ID_AUTHENTICATED,
ACL_ID_GROUP,
ACL_ID_OWNER,
ACL_ID_USER,
ACL_ID_GROUP_OVERRIDE,
ACL_ID_TYPE_COUNT
};
enum acl_modify_mode {
ACL_MODIFY_MODE_REMOVE = 0,
ACL_MODIFY_MODE_ADD,
ACL_MODIFY_MODE_REPLACE,
ACL_MODIFY_MODE_CLEAR
};
struct acl_rights {
enum acl_id_type id_type;
const char *identifier;
const char *const *rights;
const char *const *neg_rights;
bool global:1;
};
ARRAY_DEFINE_TYPE(acl_rights, struct acl_rights);
struct acl_rights_update {
struct acl_rights rights;
enum acl_modify_mode modify_mode;
enum acl_modify_mode neg_modify_mode;
time_t last_change;
};
struct acl_backend *
acl_backend_init(const char *data, struct mailbox_list *list,
const char *acl_username, const char *const *groups,
bool owner);
void acl_backend_deinit(struct acl_backend **backend);
const char *acl_backend_get_acl_username(struct acl_backend *backend);
bool acl_backend_user_is_authenticated(struct acl_backend *backend);
bool acl_backend_user_is_owner(struct acl_backend *backend);
bool acl_backend_user_name_equals(struct acl_backend *backend,
const char *username);
bool acl_backend_user_is_in_group(struct acl_backend *backend,
const char *group_name);
unsigned int acl_backend_lookup_right(struct acl_backend *backend,
const char *right);
bool acl_backend_rights_match_me(struct acl_backend *backend,
const struct acl_rights *rights);
struct acl_mailbox_list_context *
acl_backend_nonowner_lookups_iter_init(struct acl_backend *backend);
bool acl_backend_nonowner_lookups_iter_next(struct acl_mailbox_list_context *ctx,
const char **name_r);
int
acl_backend_nonowner_lookups_iter_deinit(struct acl_mailbox_list_context **ctx);
int acl_backend_nonowner_lookups_rebuild(struct acl_backend *backend);
struct acl_object *acl_object_init_from_name(struct acl_backend *backend,
const char *name);
struct acl_object *acl_object_init_from_parent(struct acl_backend *backend,
const char *child_name);
void acl_object_deinit(struct acl_object **aclobj);
int acl_object_have_right(struct acl_object *aclobj, unsigned int right_idx);
int acl_object_get_my_rights(struct acl_object *aclobj, pool_t pool,
const char *const **rights_r);
const char *const *acl_object_get_default_rights(struct acl_object *aclobj);
int acl_object_last_changed(struct acl_object *aclobj, time_t *last_changed_r);
int acl_object_update(struct acl_object *aclobj,
const struct acl_rights_update *update);
struct acl_object_list_iter *acl_object_list_init(struct acl_object *aclobj);
bool acl_object_list_next(struct acl_object_list_iter *iter,
struct acl_rights *rights_r);
int acl_object_list_deinit(struct acl_object_list_iter **iter);
const char *acl_rights_get_id(const struct acl_rights *right);
#endif