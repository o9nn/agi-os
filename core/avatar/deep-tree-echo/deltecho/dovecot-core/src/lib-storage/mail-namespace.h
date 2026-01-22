#ifndef MAIL_NAMESPACE_H
#define MAIL_NAMESPACE_H
#include "mail-user.h"
struct mail_storage_callbacks;
enum mail_namespace_type {
MAIL_NAMESPACE_TYPE_PRIVATE = 0x01,
MAIL_NAMESPACE_TYPE_SHARED = 0x02,
MAIL_NAMESPACE_TYPE_PUBLIC = 0x04
#define MAIL_NAMESPACE_TYPE_MASK_ALL \
(MAIL_NAMESPACE_TYPE_PRIVATE | MAIL_NAMESPACE_TYPE_SHARED | \
MAIL_NAMESPACE_TYPE_PUBLIC)
};
enum namespace_flags {
NAMESPACE_FLAG_INBOX_USER = 0x01,
NAMESPACE_FLAG_INBOX_ANY = 0x02,
NAMESPACE_FLAG_HIDDEN = 0x04,
NAMESPACE_FLAG_LIST_PREFIX = 0x08,
NAMESPACE_FLAG_LIST_CHILDREN = 0x10,
NAMESPACE_FLAG_SUBSCRIPTIONS = 0x20,
NAMESPACE_FLAG_AUTOCREATED = 0x1000,
NAMESPACE_FLAG_USABLE = 0x2000,
NAMESPACE_FLAG_UNUSABLE = 0x4000,
NAMESPACE_FLAG_NOQUOTA = 0x8000,
NAMESPACE_FLAG_NOACL = 0x10000
};
struct mail_namespace {
struct mail_namespace *next;
int refcount;
enum mail_namespace_type type;
enum namespace_flags flags;
char *prefix;
size_t prefix_len;
struct mail_namespace *alias_for;
struct mail_namespace *alias_chain_next;
struct mail_user *user, *owner;
struct mailbox_list *list;
struct mail_storage *storage;
ARRAY(struct mail_storage *) all_storages;
const struct mail_namespace_settings *set;
const struct mail_storage_settings *mail_set;
bool destroyed:1;
};
static inline bool mail_namespace_is_removable(const struct mail_namespace *ns)
{
return ((ns->flags & NAMESPACE_FLAG_USABLE) == 0 &&
(ns->flags & NAMESPACE_FLAG_AUTOCREATED) != 0);
}
int mail_namespace_alloc(struct mail_user *user,
const struct mail_namespace_settings *ns_set,
struct mail_namespace **ns_r,
const char **error_r);
int mail_namespaces_init(struct mail_user *user, const char **error_r);
int mail_namespaces_init_location(struct mail_user *user, const char *location,
const char **error_r) ATTR_NULL(2);
struct mail_namespace *mail_namespaces_init_empty(struct mail_user *user);
void mail_namespaces_deinit(struct mail_namespace **namespaces);
int mail_namespaces_init_add(struct mail_user *user,
const struct mail_namespace_settings *ns_set,
struct mail_namespace **ns_p, const char **error_r);
int mail_namespaces_init_finish(struct mail_namespace *namespaces,
const char **error_r);
void mail_namespace_ref(struct mail_namespace *ns);
void mail_namespace_unref(struct mail_namespace **ns);
void mail_namespaces_set_storage_callbacks(struct mail_namespace *namespaces,
struct mail_storage_callbacks *callbacks,
void *context);
void mail_namespace_add_storage(struct mail_namespace *ns,
struct mail_storage *storage);
void mail_namespace_destroy(struct mail_namespace *ns);
struct mail_storage *
mail_namespace_get_default_storage(struct mail_namespace *ns);
char mail_namespace_get_sep(struct mail_namespace *ns);
char mail_namespaces_get_root_sep(struct mail_namespace *namespaces)
ATTR_PURE;
struct mail_namespace *
mail_namespace_find(struct mail_namespace *namespaces, const char *mailbox);
struct mail_namespace *
mail_namespace_find_unalias(struct mail_namespace *namespaces,
const char **mailbox);
struct mail_namespace *
mail_namespace_find_visible(struct mail_namespace *namespaces,
const char *mailbox);
struct mail_namespace *
mail_namespace_find_subscribable(struct mail_namespace *namespaces,
const char *mailbox);
struct mail_namespace *
mail_namespace_find_unsubscribable(struct mail_namespace *namespaces,
const char *mailbox);
struct mail_namespace *
mail_namespace_find_inbox(struct mail_namespace *namespaces);
struct mail_namespace *
mail_namespace_find_prefix(struct mail_namespace *namespaces,
const char *prefix);
struct mail_namespace *
mail_namespace_find_prefix_nosep(struct mail_namespace *namespaces,
const char *prefix);
void mail_namespace_finish_list_init(struct mail_namespace *ns,
struct mailbox_list *list);
bool mail_namespace_is_shared_user_root(struct mail_namespace *ns);
static inline bool
mail_namespace_is_inbox_noinferiors(struct mail_namespace *ns)
{
return (ns->flags & NAMESPACE_FLAG_INBOX_USER) != 0 &&
ns->prefix_len > 0 &&
strncmp(ns->prefix, "INBOX", ns->prefix_len-1) != 0;
}
static inline bool
mail_namespace_prefix_is_inbox(struct mail_namespace *ns)
{
const char *suffix;
return (ns->flags & NAMESPACE_FLAG_INBOX_USER) != 0 &&
(ns->prefix_len == 6) &&
(str_begins_icase(ns->prefix, "INBOX", &suffix)) &&
(suffix[0] == mail_namespace_get_sep(ns));
}
#endif