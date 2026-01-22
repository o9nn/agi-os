#ifndef MAILBOX_LIST_H
#define MAILBOX_LIST_H
#include "mail-error.h"
#ifdef PATH_MAX
#  define MAILBOX_LIST_NAME_MAX_LENGTH PATH_MAX
#else
#  define MAILBOX_LIST_NAME_MAX_LENGTH 4096
#endif
struct fs;
struct mail_namespace;
struct mail_storage;
struct mailbox_list;
enum mailbox_list_properties {
MAILBOX_LIST_PROP_NO_MAILDIR_NAME	= 0x01,
MAILBOX_LIST_PROP_NO_ALT_DIR		= 0x02,
MAILBOX_LIST_PROP_NO_NOSELECT		= 0x04,
MAILBOX_LIST_PROP_NO_ROOT		= 0x08,
MAILBOX_LIST_PROP_AUTOCREATE_DIRS	= 0x10,
MAILBOX_LIST_PROP_NO_LIST_INDEX		= 0x20,
MAILBOX_LIST_PROP_NO_INTERNAL_NAMES	= 0x40,
};
enum mailbox_list_flags {
MAILBOX_LIST_FLAG_MAILBOX_FILES		= 0x01,
MAILBOX_LIST_FLAG_SECONDARY		= 0x02,
MAILBOX_LIST_FLAG_NO_MAIL_FILES		= 0x04,
MAILBOX_LIST_FLAG_NO_DELETES		= 0x08
};
enum mailbox_info_flags {
MAILBOX_NOSELECT		= 0x001,
MAILBOX_NONEXISTENT		= 0x002,
MAILBOX_CHILDREN		= 0x004,
MAILBOX_NOCHILDREN		= 0x008,
MAILBOX_NOINFERIORS		= 0x010,
MAILBOX_MARKED			= 0x020,
MAILBOX_UNMARKED		= 0x040,
MAILBOX_SUBSCRIBED		= 0x080,
MAILBOX_CHILD_SUBSCRIBED	= 0x100,
MAILBOX_CHILD_SPECIALUSE	= 0x200,
MAILBOX_SPECIALUSE_ALL		= 0x00010000,
MAILBOX_SPECIALUSE_ARCHIVE	= 0x00020000,
MAILBOX_SPECIALUSE_DRAFTS	= 0x00040000,
MAILBOX_SPECIALUSE_FLAGGED	= 0x00080000,
MAILBOX_SPECIALUSE_JUNK		= 0x00100000,
MAILBOX_SPECIALUSE_SENT		= 0x00200000,
MAILBOX_SPECIALUSE_TRASH	= 0x00400000,
MAILBOX_SPECIALUSE_IMPORTANT	= 0x00800000,
#define MAILBOX_SPECIALUSE_MASK		  0x00ff0000
MAILBOX_SELECT			= 0x20000000,
MAILBOX_MATCHED			= 0x40000000
};
enum mailbox_list_path_type {
MAILBOX_LIST_PATH_TYPE_DIR,
MAILBOX_LIST_PATH_TYPE_ALT_DIR,
MAILBOX_LIST_PATH_TYPE_MAILBOX,
MAILBOX_LIST_PATH_TYPE_ALT_MAILBOX,
MAILBOX_LIST_PATH_TYPE_CONTROL,
MAILBOX_LIST_PATH_TYPE_INDEX,
MAILBOX_LIST_PATH_TYPE_INDEX_PRIVATE,
MAILBOX_LIST_PATH_TYPE_INDEX_CACHE,
MAILBOX_LIST_PATH_TYPE_LIST_INDEX,
MAILBOX_LIST_PATH_TYPE_COUNT
};
enum mailbox_list_file_type {
MAILBOX_LIST_FILE_TYPE_UNKNOWN = 0,
MAILBOX_LIST_FILE_TYPE_FILE,
MAILBOX_LIST_FILE_TYPE_DIR,
MAILBOX_LIST_FILE_TYPE_SYMLINK,
MAILBOX_LIST_FILE_TYPE_OTHER
};
enum mailbox_list_get_storage_flags {
MAILBOX_LIST_GET_STORAGE_FLAG_SAVEONLY = BIT(0),
};
struct mailbox_list_settings {
const char *layout;
const char *root_dir;
const char *index_dir;
const char *index_pvt_dir;
const char *index_cache_dir;
const char *control_dir;
const char *alt_dir;
const char *volatile_dir;
const char *inbox_path;
const char *subscription_fname;
const char *list_index_fname;
const char *list_index_dir;
const char *maildir_name;
const char *mailbox_dir_name;
char storage_name_escape_char;
char vname_escape_char;
bool utf8:1;
bool alt_dir_nocheck:1;
bool index_control_use_maildir_name:1;
bool iter_from_index_dir:1;
bool keep_noselect:1;
bool no_fs_validation:1;
};
struct mailbox_permissions {
uid_t file_uid;
gid_t file_gid;
mode_t file_create_mode, dir_create_mode;
gid_t file_create_gid;
const char *file_create_gid_origin;
bool gid_origin_is_mailbox_path;
bool mail_index_permissions_set;
};
void mailbox_list_register_all(void);
void mailbox_list_register(const struct mailbox_list *list);
void mailbox_list_unregister(const struct mailbox_list *list);
const struct mailbox_list *
mailbox_list_find_class(const char *driver);
int mailbox_list_create(const char *driver, struct mail_namespace *ns,
const struct mailbox_list_settings *set,
enum mailbox_list_flags flags,
struct mailbox_list **list_r, const char **error_r);
void mailbox_list_destroy(struct mailbox_list **list);
const char *
mailbox_list_get_driver_name(const struct mailbox_list *list) ATTR_PURE;
const struct mailbox_list_settings *
mailbox_list_get_settings(const struct mailbox_list *list) ATTR_PURE;
enum mailbox_list_flags
mailbox_list_get_flags(const struct mailbox_list *list) ATTR_PURE;
struct mail_namespace *
mailbox_list_get_namespace(const struct mailbox_list *list) ATTR_PURE;
struct event *
mailbox_list_get_event(const struct mailbox_list *list) ATTR_PURE;
struct mail_user *
mailbox_list_get_user(const struct mailbox_list *list) ATTR_PURE;
int mailbox_list_get_storage(struct mailbox_list **list, const char **vname,
enum mailbox_list_get_storage_flags flags,
struct mail_storage **storage_r);
void mailbox_list_get_default_storage(struct mailbox_list *list,
struct mail_storage **storage);
char mailbox_list_get_hierarchy_sep(struct mailbox_list *list);
void mailbox_list_get_permissions(struct mailbox_list *list, const char *name,
struct mailbox_permissions *permissions_r);
void mailbox_list_get_root_permissions(struct mailbox_list *list,
struct mailbox_permissions *permissions_r);
int mailbox_list_mkdir_root(struct mailbox_list *list, const char *path,
enum mailbox_list_path_type type);
int mailbox_list_try_mkdir_root(struct mailbox_list *list, const char *path,
enum mailbox_list_path_type type,
const char **error_r);
int mailbox_list_mkdir_missing_index_root(struct mailbox_list *list);
int mailbox_list_mkdir_missing_list_index_root(struct mailbox_list *list);
bool mailbox_list_is_valid_name(struct mailbox_list *list,
const char *name, const char **error_r);
const char *mailbox_list_get_storage_name(struct mailbox_list *list,
const char *vname);
const char *mailbox_list_get_vname(struct mailbox_list *list, const char *name);
int mailbox_list_get_path(struct mailbox_list *list, const char *name,
enum mailbox_list_path_type type,
const char **path_r);
bool mailbox_list_get_root_path(struct mailbox_list *list,
enum mailbox_list_path_type type,
const char **path_r);
const char *mailbox_list_get_root_forced(struct mailbox_list *list,
enum mailbox_list_path_type type);
struct mailbox_log *mailbox_list_get_changelog(struct mailbox_list *list);
void mailbox_list_set_changelog_timestamp(struct mailbox_list *list,
time_t stamp);
const char *mailbox_list_get_temp_prefix(struct mailbox_list *list);
const char *mailbox_list_get_global_temp_prefix(struct mailbox_list *list);
int mailbox_list_set_subscribed(struct mailbox_list *list,
const char *name, bool set);
int mailbox_list_delete_dir(struct mailbox_list *list, const char *name);
int mailbox_list_delete_symlink(struct mailbox_list *list, const char *name);
const char * ATTR_NOWARN_UNUSED_RESULT
mailbox_list_get_last_error(struct mailbox_list *list,
enum mail_error *error_r);
enum mail_error mailbox_list_get_last_mail_error(struct mailbox_list *list);
const char * ATTR_NOWARN_UNUSED_RESULT
mailbox_list_get_last_internal_error(struct mailbox_list *list,
enum mail_error *error_r);
void mailbox_list_last_error_push(struct mailbox_list *list);
void mailbox_list_last_error_pop(struct mailbox_list *list);
int mailbox_list_init_fs(struct mailbox_list *list, struct event *event_parent,
const char *driver,
const char *args, const char *root_dir,
struct fs **fs_r, const char **error_r);
struct mailbox_list *mailbox_list_fs_get_list(struct fs *fs);
void mailbox_list_name_unescape(const char **name, char escape_char);
void mailbox_list_name_escape(const char *name, const char *escape_chars,
string_t *dest);
#endif