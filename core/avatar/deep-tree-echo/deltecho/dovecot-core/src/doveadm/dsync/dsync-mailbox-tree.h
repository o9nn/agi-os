#ifndef DSYNC_MAILBOX_TREE_H
#define DSYNC_MAILBOX_TREE_H
#include "guid.h"
#include "mail-error.h"
struct mail_namespace;
struct dsync_brain;
enum dsync_mailbox_trees_sync_type {
DSYNC_MAILBOX_TREES_SYNC_TYPE_TWOWAY,
DSYNC_MAILBOX_TREES_SYNC_TYPE_PRESERVE_LOCAL,
DSYNC_MAILBOX_TREES_SYNC_TYPE_PRESERVE_REMOTE
};
enum dsync_mailbox_trees_sync_flags {
DSYNC_MAILBOX_TREES_SYNC_FLAG_NO_RENAMES	= 0x04
};
enum dsync_mailbox_node_existence {
DSYNC_MAILBOX_NODE_NONEXISTENT = 0,
DSYNC_MAILBOX_NODE_EXISTS,
DSYNC_MAILBOX_NODE_DELETED
};
struct dsync_mailbox_node {
struct dsync_mailbox_node *parent, *next, *first_child;
struct mail_namespace *ns;
const char *name;
guid_128_t mailbox_guid;
uint32_t uid_validity, uid_next;
enum dsync_mailbox_node_existence existence;
time_t last_renamed_or_created;
time_t last_subscription_change;
bool subscribed:1;
bool sync_delayed_guid_change:1;
bool sync_temporary_name:1;
};
ARRAY_DEFINE_TYPE(dsync_mailbox_node, struct dsync_mailbox_node *);
#define dsync_mailbox_node_guids_equal(node1, node2) \
(memcmp((node1)->mailbox_guid, (node2)->mailbox_guid, \
sizeof(guid_128_t)) == 0)
#define dsync_mailbox_node_is_dir(node) \
guid_128_is_empty((node)->mailbox_guid)
enum dsync_mailbox_delete_type {
DSYNC_MAILBOX_DELETE_TYPE_MAILBOX = 1,
DSYNC_MAILBOX_DELETE_TYPE_DIR,
DSYNC_MAILBOX_DELETE_TYPE_UNSUBSCRIBE,
};
struct dsync_mailbox_delete {
enum dsync_mailbox_delete_type type;
guid_128_t guid;
time_t timestamp;
};
enum dsync_mailbox_tree_sync_type {
DSYNC_MAILBOX_TREE_SYNC_TYPE_CREATE_BOX,
DSYNC_MAILBOX_TREE_SYNC_TYPE_CREATE_DIR,
DSYNC_MAILBOX_TREE_SYNC_TYPE_DELETE_BOX,
DSYNC_MAILBOX_TREE_SYNC_TYPE_DELETE_DIR,
DSYNC_MAILBOX_TREE_SYNC_TYPE_RENAME,
DSYNC_MAILBOX_TREE_SYNC_TYPE_SUBSCRIBE,
DSYNC_MAILBOX_TREE_SYNC_TYPE_UNSUBSCRIBE
};
struct dsync_mailbox_tree_sync_change {
enum dsync_mailbox_tree_sync_type type;
struct mail_namespace *ns;
const char *full_name;
guid_128_t mailbox_guid;
uint32_t uid_validity;
const char *rename_dest_name;
};
struct dsync_mailbox_tree *
dsync_mailbox_tree_init(char sep, char escape_char, char alt_char);
void dsync_mailbox_tree_deinit(struct dsync_mailbox_tree **tree);
struct dsync_mailbox_node *
dsync_mailbox_tree_lookup(struct dsync_mailbox_tree *tree,
const char *full_name);
struct dsync_mailbox_node *
dsync_mailbox_tree_lookup_guid(struct dsync_mailbox_tree *tree,
const guid_128_t guid);
struct dsync_mailbox_node *
dsync_mailbox_tree_get(struct dsync_mailbox_tree *tree, const char *full_name);
const char *dsync_mailbox_node_get_full_name(const struct dsync_mailbox_tree *tree,
const struct dsync_mailbox_node *node);
void dsync_mailbox_node_append_full_name(string_t *str,
const struct dsync_mailbox_tree *tree,
const struct dsync_mailbox_node *node);
void dsync_mailbox_node_copy_data(struct dsync_mailbox_node *dest,
const struct dsync_mailbox_node *src);
const char *const *
dsync_mailbox_name_to_parts(const char *name, char hierarchy_sep,
char escape_char);
int dsync_mailbox_tree_fill(struct dsync_mailbox_tree *tree,
struct mail_namespace *ns, const char *box_name,
const guid_128_t box_guid,
const char *const *exclude_mailboxes,
char alt_char,
struct event *event,
enum mail_error *error_r);
const struct dsync_mailbox_delete *
dsync_mailbox_tree_get_deletes(struct dsync_mailbox_tree *tree,
unsigned int *count_r);
struct dsync_mailbox_node *
dsync_mailbox_tree_find_delete(struct dsync_mailbox_tree *tree,
const struct dsync_mailbox_delete *del);
int dsync_mailbox_tree_build_guid_hash(struct dsync_mailbox_tree *tree,
struct dsync_mailbox_node **dup_node1_r,
struct dsync_mailbox_node **dup_node2_r);
int dsync_mailbox_tree_guid_hash_add(struct dsync_mailbox_tree *tree,
struct dsync_mailbox_node *node,
struct dsync_mailbox_node **old_node_r);
void dsync_mailbox_tree_set_remote_chars(struct dsync_mailbox_tree *tree,
char remote_sep,
char remote_escape_char);
struct dsync_mailbox_tree_iter *
dsync_mailbox_tree_iter_init(struct dsync_mailbox_tree *tree);
bool dsync_mailbox_tree_iter_next(struct dsync_mailbox_tree_iter *iter,
const char **full_name_r,
struct dsync_mailbox_node **node_r);
void dsync_mailbox_tree_iter_deinit(struct dsync_mailbox_tree_iter **iter);
struct dsync_mailbox_tree_sync_ctx *
dsync_mailbox_trees_sync_init(struct dsync_mailbox_tree *local_tree,
struct dsync_mailbox_tree *remote_tree,
enum dsync_mailbox_trees_sync_type sync_type,
enum dsync_mailbox_trees_sync_flags sync_flags,
struct event *parent_event);
const struct dsync_mailbox_tree_sync_change *
dsync_mailbox_trees_sync_next(struct dsync_mailbox_tree_sync_ctx *ctx);
int dsync_mailbox_trees_sync_deinit(struct dsync_mailbox_tree_sync_ctx **ctx);
const char *dsync_mailbox_node_to_string(const struct dsync_mailbox_node *node);
const char *
dsync_mailbox_delete_type_to_string(enum dsync_mailbox_delete_type type);
#endif