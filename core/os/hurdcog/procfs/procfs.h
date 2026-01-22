#include <hurd/hurd_types.h>
#include <hurd/netfs.h>
struct procfs_node_ops
{
error_t (*get_contents) (void *hook, char **contents, ssize_t *contents_len);
void (*cleanup_contents) (void *hook, char *contents, ssize_t contents_len);
error_t (*lookup) (void *hook, const char *name, struct node **np);
void (*cleanup) (void *hook);
error_t (*get_translator) (void *hook, char **argz, mach_msg_type_number_t *argz_len);
};
void procfs_cleanup_contents_with_free (void *, char *, ssize_t);
void procfs_cleanup_contents_with_vm_deallocate (void *, char *, ssize_t);
struct node *procfs_make_node (const struct procfs_node_ops *ops, void *hook);
void procfs_node_chown (struct node *np, uid_t owner);
void procfs_node_chmod (struct node *np, mode_t mode);
void procfs_node_chtype (struct node *np, mode_t type);
ino64_t procfs_make_ino (struct node *np, const char *filename);
void procfs_refresh (struct node *np);
error_t procfs_get_contents (struct node *np, char **data, ssize_t *data_len);
error_t procfs_lookup (struct node *np, const char *name, struct node **npp);
void procfs_cleanup (struct node *np);
error_t procfs_get_translator (struct node *np, char **argz, mach_msg_type_number_t *argz_len);