struct procfs_dir_entry_ops
{
struct node *(*make_node)(void *dir_hook, const void *entry_hook);
int (*exists)(void *dir_hook, const void *entry_hook);
};
struct procfs_dir_entry
{
const char *name;
const void *hook;
struct procfs_dir_entry_ops ops;
};
struct procfs_dir_ops
{
const struct procfs_dir_entry *entries;
void (*cleanup)(void *dir_hook);
struct procfs_dir_entry_ops entry_ops;
};
struct node *
procfs_dir_make_node (const struct procfs_dir_ops *ops, void *dir_hook);