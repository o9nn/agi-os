#ifndef MASTER_INSTANCE_H
#define MASTER_INSTANCE_H
#define MASTER_INSTANCE_FNAME "instances"
struct master_instance_list;
struct master_instance {
time_t last_used;
const char *name;
const char *base_dir;
const char *config_path;
};
struct master_instance_list *master_instance_list_init(const char *path);
void master_instance_list_deinit(struct master_instance_list **list);
int master_instance_list_update(struct master_instance_list *list,
const char *base_dir);
int master_instance_list_set_name(struct master_instance_list *list,
const char *base_dir, const char *name);
int master_instance_list_remove(struct master_instance_list *list,
const char *base_dir);
const struct master_instance *
master_instance_list_find_by_name(struct master_instance_list *list,
const char *name);
struct master_instance_list_iter *
master_instance_list_iterate_init(struct master_instance_list *list);
const struct master_instance *
master_instance_iterate_list_next(struct master_instance_list_iter *iter);
void master_instance_iterate_list_deinit(struct master_instance_list_iter **iter);
#endif