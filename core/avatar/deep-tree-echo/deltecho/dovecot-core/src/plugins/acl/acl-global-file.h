#ifndef ACL_GLOBAL_FILE_H
#define ACL_GLOBAL_FILE_H
#include "acl-api.h"
struct acl_global_file *
acl_global_file_init(const char *path, unsigned int refresh_interval_secs,
struct event *event);
void acl_global_file_deinit(struct acl_global_file **file);
int acl_global_file_refresh(struct acl_global_file *file);
void acl_global_file_last_stat(struct acl_global_file *file, struct stat *st_r);
void acl_global_file_get(struct acl_global_file *file, const char *vname,
pool_t pool, ARRAY_TYPE(acl_rights) *rights_r);
bool acl_global_file_have_any(struct acl_global_file *file, const char *vname);
#endif