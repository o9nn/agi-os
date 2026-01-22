#ifndef QUOTA_TYPES_H
#define QUOTA_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailstream.h>
#include <libetpan/clist.h>
struct mailimap_quota_quota_resource {
char * resource_name;
uint32_t usage;
uint32_t limit;
};
LIBETPAN_EXPORT
struct mailimap_quota_quota_resource *
mailimap_quota_quota_resource_new(char * resource_name,
uint32_t usage, uint32_t limit);
LIBETPAN_EXPORT
void
mailimap_quota_quota_resource_free(struct mailimap_quota_quota_resource * res);
struct mailimap_quota_quota_data {
char * quotaroot;
clist * quota_list;
};
LIBETPAN_EXPORT
struct mailimap_quota_quota_data *
mailimap_quota_quota_data_new(char * quotaroot, clist * quota_list);
LIBETPAN_EXPORT
void
mailimap_quota_quota_data_free(struct mailimap_quota_quota_data * data);
struct mailimap_quota_quotaroot_data {
char * mailbox;
clist * quotaroot_list;
};
LIBETPAN_EXPORT
struct mailimap_quota_quotaroot_data *
mailimap_quota_quotaroot_data_new(char * mailbox, clist * quotaroot_list);
LIBETPAN_EXPORT
void
mailimap_quota_quotaroot_data_free(
struct mailimap_quota_quotaroot_data * data);
enum {
MAILIMAP_QUOTA_TYPE_QUOTA_DATA,
MAILIMAP_QUOTA_TYPE_QUOTAROOT_DATA
};
struct mailimap_quota_complete_data {
struct mailimap_quota_quotaroot_data * quotaroot_data;
clist * quota_list;
};
LIBETPAN_EXPORT
struct mailimap_quota_complete_data *
mailimap_quota_complete_data_new(
struct mailimap_quota_quotaroot_data * quotaroot_data,
clist * quota_list);
LIBETPAN_EXPORT
void
mailimap_quota_complete_data_free(struct mailimap_quota_complete_data * data);
#ifdef __cplusplus
}
#endif
#endif