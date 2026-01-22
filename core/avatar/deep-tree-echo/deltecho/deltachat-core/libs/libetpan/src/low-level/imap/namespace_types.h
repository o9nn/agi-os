#ifndef NAMESPACE_TYPES_H
#define NAMESPACE_TYPES_H
#include <libetpan/clist.h>
enum {
MAILIMAP_NAMESPACE_TYPE_NAMESPACE
};
struct mailimap_namespace_response_extension {
char * ns_name;
clist * ns_values;
};
LIBETPAN_EXPORT
struct mailimap_namespace_response_extension *
mailimap_namespace_response_extension_new(char * name,
clist * values);
LIBETPAN_EXPORT
void mailimap_namespace_response_extension_free(struct mailimap_namespace_response_extension * ext);
struct mailimap_namespace_info {
char * ns_prefix;
char ns_delimiter;
clist * ns_extensions;
};
LIBETPAN_EXPORT
struct mailimap_namespace_info * mailimap_namespace_info_new(char * prefix, char delimiter,
clist * extensions);
LIBETPAN_EXPORT
void mailimap_namespace_info_free(struct mailimap_namespace_info * info);
struct mailimap_namespace_item {
clist * ns_data_list;
};
LIBETPAN_EXPORT
struct mailimap_namespace_item * mailimap_namespace_item_new(clist * data_list);
LIBETPAN_EXPORT
void mailimap_namespace_item_free(struct mailimap_namespace_item * item);
struct mailimap_namespace_data {
struct mailimap_namespace_item * ns_personal;
struct mailimap_namespace_item * ns_other;
struct mailimap_namespace_item * ns_shared;
};
LIBETPAN_EXPORT
struct mailimap_namespace_data *
mailimap_namespace_data_new(struct mailimap_namespace_item * personal,
struct mailimap_namespace_item * other,
struct mailimap_namespace_item * shared);
LIBETPAN_EXPORT
void mailimap_namespace_data_free(struct mailimap_namespace_data * ns);
#endif