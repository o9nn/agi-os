#include "namespace_types.h"
#include "mailimap_types.h"
#include <stdlib.h>
LIBETPAN_EXPORT
struct mailimap_namespace_response_extension *
mailimap_namespace_response_extension_new(char * name,
clist * values)
{
struct mailimap_namespace_response_extension * ext;
ext = malloc(sizeof(* ext));
if (ext == NULL)
return NULL;
ext->ns_name = name;
ext->ns_values = values;
return ext;
}
LIBETPAN_EXPORT
void mailimap_namespace_response_extension_free(struct mailimap_namespace_response_extension * ext)
{
clistiter * cur;
for(cur = clist_begin(ext->ns_values) ; cur != NULL ; cur = clist_next(cur)) {
char * value;
value = clist_content(cur);
mailimap_string_free(value);
}
clist_free(ext->ns_values);
mailimap_string_free(ext->ns_name);
free(ext);
}
LIBETPAN_EXPORT
struct mailimap_namespace_info * mailimap_namespace_info_new(char * prefix, char delimiter,
clist * extensions)
{
struct mailimap_namespace_info * info;
info = malloc(sizeof(* info));
if (info == NULL)
return NULL;
info->ns_prefix = prefix;
info->ns_delimiter = delimiter;
info->ns_extensions = extensions;
return info;
}
LIBETPAN_EXPORT
void mailimap_namespace_info_free(struct mailimap_namespace_info * info)
{
clistiter * cur;
if (info->ns_extensions != NULL) {
for(cur = clist_begin(info->ns_extensions) ; cur != NULL ; cur = clist_next(cur)) {
struct mailimap_namespace_response_extension * ext;
ext = clist_content(cur);
mailimap_namespace_response_extension_free(ext);
}
clist_free(info->ns_extensions);
}
mailimap_string_free(info->ns_prefix);
free(info);
}
LIBETPAN_EXPORT
struct mailimap_namespace_item * mailimap_namespace_item_new(clist * data_list)
{
struct mailimap_namespace_item * item;
item = malloc(sizeof(* item));
if (item == NULL)
return NULL;
item->ns_data_list = data_list;
return item;
}
LIBETPAN_EXPORT
void mailimap_namespace_item_free(struct mailimap_namespace_item * item)
{
clistiter * cur;
for(cur = clist_begin(item->ns_data_list) ; cur != NULL ; cur = clist_next(cur)) {
struct mailimap_namespace_info * info;
info = clist_content(cur);
mailimap_namespace_info_free(info);
}
clist_free(item->ns_data_list);
free(item);
}
LIBETPAN_EXPORT
struct mailimap_namespace_data * mailimap_namespace_data_new(struct mailimap_namespace_item * personal,
struct mailimap_namespace_item * other,
struct mailimap_namespace_item * shared)
{
struct mailimap_namespace_data * ns;
ns = malloc(sizeof(* ns));
if (ns == NULL)
return NULL;
ns->ns_personal = personal;
ns->ns_other = other;
ns->ns_shared = shared;
return ns;
}
LIBETPAN_EXPORT
void mailimap_namespace_data_free(struct mailimap_namespace_data * ns)
{
if (ns->ns_personal != NULL) {
mailimap_namespace_item_free(ns->ns_personal);
}
if (ns->ns_other != NULL) {
mailimap_namespace_item_free(ns->ns_other);
}
if (ns->ns_shared != NULL) {
mailimap_namespace_item_free(ns->ns_shared);
}
free(ns);
}