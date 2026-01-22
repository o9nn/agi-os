#ifndef MAILIMAP_ID_TYPES_H
#define MAILIMAP_ID_TYPES_H
#include <libetpan/clist.h>
#ifdef __cplusplus
extern "C" {
#endif
struct mailimap_id_params_list {
clist * idpa_list;
};
LIBETPAN_EXPORT
struct mailimap_id_params_list * mailimap_id_params_list_new(clist * items);
LIBETPAN_EXPORT
void mailimap_id_params_list_free(struct mailimap_id_params_list * list);
struct mailimap_id_param {
char * idpa_name;
char * idpa_value;
};
LIBETPAN_EXPORT
struct mailimap_id_param * mailimap_id_param_new(char * name, char * value);
LIBETPAN_EXPORT
void mailimap_id_param_free(struct mailimap_id_param * param);
LIBETPAN_EXPORT
struct mailimap_id_params_list * mailimap_id_params_list_new_empty(void);
LIBETPAN_EXPORT
int mailimap_id_params_list_add_name_value(struct mailimap_id_params_list * list, char * name, char * value);
#ifdef __cplusplus
}
#endif
#endif