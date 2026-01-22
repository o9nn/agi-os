#ifndef ACL_TYPES_H
#define ACL_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailstream.h>
#include <libetpan/clist.h>
enum {
MAILIMAP_ACL_TYPE_ACL_DATA,
MAILIMAP_ACL_TYPE_LISTRIGHTS_DATA,
MAILIMAP_ACL_TYPE_MYRIGHTS_DATA
};
void mailimap_acl_identifier_free(char * identifier);
void mailimap_acl_rights_free(char * rights);
struct mailimap_acl_identifier_rights {
char * identifer;
char * rights;
};
struct mailimap_acl_identifier_rights *
mailimap_acl_identifier_rights_new(char * identifier, char * rights);
void mailimap_acl_identifier_rights_free(
struct mailimap_acl_identifier_rights * id_rights);
struct mailimap_acl_acl_data {
char * mailbox;
clist * idrights_list;
};
struct mailimap_acl_acl_data *
mailimap_acl_acl_data_new(char * mailbox, clist * idrights_list);
LIBETPAN_EXPORT
void mailimap_acl_acl_data_free(struct
mailimap_acl_acl_data * acl_data);
struct mailimap_acl_listrights_data {
char * mailbox;
char * identifier;
clist * rights_list;
};
struct mailimap_acl_listrights_data *
mailimap_acl_listrights_data_new(char * mailbox,
char * identifier, clist * rights_list);
LIBETPAN_EXPORT
void mailimap_acl_listrights_data_free(struct
mailimap_acl_listrights_data * listrights_data);
struct mailimap_acl_myrights_data {
char * mailbox;
char * rights;
};
struct mailimap_acl_myrights_data *
mailimap_acl_myrights_data_new(char * mailbox, char * rights);
LIBETPAN_EXPORT
void mailimap_acl_myrights_data_free(struct
mailimap_acl_myrights_data * myrights_data);
void
mailimap_acl_free(struct mailimap_extension_data * ext_data);
#ifdef __cplusplus
}
#endif
#endif