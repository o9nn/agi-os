#ifndef MAILIMAP_EXTENSION_TYPES_H
#define MAILIMAP_EXTENSION_TYPES_H
#include <libetpan/mailstream.h>
#include <libetpan/mailimap_types.h>
struct mailimap_extension_data;
enum {
MAILIMAP_EXTENSION_ANNOTATEMORE,
MAILIMAP_EXTENSION_ACL,
MAILIMAP_EXTENSION_UIDPLUS,
MAILIMAP_EXTENSION_QUOTA,
MAILIMAP_EXTENSION_NAMESPACE,
MAILIMAP_EXTENSION_XLIST,
MAILIMAP_EXTENSION_XGMLABELS,
MAILIMAP_EXTENSION_XGMMSGID,
MAILIMAP_EXTENSION_XGMTHRID,
MAILIMAP_EXTENSION_ID,
MAILIMAP_EXTENSION_ENABLE,
MAILIMAP_EXTENSION_CONDSTORE,
MAILIMAP_EXTENSION_QRESYNC,
MAILIMAP_EXTENSION_SORT
};
enum {
MAILIMAP_EXTENDED_PARSER_RESPONSE_DATA,
MAILIMAP_EXTENDED_PARSER_RESP_TEXT_CODE,
MAILIMAP_EXTENDED_PARSER_MAILBOX_DATA,
MAILIMAP_EXTENDED_PARSER_FETCH_DATA,
MAILIMAP_EXTENDED_PARSER_STATUS_ATT
};
struct mailimap_extension_api {
char * ext_name;
int ext_id;
int (* ext_parser)(int calling_parser, mailstream * fd,
MMAPString * buffer, struct mailimap_parser_context * parser_ctx, size_t * indx,
struct mailimap_extension_data ** result,
size_t progr_rate,
progress_function * progr_fun);
void (* ext_free)(struct mailimap_extension_data * ext_data);
};
struct mailimap_extension_data {
struct mailimap_extension_api * ext_extension;
int ext_type;
void * ext_data;
};
#endif