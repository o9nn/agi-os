#ifndef ACL_PARSER_H
#define ACL_PARSER_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mailimap_parser.h"
#include "acl_types.h"
int
mailimap_acl_acl_data_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx,
struct mailimap_acl_acl_data ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_acl_listrights_data_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx,
struct mailimap_acl_listrights_data ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_acl_myrights_data_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx,
struct mailimap_acl_myrights_data ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_acl_identifier_rights_parse(mailstream * fd,
MMAPString *buffer, struct mailimap_parser_context * parser_ctx, size_t * indx,
struct mailimap_acl_identifier_rights ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_acl_identifier_parse(mailstream * fd,
MMAPString *buffer, struct mailimap_parser_context * parser_ctx, size_t * indx,
char ** result, size_t progr_rate,
progress_function * progr_fun);
int mailimap_acl_rights_parse(mailstream * fd,
MMAPString *buffer, struct mailimap_parser_context * parser_ctx, size_t * indx,
char ** result, size_t progr_rate,
progress_function * progr_fun);
int mailimap_acl_parse(int calling_parser, mailstream * fd,
MMAPString * buffer, struct mailimap_parser_context * parser_ctx, size_t * indx,
struct mailimap_extension_data ** result,
size_t progr_rate,
progress_function * progr_fun);
#ifdef __cplusplus
}
#endif
#endif