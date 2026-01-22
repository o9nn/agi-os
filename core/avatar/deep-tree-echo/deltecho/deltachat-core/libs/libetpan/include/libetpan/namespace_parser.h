#ifndef NAMESPACE_PARSER_H
#define NAMESPACE_PARSER_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mailimap_parser.h"
#include "namespace_types.h"
int
mailimap_namespace_extension_parse(int calling_parser, mailstream * fd,
MMAPString * buffer, struct mailimap_parser_context * parser_ctx, size_t * indx,
struct mailimap_extension_data ** result,
size_t progr_rate, progress_function * progr_fun);
#ifdef __cplusplus
}
#endif
#endif