#ifndef ANNOTATEMORE_PARSER_H
#define ANNOTATEMORE_PARSER_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mailimap_parser.h"
#include "annotatemore_types.h"
int
mailimap_annotatemore_annotate_data_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx,
struct mailimap_annotatemore_annotate_data ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_annotatemore_entry_list_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx,
struct mailimap_annotatemore_entry_list ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_annotatemore_entry_att_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx,
struct mailimap_annotatemore_entry_att ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_annotatemore_att_value_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx,
struct mailimap_annotatemore_att_value ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_annotatemore_attrib_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx, char ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_annotatemore_value_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx, char ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_annotatemore_entry_parse(mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx, char ** result,
size_t progr_rate,
progress_function * progr_fun);
int
mailimap_annotatemore_text_code_annotatemore_parse(mailstream * fd,
MMAPString *buffer, struct mailimap_parser_context * parser_ctx, size_t * indx, int * result,
size_t progr_rate, progress_function * progr_fun);
int mailimap_annotatemore_parse(int calling_parser, mailstream * fd,
MMAPString * buffer, struct mailimap_parser_context * parser_ctx, size_t * indx,
struct mailimap_extension_data ** result,
size_t progr_rate,
progress_function * progr_fun);
#ifdef __cplusplus
}
#endif
#endif