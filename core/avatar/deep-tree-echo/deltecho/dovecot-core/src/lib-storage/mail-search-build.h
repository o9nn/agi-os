#ifndef MAIL_SEARCH_BUILD_H
#define MAIL_SEARCH_BUILD_H
#include "mail-search.h"
#include "mail-search-register.h"
struct mailbox;
struct mail_search_build_context {
pool_t pool;
struct mail_search_args *args;
struct mail_search_register *reg;
struct mail_search_parser *parser;
const char *charset;
struct mail_search_arg *parent;
const char *_error;
bool charset_checked;
bool unknown_charset;
};
struct mail_search_args *mail_search_build_init(void);
int mail_search_build(struct mail_search_register *reg,
struct mail_search_parser *parser, const char **charset,
struct mail_search_args **args_r,
const char **client_error_r);
struct mail_search_arg *
mail_search_build_add(struct mail_search_args *args,
enum mail_search_arg_type type);
void mail_search_build_add_all(struct mail_search_args *args);
void mail_search_build_add_seqset(struct mail_search_args *args,
uint32_t seq1, uint32_t seq2);
int mail_search_build_get_utf8(struct mail_search_build_context *ctx,
const char *input, const char **output_r);
struct mail_search_arg *
mail_search_build_new(struct mail_search_build_context *ctx,
enum mail_search_arg_type type);
struct mail_search_arg *
mail_search_build_str(struct mail_search_build_context *ctx,
enum mail_search_arg_type type);
int mail_search_build_key(struct mail_search_build_context *ctx,
struct mail_search_arg *parent,
struct mail_search_arg **arg_r);
#endif