#include "lib.h"
#include "array.h"
#include "mail-namespace.h"
#include "mail-search.h"
#include "fts-api-private.h"
#include "fts-tokenizer.h"
#include "fts-filter.h"
#include "fts-user.h"
#include "fts-search-args.h"
static void strings_deduplicate(ARRAY_TYPE(const_string) *arr)
{
const char *const *strings;
unsigned int i, count;
strings = array_get(arr, &count);
for (i = 1; i < count; ) {
if (strcmp(strings[i-1], strings[i]) == 0) {
array_delete(arr, i, 1);
strings = array_get(arr, &count);
} else {
i++;
}
}
}
static struct mail_search_arg *
fts_search_arg_create_or(const struct mail_search_arg *orig_arg, pool_t pool,
const ARRAY_TYPE(const_string) *tokens)
{
struct mail_search_arg *arg, *or_arg, **argp;
const char *token;
or_arg = p_new(pool, struct mail_search_arg, 1);
or_arg->type = SEARCH_OR;
argp = &or_arg->value.subargs;
array_foreach_elem(tokens, token) {
arg = p_new(pool, struct mail_search_arg, 1);
*arg = *orig_arg;
arg->match_not = FALSE;
arg->next = NULL;
arg->value.str = p_strdup(pool, token);
*argp = arg;
argp = &arg->next;
}
return or_arg;
}
static int
fts_backend_dovecot_expand_tokens(struct fts_filter *filter,
pool_t pool,
struct mail_search_arg *parent_arg,
const struct mail_search_arg *orig_arg,
const char *orig_token, const char *token,
const char **error_r)
{
struct mail_search_arg *arg;
ARRAY_TYPE(const_string) tokens;
const char *token2, *error;
int ret;
t_array_init(&tokens, 4);
array_push_back(&tokens, &orig_token);
array_push_back(&tokens, &token);
if (filter != NULL) {
token2 = t_strdup(token);
ret = fts_filter_filter(filter, &token2, &error);
if (ret > 0) {
token2 = t_strdup(token2);
array_push_back(&tokens, &token2);
} else if (ret < 0) {
*error_r = t_strdup_printf("Couldn't filter search token: %s", error);
return -1;
} else {
return 0;
}
}
array_sort(&tokens, i_strcmp_p);
strings_deduplicate(&tokens);
arg = fts_search_arg_create_or(orig_arg, pool, &tokens);
arg->next = parent_arg->value.subargs;
parent_arg->value.subargs = arg;
return 0;
}
static int
fts_backend_dovecot_tokenize_lang(struct fts_user_language *user_lang,
pool_t pool, struct mail_search_arg *or_arg,
struct mail_search_arg *orig_arg,
const char *orig_token, const char **error_r)
{
size_t orig_token_len = strlen(orig_token);
struct mail_search_arg *and_arg, *orig_or_args = or_arg->value.subargs;
const char *token, *error;
int ret;
and_arg = p_new(pool, struct mail_search_arg, 1);
and_arg->type = SEARCH_SUB;
and_arg->next = orig_or_args;
or_arg->value.subargs = and_arg;
fts_tokenizer_reset(user_lang->search_tokenizer);
while ((ret = fts_tokenizer_next(user_lang->search_tokenizer,
(const void *)orig_token,
orig_token_len, &token, &error)) > 0) {
if (fts_backend_dovecot_expand_tokens(user_lang->filter, pool,
and_arg, orig_arg, orig_token,
token, error_r) < 0)
return -1;
}
while (ret >= 0 &&
(ret = fts_tokenizer_final(user_lang->search_tokenizer, &token, &error)) > 0) {
if (fts_backend_dovecot_expand_tokens(user_lang->filter, pool,
and_arg, orig_arg, orig_token,
token, error_r) < 0)
return -1;
}
if (ret < 0) {
*error_r = t_strdup_printf("Couldn't tokenize search args: %s", error);
return -1;
}
if (and_arg->value.subargs == NULL) {
or_arg->value.subargs = orig_or_args;
}
return 0;
}
static int fts_search_arg_expand(struct fts_backend *backend, pool_t pool,
struct mail_search_arg **argp)
{
struct event *event = backend->event;
const ARRAY_TYPE(fts_user_language) *languages;
struct fts_user_language *lang;
struct mail_search_arg *or_arg, *orig_arg = *argp;
const char *error, *orig_token = orig_arg->value.str;
if (((*argp)->type == SEARCH_HEADER ||
(*argp)->type == SEARCH_HEADER_ADDRESS ||
(*argp)->type == SEARCH_HEADER_COMPRESS_LWSP) &&
!fts_header_has_language((*argp)->hdr_field_name)) {
languages = fts_user_get_data_languages(backend->ns->user);
} else {
languages = fts_user_get_all_languages(backend->ns->user);
}
or_arg = p_new(pool, struct mail_search_arg, 1);
or_arg->type = SEARCH_OR;
or_arg->match_not = orig_arg->match_not;
or_arg->next = orig_arg->next;
array_foreach_elem(languages, lang) {
if (fts_backend_dovecot_tokenize_lang(lang, pool, or_arg,
orig_arg, orig_token, &error) < 0) {
e_error(event, "%s", error);
return -1;
}
}
if (or_arg->value.subargs == NULL) {
or_arg->type = SEARCH_ALL;
or_arg->match_not = !or_arg->match_not;
}
*argp = or_arg;
return 0;
}
static int
fts_search_args_expand_tree(struct fts_backend *backend, pool_t pool,
struct mail_search_arg **argp)
{
int ret;
for (; *argp != NULL; argp = &(*argp)->next) {
switch ((*argp)->type) {
case SEARCH_OR:
case SEARCH_SUB:
case SEARCH_INTHREAD:
if (fts_search_args_expand_tree(backend, pool,
&(*argp)->value.subargs) < 0)
return -1;
break;
case SEARCH_HEADER:
case SEARCH_HEADER_ADDRESS:
case SEARCH_HEADER_COMPRESS_LWSP:
if ((*argp)->value.str[0] == '\0') {
break;
}
case SEARCH_BODY:
case SEARCH_TEXT:
T_BEGIN {
ret = fts_search_arg_expand(backend, pool, argp);
} T_END;
if (ret < 0)
return -1;
break;
default:
break;
}
}
return 0;
}
int fts_search_args_expand(struct fts_backend *backend,
struct mail_search_args *args)
{
struct mail_search_arg *args_dup, *orig_args = args->args;
if (args->fts_expanded)
return 0;
args->fts_expanded = TRUE;
args_dup = mail_search_arg_dup(args->pool, args->args);
if (fts_search_args_expand_tree(backend, args->pool, &args_dup) < 0)
return -1;
args->simplified = FALSE;
args->args = args_dup;
mail_search_args_simplify(args);
i_assert(args->init_refcount > 0);
mail_search_arg_init(args, args_dup);
mail_search_arg_deinit(orig_args);
return 0;
}