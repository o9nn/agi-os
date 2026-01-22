#include "lib.h"
#include "str.h"
#include "buffer.h"
#include "rfc822-parser.h"
#include "fts-tokenizer-private.h"
#include "fts-tokenizer-common.h"
#define IS_DTEXT(c) \
(rfc822_atext_chars[(int)(unsigned char)(c)] == 2)
#define FTS_DEFAULT_ADDRESS_MAX_LENGTH 254
enum email_address_parser_state {
EMAIL_ADDRESS_PARSER_STATE_NONE = 0,
EMAIL_ADDRESS_PARSER_STATE_LOCALPART,
EMAIL_ADDRESS_PARSER_STATE_DOMAIN,
EMAIL_ADDRESS_PARSER_STATE_COMPLETE,
EMAIL_ADDRESS_PARSER_STATE_SKIP,
};
struct email_address_fts_tokenizer {
struct fts_tokenizer tokenizer;
enum email_address_parser_state state;
string_t *last_word;
string_t *parent_data;
unsigned int max_length;
bool search;
};
static int
fts_tokenizer_email_address_create(const char *const *settings,
struct fts_tokenizer **tokenizer_r,
const char **error_r)
{
struct email_address_fts_tokenizer *tok;
bool search = FALSE;
unsigned int max_length = FTS_DEFAULT_ADDRESS_MAX_LENGTH;
unsigned int i;
for (i = 0; settings[i] != NULL; i += 2) {
const char *key = settings[i], *value = settings[i+1];
if (strcmp(key, "search") == 0) {
search = TRUE;
} else if (strcmp(key, "maxlen") == 0) {
if (str_to_uint(value, &max_length) < 0 ||
max_length == 0) {
*error_r = t_strdup_printf("Invalid maxlen setting: %s", value);
return -1;
}
} else {
*error_r = t_strdup_printf("Unknown setting: %s", key);
return -1;
}
}
tok = i_new(struct email_address_fts_tokenizer, 1);
tok->tokenizer = *fts_tokenizer_email_address;
tok->last_word = str_new(default_pool, 128);
tok->parent_data = str_new(default_pool, 128);
tok->max_length = max_length;
tok->search = search;
*tokenizer_r = &tok->tokenizer;
return 0;
}
static void fts_tokenizer_email_address_destroy(struct fts_tokenizer *_tok)
{
struct email_address_fts_tokenizer *tok =
(struct email_address_fts_tokenizer *)_tok;
str_free(&tok->last_word);
str_free(&tok->parent_data);
i_free(tok);
}
static bool
fts_tokenizer_address_current_token(struct email_address_fts_tokenizer *tok,
const char **token_r)
{
const unsigned char *data = tok->last_word->data;
size_t len = tok->last_word->used;
tok->tokenizer.skip_parents = TRUE;
tok->state = EMAIL_ADDRESS_PARSER_STATE_NONE;
if (str_len(tok->last_word) > tok->max_length) {
str_truncate(tok->last_word, tok->max_length);
len = tok->last_word->used;
fts_tokenizer_delete_trailing_partial_char(data, &len);
i_assert(len <= tok->max_length);
}
if (len > 0)
fts_tokenizer_delete_trailing_invalid_char(data, &len);
*token_r = len == 0 ? "" :
t_strndup(data, len);
return len > 0;
}
static bool
fts_tokenizer_address_parent_data(struct email_address_fts_tokenizer *tok,
const char **token_r)
{
if (tok->tokenizer.parent == NULL || str_len(tok->parent_data) == 0)
return FALSE;
if (tok->search && tok->state >= EMAIL_ADDRESS_PARSER_STATE_DOMAIN) {
size_t parent_prefix_len =
str_len(tok->parent_data) - str_len(tok->last_word);
i_assert(str_len(tok->parent_data) >= str_len(tok->last_word) &&
strcmp(str_c(tok->parent_data) + parent_prefix_len,
str_c(tok->last_word)) == 0);
str_truncate(tok->parent_data, parent_prefix_len);
if (str_len(tok->parent_data) == 0)
return FALSE;
}
*token_r = t_strdup(str_c(tok->parent_data));
str_truncate(tok->parent_data, 0);
return TRUE;
}
static size_t skip_nonlocal_part(const unsigned char *data, size_t size)
{
size_t skip = 0;
while (skip < size && (!IS_ATEXT(data[skip]) && data[skip] != '.'))
skip++;
return skip;
}
static bool
fts_tokenizer_email_address_too_large(struct email_address_fts_tokenizer *tok,
size_t pos)
{
if (str_len(tok->last_word) + pos <= tok->max_length)
return FALSE;
str_truncate(tok->last_word, 0);
return TRUE;
}
static enum email_address_parser_state
fts_tokenizer_email_address_parse_local(struct email_address_fts_tokenizer *tok,
const unsigned char *data, size_t size,
size_t *skip_r)
{
size_t pos = 0;
bool seen_at = FALSE;
i_assert(size == 0 || data != NULL);
while (pos < size && (IS_ATEXT(data[pos]) ||
data[pos] == '@' || data[pos] == '.')) {
if (data[pos] == '@')
seen_at = TRUE;
pos++;
if (seen_at)
break;
}
if (fts_tokenizer_email_address_too_large(tok, pos)) {
*skip_r = 0;
return EMAIL_ADDRESS_PARSER_STATE_SKIP;
}
if (seen_at && (pos > 1 || str_len(tok->last_word) > 0)) {
str_append_data(tok->last_word, data, pos);
*skip_r = pos;
return EMAIL_ADDRESS_PARSER_STATE_DOMAIN;
}
if (pos > 0 && (IS_ATEXT(data[pos-1]) || data[pos-1] == '.')) {
str_append_data(tok->last_word, data, pos);
*skip_r = pos;
return  EMAIL_ADDRESS_PARSER_STATE_LOCALPART;
}
pos += skip_nonlocal_part(data+pos, size - pos);
*skip_r = pos;
return EMAIL_ADDRESS_PARSER_STATE_NONE;
}
static bool domain_is_empty(struct email_address_fts_tokenizer *tok)
{
const char *p, *str = str_c(tok->last_word);
if ((p = strchr(str, '@')) == NULL)
return TRUE;
return p[1] == '\0';
}
static enum email_address_parser_state
fts_tokenizer_email_address_parse_domain(struct email_address_fts_tokenizer *tok,
const unsigned char *data, size_t size,
size_t *skip_r)
{
size_t pos = 0;
while (pos < size && (IS_DTEXT(data[pos]) || data[pos] == '.' || data[pos] == '-'))
pos++;
if (fts_tokenizer_email_address_too_large(tok, pos)) {
*skip_r = 0;
return EMAIL_ADDRESS_PARSER_STATE_SKIP;
}
if ((pos > 0 && pos < size) ||
(pos < size && !domain_is_empty(tok))) {
str_append_data(tok->last_word, data, pos);
*skip_r = pos;
return EMAIL_ADDRESS_PARSER_STATE_COMPLETE;
}
if (pos == size) {
str_append_data(tok->last_word, data, pos);
*skip_r = pos;
return EMAIL_ADDRESS_PARSER_STATE_DOMAIN;
}
pos += skip_nonlocal_part(data + pos, size - pos);
*skip_r = pos;
return EMAIL_ADDRESS_PARSER_STATE_NONE;
}
static bool
fts_tokenizer_address_skip(const unsigned char *data, size_t size,
size_t *skip_r)
{
for (size_t pos = 0; pos < size; pos++) {
if (!(IS_ATEXT(data[pos]) || data[pos] == '.' ||
data[pos] == '-') || data[pos] == '@') {
*skip_r = pos;
return TRUE;
}
}
*skip_r = size;
return FALSE;
}
static void
fts_tokenizer_address_update_parent(struct email_address_fts_tokenizer *tok,
const unsigned char *data, size_t size)
{
if (tok->tokenizer.parent != NULL)
str_append_data(tok->parent_data, data, size);
}
static void fts_tokenizer_email_address_reset(struct fts_tokenizer *_tok)
{
struct email_address_fts_tokenizer *tok =
(struct email_address_fts_tokenizer *)_tok;
tok->state = EMAIL_ADDRESS_PARSER_STATE_NONE;
str_truncate(tok->last_word, 0);
str_truncate(tok->parent_data, 0);
}
static int
fts_tokenizer_email_address_next(struct fts_tokenizer *_tok,
const unsigned char *data, size_t size,
size_t *skip_r, const char **token_r,
const char **error_r ATTR_UNUSED)
{
struct email_address_fts_tokenizer *tok =
(struct email_address_fts_tokenizer *)_tok;
size_t pos = 0, local_skip;
bool finished;
if (tok->tokenizer.skip_parents == TRUE)
tok->tokenizer.skip_parents = FALSE;
if (tok->state == EMAIL_ADDRESS_PARSER_STATE_COMPLETE) {
*skip_r = pos;
if (fts_tokenizer_address_current_token(tok, token_r))
return 1;
}
if (size == 0) {
if (tok->state == EMAIL_ADDRESS_PARSER_STATE_DOMAIN &&
domain_is_empty(tok)) {
str_truncate(tok->last_word, 0);
tok->state = EMAIL_ADDRESS_PARSER_STATE_NONE;
}
if (fts_tokenizer_address_parent_data(tok, token_r))
return 1;
if (tok->state == EMAIL_ADDRESS_PARSER_STATE_DOMAIN) {
if (fts_tokenizer_address_current_token(tok, token_r))
return 1;
}
tok->state = EMAIL_ADDRESS_PARSER_STATE_NONE;
}
while(pos < size || tok->state == EMAIL_ADDRESS_PARSER_STATE_COMPLETE) {
switch (tok->state) {
case EMAIL_ADDRESS_PARSER_STATE_NONE:
str_truncate(tok->last_word, 0);
if (fts_tokenizer_address_parent_data(tok, token_r)) {
*skip_r = pos;
return 1;
}
case EMAIL_ADDRESS_PARSER_STATE_LOCALPART:
tok->state =
fts_tokenizer_email_address_parse_local(tok,
data + pos,
size - pos,
&local_skip);
fts_tokenizer_address_update_parent(tok, data+pos,
local_skip);
pos += local_skip;
break;
case EMAIL_ADDRESS_PARSER_STATE_DOMAIN:
tok->state =
fts_tokenizer_email_address_parse_domain(tok,
data + pos,
size - pos,
&local_skip);
fts_tokenizer_address_update_parent(tok, data+pos,
local_skip);
pos += local_skip;
break;
case EMAIL_ADDRESS_PARSER_STATE_COMPLETE:
*skip_r = pos;
if (fts_tokenizer_address_parent_data(tok, token_r))
return 1;
if (fts_tokenizer_address_current_token(tok, token_r))
return 1;
break;
case EMAIL_ADDRESS_PARSER_STATE_SKIP:
*skip_r = pos;
if (fts_tokenizer_address_parent_data(tok, token_r))
return 1;
finished = fts_tokenizer_address_skip(data + pos,
size - pos,
&local_skip);
fts_tokenizer_address_update_parent(tok, data+pos,
local_skip);
pos += local_skip;
if (finished) {
*skip_r = pos;
if (fts_tokenizer_address_parent_data(tok, token_r)) {
tok->state = EMAIL_ADDRESS_PARSER_STATE_NONE;
return 1;
}
tok->state = EMAIL_ADDRESS_PARSER_STATE_NONE;
}
break;
default:
i_unreached();
}
}
*skip_r = pos;
return 0;
}
static const struct fts_tokenizer_vfuncs email_address_tokenizer_vfuncs = {
fts_tokenizer_email_address_create,
fts_tokenizer_email_address_destroy,
fts_tokenizer_email_address_reset,
fts_tokenizer_email_address_next
};
static const struct fts_tokenizer fts_tokenizer_email_address_real = {
.name = "email-address",
.v = &email_address_tokenizer_vfuncs,
.stream_to_parents = TRUE,
};
const struct fts_tokenizer *fts_tokenizer_email_address =
&fts_tokenizer_email_address_real;