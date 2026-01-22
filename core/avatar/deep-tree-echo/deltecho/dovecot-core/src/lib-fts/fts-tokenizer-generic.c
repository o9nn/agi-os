#include "lib.h"
#include "base64.h"
#include "buffer.h"
#include "str.h"
#include "unichar.h"
#include "bsearch-insert-pos.h"
#include "fts-common.h"
#include "fts-tokenizer-private.h"
#include "fts-tokenizer-generic-private.h"
#include "fts-tokenizer-common.h"
#include "word-boundary-data.c"
#include "word-break-data.c"
#define FTS_SKIP_BASE64_MIN_SEQUENCES 1
#define FTS_SKIP_BASE64_MIN_CHARS 50
#define FTS_DEFAULT_TOKEN_MAX_LENGTH 30
#define FTS_WB5A_PREFIX_MAX_LENGTH 3
static unsigned char fts_ascii_word_breaks[128] = {
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,
1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0
};
static int
fts_tokenizer_generic_create(const char *const *settings,
struct fts_tokenizer **tokenizer_r,
const char **error_r)
{
struct generic_fts_tokenizer *tok;
unsigned int max_length = FTS_DEFAULT_TOKEN_MAX_LENGTH;
enum boundary_algorithm algo = BOUNDARY_ALGORITHM_SIMPLE;
bool wb5a = FALSE;
bool search = FALSE;
bool explicitprefix = FALSE;
unsigned int i;
for (i = 0; settings[i] != NULL; i += 2) {
const char *key = settings[i], *value = settings[i+1];
if (strcmp(key, "maxlen") == 0) {
if (str_to_uint(value, &max_length) < 0 ||
max_length == 0) {
*error_r = t_strdup_printf(
"Invalid maxlen setting: %s", value);
return -1;
}
} else if (strcmp(key, "algorithm") == 0) {
if (strcmp(value, ALGORITHM_TR29_NAME) == 0)
algo = BOUNDARY_ALGORITHM_TR29;
else if (strcmp(value, ALGORITHM_SIMPLE_NAME) == 0)
;
else {
*error_r = t_strdup_printf(
"Invalid algorithm: %s", value);
return -1;
}
} else if (strcmp(key, "search") == 0) {
search = TRUE;
} else if (strcasecmp(key, "wb5a") == 0) {
if (strcasecmp(value, "no") == 0)
wb5a = FALSE;
else
wb5a = TRUE;
} else if (strcasecmp(key, "explicitprefix") == 0) {
explicitprefix = TRUE;
} else {
*error_r = t_strdup_printf("Unknown setting: %s", key);
return -1;
}
}
if (!search)
explicitprefix = FALSE;
if (wb5a && algo != BOUNDARY_ALGORITHM_TR29) {
*error_r = "Can not use WB5a for algorithms other than TR29.";
return -1;
}
tok = i_new(struct generic_fts_tokenizer, 1);
if (algo == BOUNDARY_ALGORITHM_TR29)
tok->tokenizer.v = &generic_tokenizer_vfuncs_tr29;
else
tok->tokenizer.v = &generic_tokenizer_vfuncs_simple;
tok->max_length = max_length;
tok->algorithm = algo;
tok->wb5a = wb5a;
tok->prefixsplat = explicitprefix;
tok->token = buffer_create_dynamic(default_pool, 64);
*tokenizer_r = &tok->tokenizer;
return 0;
}
static void
fts_tokenizer_generic_destroy(struct fts_tokenizer *_tok)
{
struct generic_fts_tokenizer *tok =
container_of(_tok, struct generic_fts_tokenizer, tokenizer);
buffer_free(&tok->token);
i_free(tok);
}
static inline void
shift_prev_type(struct generic_fts_tokenizer *tok, enum letter_type lt)
{
tok->prev_prev_type = tok->prev_type;
tok->prev_type = lt;
}
static inline void
add_prev_type(struct generic_fts_tokenizer *tok, enum letter_type lt)
{
if(tok->prev_type != LETTER_TYPE_NONE)
tok->prev_prev_type = tok->prev_type;
tok->prev_type = lt;
}
static inline void
add_letter(struct generic_fts_tokenizer *tok, unichar_t c)
{
if(tok->letter != 0)
tok->prev_letter = tok->letter;
tok->letter = c;
}
static bool
fts_tokenizer_generic_simple_current_token(struct generic_fts_tokenizer *tok,
const char **token_r)
{
const unsigned char *data = tok->token->data;
size_t len = tok->token->used;
if (tok->untruncated_length <= tok->max_length) {
if (len > 0 && data[len-1] == '\'') {
len--;
i_assert(len > 0 && data[len-1] != '\'');
}
if (len > 0 && data[len-1] == '*' && !tok->prefixsplat) {
len--;
i_assert(len > 0 && data[len-1] != '*');
}
} else {
fts_tokenizer_delete_trailing_partial_char(data, &len);
}
i_assert(len <= tok->max_length);
*token_r = len == 0 ? "" :
t_strndup(tok->token->data, len);
buffer_set_used_size(tok->token, 0);
tok->untruncated_length = 0;
return len > 0;
}
static bool uint32_find(const uint32_t *data, unsigned int count,
uint32_t value, unsigned int *idx_r)
{
BINARY_NUMBER_SEARCH(data, count, value, idx_r);
}
static bool fts_uni_word_break(unichar_t c)
{
unsigned int idx;
if (c >= 0x2000 && c <= 0x206f)
return TRUE;
if (uint32_find(White_Space, N_ELEMENTS(White_Space), c, &idx))
return TRUE;
if (uint32_find(Dash, N_ELEMENTS(Dash), c, &idx))
return TRUE;
if (uint32_find(Quotation_Mark, N_ELEMENTS(Quotation_Mark), c, &idx))
return TRUE;
if (uint32_find(Terminal_Punctuation, N_ELEMENTS(Terminal_Punctuation), c, &idx))
return TRUE;
if (uint32_find(STerm, N_ELEMENTS(STerm), c, &idx))
return TRUE;
if (uint32_find(Pattern_White_Space, N_ELEMENTS(Pattern_White_Space), c, &idx))
return TRUE;
return FALSE;
}
enum fts_break_type {
FTS_FROM_STOP = 0,
FTS_FROM_WORD = 2,
FTS_TO_STOP= 0,
FTS_TO_WORD = 1,
#define FROM_TO(f,t) FTS_##f##_TO_##t = FTS_FROM_##f | FTS_TO_##t
FROM_TO(STOP,STOP),
FROM_TO(STOP,WORD),
FROM_TO(WORD,STOP),
FROM_TO(WORD,WORD),
};
static inline enum fts_break_type
fts_simple_is_word_break(const struct generic_fts_tokenizer *tok,
unichar_t c, bool apostrophe)
{
if (apostrophe)
return tok->prev_type == LETTER_TYPE_ALETTER ? FTS_WORD_TO_WORD : FTS_STOP_TO_STOP;
bool new_breakiness = (c < 0x80) ? (fts_ascii_word_breaks[c] != 0) : fts_uni_word_break(c);
return (new_breakiness ? FTS_TO_STOP : FTS_TO_WORD)
+ (tok->prev_type == LETTER_TYPE_ALETTER ||
tok->prev_type == LETTER_TYPE_SINGLE_QUOTE
? FTS_FROM_WORD : FTS_FROM_STOP);
}
static void fts_tokenizer_generic_reset(struct fts_tokenizer *_tok)
{
struct generic_fts_tokenizer *tok =
container_of(_tok, struct generic_fts_tokenizer, tokenizer);
tok->prev_type = LETTER_TYPE_NONE;
tok->prev_prev_type = LETTER_TYPE_NONE;
tok->untruncated_length = 0;
buffer_set_used_size(tok->token, 0);
}
static void tok_append_truncated(struct generic_fts_tokenizer *tok,
const unsigned char *data, size_t size)
{
buffer_append(tok->token, data,
I_MIN(size, tok->max_length - tok->token->used));
tok->untruncated_length += size;
}
inline static bool
is_base64(const unsigned char ch)
{
return base64_scheme.decmap[ch] != 0xff;
}
#define allowed_base64_trailers allowed_base64_leaders
static unsigned char allowed_base64_leaders[] = {
' ', '\t', '\r', '\n', '=', ';', ':', '?'
};
static size_t
skip_base64(const unsigned char *data, size_t size)
{
if (data == NULL) {
i_assert(size == 0);
return 0;
}
const unsigned char *start, *end = data + size;
unsigned int matches = 0;
for (start = data; start < end; ) {
const unsigned char *first;
for (first = start; first < end && !is_base64(*first); first++);
if (first > start && memchr(allowed_base64_leaders, *(first - 1),
N_ELEMENTS(allowed_base64_leaders)) == NULL)
break;
const unsigned char *past;
for (past = first; past < end && is_base64(*past); past++);
if (past - first < FTS_SKIP_BASE64_MIN_CHARS)
break;
if (past < end && memchr(allowed_base64_trailers, *past,
N_ELEMENTS(allowed_base64_trailers)) == NULL)
break;
start = past;
matches++;
}
return matches < FTS_SKIP_BASE64_MIN_SEQUENCES ? 0 : start - data;
}
static int
fts_tokenizer_generic_simple_next(struct fts_tokenizer *_tok,
const unsigned char *data, size_t size,
size_t *skip_r, const char **token_r,
const char **error_r ATTR_UNUSED)
{
struct generic_fts_tokenizer *tok =
container_of(_tok, struct generic_fts_tokenizer, tokenizer);
size_t i, start;
int char_size;
unichar_t c;
bool apostrophe;
enum fts_break_type break_type;
start = tok->token->used > 0 ? 0 : skip_base64(data, size);
for (i = start; i < size; i += char_size) {
char_size = uni_utf8_get_char_n(data + i, size - i, &c);
i_assert(char_size > 0);
apostrophe = IS_APOSTROPHE(c);
if ((tok->prefixsplat && IS_PREFIX_SPLAT(c)) &&
(tok->prev_type == LETTER_TYPE_ALETTER)) {
shift_prev_type(tok, LETTER_TYPE_PREFIXSPLAT);
} else if ((break_type = fts_simple_is_word_break(tok, c, apostrophe))
!= FTS_WORD_TO_WORD) {
tok_append_truncated(tok, data + start, i - start);
shift_prev_type(tok, (break_type & FTS_TO_WORD) != 0
? LETTER_TYPE_ALETTER : LETTER_TYPE_NONE);
if (fts_tokenizer_generic_simple_current_token(tok, token_r)) {
*skip_r = i;
if (break_type != FTS_STOP_TO_WORD)
*skip_r += char_size;
return 1;
}
if ((break_type & FTS_TO_WORD) == 0)
start = i + char_size;
} else if (apostrophe) {
const unsigned char apostrophe_char = '\'';
tok_append_truncated(tok, data + start, i - start);
if (tok->token->used > 0)
tok_append_truncated(tok, &apostrophe_char, 1);
start = i + char_size;
shift_prev_type(tok, LETTER_TYPE_SINGLE_QUOTE);
} else {
shift_prev_type(tok, LETTER_TYPE_ALETTER);
}
}
if (i > start)
tok_append_truncated(tok, data + start, i - start);
*skip_r = i;
if (size == 0) {
shift_prev_type(tok, LETTER_TYPE_NONE);
if (fts_tokenizer_generic_simple_current_token(tok, token_r))
return 1;
}
return 0;
}
static enum letter_type letter_type(unichar_t c)
{
unsigned int idx;
if (IS_APOSTROPHE(c))
return LETTER_TYPE_APOSTROPHE;
if (uint32_find(CR, N_ELEMENTS(CR), c, &idx))
return LETTER_TYPE_CR;
if (uint32_find(LF, N_ELEMENTS(LF), c, &idx))
return LETTER_TYPE_LF;
if (uint32_find(Newline, N_ELEMENTS(Newline), c, &idx))
return LETTER_TYPE_NEWLINE;
if (uint32_find(Extend, N_ELEMENTS(Extend), c, &idx))
return LETTER_TYPE_EXTEND;
if (uint32_find(Regional_Indicator, N_ELEMENTS(Regional_Indicator), c, &idx))
return LETTER_TYPE_REGIONAL_INDICATOR;
if (uint32_find(Format, N_ELEMENTS(Format), c, &idx))
return LETTER_TYPE_FORMAT;
if (uint32_find(Katakana, N_ELEMENTS(Katakana), c, &idx))
return LETTER_TYPE_KATAKANA;
if (uint32_find(Hebrew_Letter, N_ELEMENTS(Hebrew_Letter), c, &idx))
return LETTER_TYPE_HEBREW_LETTER;
if (uint32_find(ALetter, N_ELEMENTS(ALetter), c, &idx))
return LETTER_TYPE_ALETTER;
if (uint32_find(Single_Quote, N_ELEMENTS(Single_Quote), c, &idx))
return LETTER_TYPE_SINGLE_QUOTE;
if (uint32_find(Double_Quote, N_ELEMENTS(Double_Quote), c, &idx))
return LETTER_TYPE_DOUBLE_QUOTE;
if (uint32_find(MidNumLet, N_ELEMENTS(MidNumLet), c, &idx))
return LETTER_TYPE_MIDNUMLET;
if (uint32_find(MidLetter, N_ELEMENTS(MidLetter), c, &idx))
return LETTER_TYPE_MIDLETTER;
if (uint32_find(MidNum, N_ELEMENTS(MidNum), c, &idx))
return LETTER_TYPE_MIDNUM;
if (uint32_find(Numeric, N_ELEMENTS(Numeric), c, &idx))
return LETTER_TYPE_NUMERIC;
if (uint32_find(ExtendNumLet, N_ELEMENTS(ExtendNumLet), c, &idx))
return LETTER_TYPE_EXTENDNUMLET;
if (IS_PREFIX_SPLAT(c))
return LETTER_TYPE_PREFIXSPLAT;
return LETTER_TYPE_OTHER;
}
static bool letter_panic(struct generic_fts_tokenizer *tok ATTR_UNUSED)
{
i_panic("Letter type should not be used.");
}
static bool letter_cr_lf_newline(struct generic_fts_tokenizer *tok ATTR_UNUSED)
{
return TRUE;
}
static bool letter_extend_format(struct generic_fts_tokenizer *tok ATTR_UNUSED)
{
return FALSE;
}
static bool letter_regional_indicator(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_REGIONAL_INDICATOR)
return FALSE;
return TRUE;
}
static bool letter_katakana(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_KATAKANA)
return FALSE;
if (tok->prev_type == LETTER_TYPE_EXTENDNUMLET)
return FALSE;
return TRUE;
}
static bool letter_hebrew(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_HEBREW_LETTER)
return FALSE;
if (tok->prev_prev_type == LETTER_TYPE_HEBREW_LETTER &&
(tok->prev_type == LETTER_TYPE_SINGLE_QUOTE ||
tok->prev_type == LETTER_TYPE_APOSTROPHE ||
tok->prev_type == LETTER_TYPE_MIDLETTER ||
tok->prev_type == LETTER_TYPE_DOUBLE_QUOTE))
return FALSE;
if (tok->prev_type == LETTER_TYPE_NUMERIC)
return FALSE;
if (tok->prev_type == LETTER_TYPE_EXTENDNUMLET)
return FALSE;
return TRUE;
}
static bool letter_aletter(struct generic_fts_tokenizer *tok)
{
if (tok->wb5a && tok->token->used <= FTS_WB5A_PREFIX_MAX_LENGTH)
if (IS_WB5A_APOSTROPHE(tok->prev_letter) && IS_VOWEL(tok->letter)) {
tok->seen_wb5a = TRUE;
return TRUE;
}
if (tok->prev_type == LETTER_TYPE_ALETTER)
return FALSE;
if (tok->prev_prev_type == LETTER_TYPE_ALETTER &&
(tok->prev_type == LETTER_TYPE_SINGLE_QUOTE ||
tok->prev_type == LETTER_TYPE_APOSTROPHE ||
tok->prev_type == LETTER_TYPE_MIDLETTER))
return FALSE;
if (tok->prev_type == LETTER_TYPE_NUMERIC)
return FALSE;
if (tok->prev_type == LETTER_TYPE_EXTENDNUMLET)
return FALSE;
return TRUE;
}
static bool letter_single_quote(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_ALETTER ||
tok->prev_type == LETTER_TYPE_HEBREW_LETTER)
return FALSE;
if (tok->prev_type == LETTER_TYPE_NUMERIC)
return FALSE;
return TRUE;
}
static bool letter_double_quote(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_DOUBLE_QUOTE)
return FALSE;
return TRUE;
}
static bool letter_midnumlet(struct generic_fts_tokenizer *tok ATTR_UNUSED)
{
return TRUE;
}
static bool letter_midletter(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_ALETTER ||
tok->prev_type == LETTER_TYPE_HEBREW_LETTER)
return FALSE;
return TRUE;
}
static bool letter_midnum(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_NUMERIC)
return FALSE;
return TRUE;
}
static bool letter_numeric(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_NUMERIC)
return FALSE;
if (tok->prev_type == LETTER_TYPE_ALETTER ||
tok->prev_type == LETTER_TYPE_HEBREW_LETTER)
return FALSE;
if(tok->prev_prev_type == LETTER_TYPE_NUMERIC &&
(tok->prev_type == LETTER_TYPE_MIDNUM ||
tok->prev_type == LETTER_TYPE_MIDNUMLET ||
tok->prev_type == LETTER_TYPE_SINGLE_QUOTE))
return FALSE;
if (tok->prev_type == LETTER_TYPE_EXTENDNUMLET)
return FALSE;
return TRUE;
}
static bool letter_extendnumlet(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_ALETTER ||
tok->prev_type == LETTER_TYPE_HEBREW_LETTER ||
tok->prev_type == LETTER_TYPE_NUMERIC ||
tok->prev_type == LETTER_TYPE_KATAKANA ||
tok->prev_type == LETTER_TYPE_EXTENDNUMLET)
return FALSE;
return TRUE;
}
static bool letter_apostrophe(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_ALETTER ||
tok->prev_type == LETTER_TYPE_HEBREW_LETTER)
return FALSE;
return TRUE;
}
static bool letter_prefixsplat(struct generic_fts_tokenizer *tok ATTR_UNUSED)
{
return TRUE;
}
static bool letter_other(struct generic_fts_tokenizer *tok ATTR_UNUSED)
{
return TRUE;
}
static bool is_nontoken(enum letter_type lt)
{
if (lt == LETTER_TYPE_REGIONAL_INDICATOR || lt == LETTER_TYPE_KATAKANA ||
lt == LETTER_TYPE_HEBREW_LETTER || lt == LETTER_TYPE_ALETTER ||
lt == LETTER_TYPE_NUMERIC)
return FALSE;
return TRUE;
}
static bool is_one_past_end(struct generic_fts_tokenizer *tok)
{
if (tok->prev_type == LETTER_TYPE_MIDLETTER ||
tok->prev_type == LETTER_TYPE_MIDNUMLET ||
tok->prev_type == LETTER_TYPE_APOSTROPHE ||
tok->prev_type == LETTER_TYPE_SINGLE_QUOTE )
return TRUE;
if (tok->prev_type == LETTER_TYPE_MIDNUM ||
tok->prev_type == LETTER_TYPE_MIDNUMLET ||
tok->prev_type == LETTER_TYPE_APOSTROPHE ||
tok->prev_type == LETTER_TYPE_SINGLE_QUOTE)
return TRUE;
return FALSE;
}
static void
fts_tokenizer_generic_tr29_current_token(struct generic_fts_tokenizer *tok,
const char **token_r)
{
const unsigned char *data = tok->token->data;
size_t len = tok->token->used;
if (is_one_past_end(tok) &&
tok->untruncated_length <= tok->max_length) {
while (!UTF8_IS_START_SEQ(data[len-1]))
len--;
i_assert(len > 0);
len--;
} else if (tok->untruncated_length > tok->max_length) {
fts_tokenizer_delete_trailing_partial_char(data, &len);
}
i_assert(len > 0);
i_assert(len <= tok->max_length);
tok->prev_prev_type = LETTER_TYPE_NONE;
tok->prev_type = LETTER_TYPE_NONE;
*token_r = t_strndup(data, len);
buffer_set_used_size(tok->token, 0);
tok->untruncated_length = 0;
}
static void wb5a_reinsert(struct generic_fts_tokenizer *tok)
{
string_t *utf8_str = t_str_new(6);
uni_ucs4_to_utf8_c(tok->letter, utf8_str);
buffer_insert(tok->token, 0, str_data(utf8_str), str_len(utf8_str));
tok->prev_type = letter_type(tok->letter);
tok->letter = 0;
tok->prev_letter = 0;
tok->seen_wb5a = FALSE;
}
struct letter_fn {
bool (*fn)(struct generic_fts_tokenizer *tok);
};
static struct letter_fn letter_fns[] = {
{letter_panic}, {letter_cr_lf_newline}, {letter_cr_lf_newline},
{letter_cr_lf_newline}, {letter_extend_format},
{letter_regional_indicator}, {letter_extend_format},
{letter_katakana}, {letter_hebrew}, {letter_aletter},
{letter_single_quote}, {letter_double_quote},
{letter_midnumlet}, {letter_midletter}, {letter_midnum},
{letter_numeric}, {letter_extendnumlet}, {letter_panic},
{letter_panic}, {letter_apostrophe}, {letter_prefixsplat},
{letter_other}
};
static bool
uni_found_word_boundary(struct generic_fts_tokenizer *tok, enum letter_type lt)
{
if (tok->prev_type != LETTER_TYPE_NONE) {
if (letter_fns[lt].fn(tok))
return TRUE;
}
if (lt == LETTER_TYPE_EXTEND || lt == LETTER_TYPE_FORMAT) {
} else {
add_prev_type(tok,lt);
}
return FALSE;
}
static int
fts_tokenizer_generic_tr29_next(struct fts_tokenizer *_tok,
const unsigned char *data, size_t size,
size_t *skip_r, const char **token_r,
const char **error_r ATTR_UNUSED)
{
struct generic_fts_tokenizer *tok =
container_of(_tok, struct generic_fts_tokenizer, tokenizer);
unichar_t c;
size_t i, char_start_i, start_pos;
enum letter_type lt;
int char_size;
start_pos = tok->token->used > 0 ? 0 : skip_base64(data, size);
for (i = start_pos; i < size; ) {
char_start_i = i;
char_size = uni_utf8_get_char_n(data + i, size - i, &c);
i_assert(char_size > 0);
i += char_size;
lt = letter_type(c);
if (tok->seen_wb5a)
wb5a_reinsert(tok);
if (tok->prev_type == LETTER_TYPE_NONE && is_nontoken(lt)) {
i_assert(tok->token->used == 0);
start_pos = i;
continue;
}
if (tok->wb5a &&  tok->token->used <= FTS_WB5A_PREFIX_MAX_LENGTH)
add_letter(tok, c);
if (uni_found_word_boundary(tok, lt)) {
i_assert(char_start_i >= start_pos && size >= start_pos);
tok_append_truncated(tok, data + start_pos,
char_start_i - start_pos);
if (lt == LETTER_TYPE_PREFIXSPLAT && tok->prefixsplat) {
const unsigned char prefix_char = FTS_PREFIX_SPLAT_CHAR;
tok_append_truncated(tok, &prefix_char, 1);
}
*skip_r = i;
fts_tokenizer_generic_tr29_current_token(tok, token_r);
return 1;
} else if (lt == LETTER_TYPE_APOSTROPHE ||
lt == LETTER_TYPE_SINGLE_QUOTE) {
const unsigned char apostrophe_char = '\'';
tok_append_truncated(tok, data + start_pos,
char_start_i - start_pos);
tok_append_truncated(tok, &apostrophe_char, 1);
start_pos = i;
}
}
i_assert(i >= start_pos && size >= start_pos);
if (i > start_pos)
tok_append_truncated(tok, data + start_pos, i - start_pos);
*skip_r = i;
if (size == 0 && tok->token->used > 0) {
*skip_r = 0;
fts_tokenizer_generic_tr29_current_token(tok, token_r);
return 1;
}
return 0;
}
static int
fts_tokenizer_generic_next(struct fts_tokenizer *_tok ATTR_UNUSED,
const unsigned char *data ATTR_UNUSED,
size_t size ATTR_UNUSED,
size_t *skip_r ATTR_UNUSED,
const char **token_r ATTR_UNUSED,
const char **error_r ATTR_UNUSED)
{
i_unreached();
}
static const struct fts_tokenizer_vfuncs generic_tokenizer_vfuncs = {
fts_tokenizer_generic_create,
fts_tokenizer_generic_destroy,
fts_tokenizer_generic_reset,
fts_tokenizer_generic_next
};
static const struct fts_tokenizer fts_tokenizer_generic_real = {
.name = "generic",
.v = &generic_tokenizer_vfuncs
};
const struct fts_tokenizer *fts_tokenizer_generic = &fts_tokenizer_generic_real;
const struct fts_tokenizer_vfuncs generic_tokenizer_vfuncs_simple = {
fts_tokenizer_generic_create,
fts_tokenizer_generic_destroy,
fts_tokenizer_generic_reset,
fts_tokenizer_generic_simple_next
};
const struct fts_tokenizer_vfuncs generic_tokenizer_vfuncs_tr29 = {
fts_tokenizer_generic_create,
fts_tokenizer_generic_destroy,
fts_tokenizer_generic_reset,
fts_tokenizer_generic_tr29_next
};