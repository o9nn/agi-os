#include "lib.h"
#include "ioloop.h"
#include "array.h"
#include "buffer.h"
#include "istream.h"
#include "str.h"
#include "write-full.h"
#include "message-parser.h"
#include "mail-index.h"
#include "mbox-storage.h"
#include "mbox-md5.h"
#include "mbox-sync-private.h"
#define IS_LWSP_LF(c) (IS_LWSP(c) || (c) == '\n')
struct mbox_sync_header_func {
const char *header;
bool (*func)(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr);
};
struct mbox_flag_type mbox_status_flags[] = {
{ 'R', MAIL_SEEN },
{ 'O', MBOX_NONRECENT_KLUDGE },
{ 0, 0 }
};
struct mbox_flag_type mbox_xstatus_flags[] = {
{ 'A', MAIL_ANSWERED },
{ 'F', MAIL_FLAGGED },
{ 'T', MAIL_DRAFT },
{ 'D', MAIL_DELETED },
{ 0, 0 }
};
static void parse_trailing_whitespace(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
size_t i, space = 0;
for (i = hdr->full_value_len; i > 0; i--) {
if (!IS_LWSP(hdr->full_value[i-1]))
break;
space++;
}
if ((ssize_t)space > ctx->mail.space) {
i_assert(space != 0);
ctx->mail.offset = ctx->hdr_offset + str_len(ctx->header) + i;
ctx->mail.space = space;
}
}
static enum mail_flags mbox_flag_find(struct mbox_flag_type *flags, char chr)
{
int i;
for (i = 0; flags[i].chr != 0; i++) {
if (flags[i].chr == chr)
return flags[i].flag;
}
return 0;
}
static bool parse_status_flags(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr,
struct mbox_flag_type *flags_list)
{
enum mail_flags flag;
size_t i;
bool duplicates = FALSE;
ctx->mail.flags ^= MBOX_NONRECENT_KLUDGE;
for (i = 0; i < hdr->full_value_len; i++) {
flag = mbox_flag_find(flags_list, hdr->full_value[i]);
if ((ctx->mail.flags & flag) != 0)
duplicates = TRUE;
else
ctx->mail.flags |= flag;
}
ctx->mail.flags ^= MBOX_NONRECENT_KLUDGE;
return duplicates;
}
static bool parse_status(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
if (parse_status_flags(ctx, hdr, mbox_status_flags))
ctx->mail.status_broken = TRUE;
ctx->hdr_pos[MBOX_HDR_STATUS] = str_len(ctx->header);
return TRUE;
}
static bool parse_x_status(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
if (parse_status_flags(ctx, hdr, mbox_xstatus_flags))
ctx->mail.xstatus_broken = TRUE;
ctx->hdr_pos[MBOX_HDR_X_STATUS] = str_len(ctx->header);
return TRUE;
}
static void
parse_imap_keywords_list(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr, size_t pos)
{
struct mailbox *box = &ctx->sync_ctx->mbox->box;
struct index_mailbox_context *ibox = INDEX_STORAGE_CONTEXT(box);
const char *keyword, *error;
size_t keyword_start;
unsigned int idx, count;
count = 0;
while (pos < hdr->full_value_len) {
if (IS_LWSP_LF(hdr->full_value[pos])) {
pos++;
continue;
}
keyword_start = pos;
for (; pos < hdr->full_value_len; pos++) {
if (IS_LWSP_LF(hdr->full_value[pos]))
break;
}
keyword = t_strndup(hdr->full_value + keyword_start,
pos - keyword_start);
if (mailbox_keyword_is_valid(&ctx->sync_ctx->mbox->box,
keyword, &error)) {
mail_index_keyword_lookup_or_create(box->index,
keyword, &idx);
}
count++;
}
if (count != array_count(ibox->keyword_names)) {
ctx->imapbase_rewrite = TRUE;
ctx->need_rewrite = TRUE;
}
}
static bool parse_x_imap_base(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
size_t i, j, uid_last_pos;
uint32_t uid_validity, uid_last;
if (ctx->seq != 1 || ctx->seen_imapbase ||
ctx->sync_ctx->renumber_uids) {
return FALSE;
}
for (i = 0, uid_validity = 0; i < hdr->full_value_len; i++) {
if (hdr->full_value[i] < '0' || hdr->full_value[i] > '9') {
if (hdr->full_value[i] != ' ')
return FALSE;
break;
}
uid_validity = uid_validity * 10 + (hdr->full_value[i] - '0');
}
if (uid_validity == 0) {
return FALSE;
}
for (; i < hdr->full_value_len; i++) {
if (!IS_LWSP_LF(hdr->full_value[i]))
break;
}
uid_last_pos = i;
for (uid_last = 0, j = 0; i < hdr->full_value_len; i++, j++) {
if (hdr->full_value[i] < '0' || hdr->full_value[i] > '9') {
if (!IS_LWSP_LF(hdr->full_value[i]))
return FALSE;
break;
}
uid_last = uid_last * 10 + (hdr->full_value[i] - '0');
}
if (j != 10 ||
hdr->full_value_offset != ctx->hdr_offset + str_len(ctx->header)) {
ctx->imapbase_rewrite = TRUE;
ctx->need_rewrite = TRUE;
} else {
ctx->last_uid_value_start_pos = uid_last_pos;
ctx->sync_ctx->base_uid_last_offset =
hdr->full_value_offset + uid_last_pos;
}
if (ctx->sync_ctx->base_uid_validity == 0) {
ctx->sync_ctx->base_uid_validity = uid_validity;
ctx->sync_ctx->base_uid_last = uid_last;
if (ctx->sync_ctx->next_uid-1 <= uid_last) {
ctx->sync_ctx->next_uid = uid_last+1;
} else {
ctx->need_rewrite = TRUE;
}
i_assert(ctx->sync_ctx->next_uid > ctx->sync_ctx->prev_msg_uid);
}
ctx->hdr_pos[MBOX_HDR_X_IMAPBASE] = str_len(ctx->header);
ctx->seen_imapbase = TRUE;
T_BEGIN {
parse_imap_keywords_list(ctx, hdr, i);
} T_END;
parse_trailing_whitespace(ctx, hdr);
return TRUE;
}
static bool parse_x_imap(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
if (!parse_x_imap_base(ctx, hdr))
return FALSE;
ctx->mail.pseudo = TRUE;
return TRUE;
}
static bool parse_x_keywords_real(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
struct mailbox *box = &ctx->sync_ctx->mbox->box;
ARRAY_TYPE(keyword_indexes) keyword_list;
const unsigned int *list;
string_t *keyword;
size_t keyword_start;
unsigned int i, idx, count;
size_t pos;
if (array_is_created(&ctx->mail.keywords))
return FALSE;
keyword = t_str_new(128);
t_array_init(&keyword_list, 16);
for (pos = 0; pos < hdr->full_value_len; ) {
if (IS_LWSP_LF(hdr->full_value[pos])) {
pos++;
continue;
}
keyword_start = pos;
for (; pos < hdr->full_value_len; pos++) {
if (IS_LWSP_LF(hdr->full_value[pos]))
break;
}
str_truncate(keyword, 0);
str_append_data(keyword, hdr->full_value + keyword_start,
pos - keyword_start);
if (!mail_index_keyword_lookup(box->index, str_c(keyword),
&idx)) {
return FALSE;
}
list = array_get(&keyword_list, &count);
for (i = 0; i < count; i++) {
if (list[i] == idx)
break;
}
if (i == count)
array_push_back(&keyword_list, &idx);
}
if (array_count(&keyword_list) > 0) {
p_array_init(&ctx->mail.keywords,
ctx->sync_ctx->mail_keyword_pool,
array_count(&keyword_list));
array_append_array(&ctx->mail.keywords, &keyword_list);
}
ctx->hdr_pos[MBOX_HDR_X_KEYWORDS] = str_len(ctx->header);
parse_trailing_whitespace(ctx, hdr);
return TRUE;
}
static bool parse_x_keywords(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
bool ret;
T_BEGIN {
ret = parse_x_keywords_real(ctx, hdr);
} T_END;
return ret;
}
static bool parse_x_uid(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
uint32_t value = 0;
size_t i;
if (ctx->mail.uid != 0) {
return FALSE;
}
for (i = 0; i < hdr->full_value_len; i++) {
if (hdr->full_value[i] < '0' || hdr->full_value[i] > '9')
break;
value = value*10 + (hdr->full_value[i] - '0');
}
for (; i < hdr->full_value_len; i++) {
if (!IS_LWSP_LF(hdr->full_value[i])) {
return FALSE;
}
}
if (ctx->sync_ctx == NULL) {
ctx->mail.uid = value;
return TRUE;
}
if (ctx->seq == 1 && !ctx->seen_imapbase) {
return FALSE;
}
if (value == ctx->sync_ctx->next_uid) {
ctx->sync_ctx->next_uid++;
} else if (value > ctx->sync_ctx->next_uid) {
ctx->mail.uid_broken = TRUE;
return FALSE;
}
if (value <= ctx->sync_ctx->prev_msg_uid) {
ctx->mail.uid_broken = TRUE;
return FALSE;
}
ctx->mail.uid = value;
ctx->mail.uid_broken = FALSE;
if (ctx->sync_ctx->dest_first_mail && ctx->seq != 1) {
return FALSE;
}
ctx->hdr_pos[MBOX_HDR_X_UID] = str_len(ctx->header);
ctx->parsed_uid = value;
parse_trailing_whitespace(ctx, hdr);
return TRUE;
}
static bool parse_content_length(struct mbox_sync_mail_context *ctx,
struct message_header_line *hdr)
{
uoff_t value = 0;
size_t i;
if (ctx->content_length != UOFF_T_MAX) {
return FALSE;
}
for (i = 0; i < hdr->full_value_len; i++) {
if (hdr->full_value[i] < '0' || hdr->full_value[i] > '9')
break;
value = value*10 + (hdr->full_value[i] - '0');
}
for (; i < hdr->full_value_len; i++) {
if (!IS_LWSP_LF(hdr->full_value[i])) {
return FALSE;
}
}
ctx->content_length = value;
return TRUE;
}
static struct mbox_sync_header_func header_funcs[] = {
{ "Content-Length", parse_content_length },
{ "Status", parse_status },
{ "X-IMAP", parse_x_imap },
{ "X-IMAPbase", parse_x_imap_base },
{ "X-Keywords", parse_x_keywords },
{ "X-Status", parse_x_status },
{ "X-UID", parse_x_uid }
};
static int mbox_sync_bsearch_header_func_cmp(const void *p1, const void *p2)
{
const char *key = p1;
const struct mbox_sync_header_func *func = p2;
return strcasecmp(key, func->header);
}
int mbox_sync_parse_next_mail(struct istream *input,
struct mbox_sync_mail_context *ctx)
{
struct mbox_sync_context *sync_ctx = ctx->sync_ctx;
struct message_header_parser_ctx *hdr_ctx;
struct message_header_line *hdr;
struct mbox_sync_header_func *func;
struct mbox_md5_context *mbox_md5_ctx;
size_t line_start_pos;
int i, ret;
ctx->hdr_offset = ctx->mail.offset;
ctx->mail.flags = MAIL_RECENT;
ctx->header_first_change = SIZE_MAX;
ctx->header_last_change = 0;
for (i = 0; i < MBOX_HDR_COUNT; i++)
ctx->hdr_pos[i] = SIZE_MAX;
ctx->content_length = UOFF_T_MAX;
str_truncate(ctx->header, 0);
mbox_md5_ctx = ctx->sync_ctx->mbox->md5_v.init();
line_start_pos = 0;
hdr_ctx = message_parse_header_init(input, NULL, 0);
while ((ret = message_parse_header_next(hdr_ctx, &hdr)) > 0) {
if (hdr->eoh) {
ctx->have_eoh = TRUE;
break;
}
if (!hdr->continued) {
line_start_pos = str_len(ctx->header);
str_append(ctx->header, hdr->name);
str_append_data(ctx->header, hdr->middle, hdr->middle_len);
}
func = bsearch(hdr->name, header_funcs,
N_ELEMENTS(header_funcs), sizeof(*header_funcs),
mbox_sync_bsearch_header_func_cmp);
if (func != NULL) {
if (hdr->continues) {
hdr->use_full_value = TRUE;
continue;
}
if (!func->func(ctx, hdr)) {
ctx->need_rewrite = TRUE;
str_truncate(ctx->header, line_start_pos);
if (ctx->header_first_change == SIZE_MAX) {
ctx->header_first_change =
line_start_pos;
}
continue;
}
buffer_append(ctx->header, hdr->full_value,
hdr->full_value_len);
} else {
ctx->sync_ctx->mbox->md5_v.more(mbox_md5_ctx, hdr);
buffer_append(ctx->header, hdr->value,
hdr->value_len);
}
if (!hdr->no_newline) {
if (hdr->crlf_newline)
str_append_c(ctx->header, '\r');
str_append_c(ctx->header, '\n');
}
}
i_assert(ret != 0);
message_parse_header_deinit(&hdr_ctx);
ctx->sync_ctx->mbox->md5_v.finish(mbox_md5_ctx, ctx->hdr_md5_sum);
if ((ctx->seq == 1 && !ctx->seen_imapbase) ||
(ctx->seq > 1 && sync_ctx->dest_first_mail)) {
ctx->need_rewrite = TRUE;
if (sync_ctx->base_uid_validity == 0) {
sync_ctx->base_uid_validity =
sync_ctx->hdr->uid_validity != 0 &&
!sync_ctx->renumber_uids ?
sync_ctx->hdr->uid_validity :
I_MAX(ioloop_time32, 1);
}
}
ctx->body_offset = input->v_offset;
if (input->stream_errno != 0) {
mbox_sync_set_critical(ctx->sync_ctx, "read(%s) failed: %s",
i_stream_get_name(input), i_stream_get_error(input));
return -1;
}
return 0;
}
bool mbox_sync_parse_match_mail(struct mbox_mailbox *mbox,
struct mail_index_view *view, uint32_t seq)
{
struct mbox_sync_mail_context ctx;
struct message_header_parser_ctx *hdr_ctx;
struct message_header_line *hdr;
struct header_func *func;
struct mbox_md5_context *mbox_md5_ctx;
const void *data;
bool expunged;
uint32_t uid;
int ret;
mail_index_lookup_uid(view, seq, &uid);
i_zero(&ctx);
mbox_md5_ctx = mbox->md5_v.init();
hdr_ctx = message_parse_header_init(mbox->mbox_stream, NULL, 0);
while ((ret = message_parse_header_next(hdr_ctx, &hdr)) > 0) {
if (hdr->eoh)
break;
func = bsearch(hdr->name, header_funcs,
N_ELEMENTS(header_funcs), sizeof(*header_funcs),
mbox_sync_bsearch_header_func_cmp);
if (func != NULL) {
if (strcasecmp(hdr->name, "X-UID") == 0) {
if (hdr->continues) {
hdr->use_full_value = TRUE;
continue;
}
(void)parse_x_uid(&ctx, hdr);
if (ctx.mail.uid == uid)
break;
}
} else {
mbox->md5_v.more(mbox_md5_ctx, hdr);
}
}
i_assert(ret != 0);
message_parse_header_deinit(&hdr_ctx);
mbox->md5_v.finish(mbox_md5_ctx, ctx.hdr_md5_sum);
if (ctx.mail.uid == uid)
return TRUE;
mbox->mbox_save_md5 = TRUE;
mail_index_lookup_ext(view, seq, mbox->md5hdr_ext_idx,
&data, &expunged);
return data == NULL ? 0 :
memcmp(data, ctx.hdr_md5_sum, 16) == 0;
}