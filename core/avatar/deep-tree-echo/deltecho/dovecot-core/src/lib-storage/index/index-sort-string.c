#include "lib.h"
#include "array.h"
#include "str.h"
#include "index-storage.h"
#include "index-sort-private.h"
struct mail_sort_node {
uint32_t seq:29;
bool wanted:1;
bool no_update:1;
bool sort_id_changed:1;
uint32_t sort_id;
};
ARRAY_DEFINE_TYPE(mail_sort_node, struct mail_sort_node);
struct sort_string_context {
struct mail_search_sort_program *program;
const char *primary_sort_name;
ARRAY_TYPE(mail_sort_node) zero_nodes, nonzero_nodes, sorted_nodes;
const char **sort_strings;
pool_t sort_string_pool;
unsigned int first_missing_sort_id_idx;
uint32_t ext_id, last_seq, highest_reset_id, prev_seq;
uint32_t lowest_nonexpunged_zero;
bool regetting:1;
bool have_all_wanted:1;
bool no_writing:1;
bool reverse:1;
bool seqs_nonsorted:1;
bool broken:1;
bool failed:1;
};
static struct sort_string_context *static_zero_cmp_context;
static void index_sort_list_reset_broken(struct sort_string_context *ctx,
const char *reason);
static void index_sort_node_add(struct sort_string_context *ctx,
struct mail_sort_node *node);
void index_sort_list_init_string(struct mail_search_sort_program *program)
{
struct sort_string_context *ctx;
const char *name;
switch (program->sort_program[0] & MAIL_SORT_MASK) {
case MAIL_SORT_CC:
name = "sort-c";
break;
case MAIL_SORT_FROM:
name = "sort-f";
break;
case MAIL_SORT_SUBJECT:
name = "sort-s";
break;
case MAIL_SORT_TO:
name = "sort-t";
break;
case MAIL_SORT_DISPLAYFROM:
name = "sort-df";
break;
case MAIL_SORT_DISPLAYTO:
name = "sort-dt";
break;
default:
i_unreached();
}
program->context = ctx = i_new(struct sort_string_context, 1);
ctx->reverse = (program->sort_program[0] & MAIL_SORT_FLAG_REVERSE) != 0;
ctx->program = program;
ctx->primary_sort_name = name;
ctx->ext_id = mail_index_ext_register(program->t->box->index, name, 0,
sizeof(uint32_t),
sizeof(uint32_t));
i_array_init(&ctx->zero_nodes, 128);
i_array_init(&ctx->nonzero_nodes, 128);
}
static int sort_node_seq_cmp(const struct mail_sort_node *n1,
const struct mail_sort_node *n2)
{
if (n1->seq < n2->seq)
return -1;
if (n1->seq > n2->seq)
return 1;
return 0;
}
static void index_sort_generate_seqs(struct sort_string_context *ctx)
{
const struct mail_sort_node *nodes, *nodes2;
unsigned int i, j, count, count2;
uint32_t seq;
nodes = array_get(&ctx->nonzero_nodes, &count);
nodes2 = array_get(&ctx->zero_nodes, &count2);
if (!array_is_created(&ctx->program->seqs))
i_array_init(&ctx->program->seqs, count + count2);
else
array_clear(&ctx->program->seqs);
for (i = j = 0;;) {
if (i < count && j < count2) {
if (nodes[i].seq < nodes2[j].seq)
seq = nodes[i++].seq;
else
seq = nodes2[j++].seq;
} else if (i < count) {
seq = nodes[i++].seq;
} else if (j < count2) {
seq = nodes2[j++].seq;
} else {
break;
}
array_push_back(&ctx->program->seqs, &seq);
}
}
static void index_sort_reget_sort_ids(struct sort_string_context *ctx)
{
struct mail_sort_node node;
const uint32_t *seqs;
unsigned int i, count;
i_assert(!ctx->regetting);
ctx->regetting = TRUE;
index_sort_generate_seqs(ctx);
array_clear(&ctx->zero_nodes);
array_clear(&ctx->nonzero_nodes);
i_zero(&node);
node.wanted = TRUE;
seqs = array_get(&ctx->program->seqs, &count);
for (i = 0; i < count; i++) {
node.seq = seqs[i];
index_sort_node_add(ctx, &node);
}
ctx->regetting = FALSE;
}
static void index_sort_node_add(struct sort_string_context *ctx,
struct mail_sort_node *node)
{
struct mail_index_map *map;
const void *data;
uint32_t reset_id;
bool expunged;
mail_index_lookup_ext_full(ctx->program->t->view, node->seq,
ctx->ext_id, &map, &data, &expunged);
if (expunged) {
node->no_update = TRUE;
node->sort_id = 0;
} else {
node->sort_id = ctx->broken || data == NULL ? 0 :
*(const uint32_t *)data;
if (node->sort_id == 0) {
if (ctx->lowest_nonexpunged_zero > node->seq ||
ctx->lowest_nonexpunged_zero == 0)
ctx->lowest_nonexpunged_zero = node->seq;
} else if (ctx->lowest_nonexpunged_zero != 0 &&
ctx->lowest_nonexpunged_zero <= node->seq) {
uint32_t nonzero_uid, zero_uid;
mail_index_lookup_uid(ctx->program->t->view,
node->seq, &nonzero_uid);
mail_index_lookup_uid(ctx->program->t->view,
ctx->lowest_nonexpunged_zero, &zero_uid);
index_sort_list_reset_broken(ctx, t_strdup_printf(
"sort_id=0 found in the middle "
"(uid=%u has sort_id, uid=%u doesn't)",
nonzero_uid, zero_uid));
ctx->broken = TRUE;
node->sort_id = 0;
}
}
if (node->sort_id != 0) {
if (!mail_index_ext_get_reset_id(ctx->program->t->view, map,
ctx->ext_id, &reset_id))
reset_id = 0;
if (reset_id != ctx->highest_reset_id) {
if (reset_id < ctx->highest_reset_id) {
i_assert(expunged);
node->sort_id = 0;
} else if (ctx->have_all_wanted) {
node->sort_id = 0;
ctx->no_writing = TRUE;
} else {
ctx->highest_reset_id = reset_id;
index_sort_reget_sort_ids(ctx);
}
}
}
if (node->sort_id == 0)
array_push_back(&ctx->zero_nodes, node);
else
array_push_back(&ctx->nonzero_nodes, node);
if (ctx->last_seq < node->seq)
ctx->last_seq = node->seq;
}
void index_sort_list_add_string(struct mail_search_sort_program *program,
struct mail *mail)
{
struct sort_string_context *ctx = program->context;
struct mail_sort_node node;
i_zero(&node);
node.seq = mail->seq;
node.wanted = TRUE;
if (mail->seq < ctx->prev_seq)
ctx->seqs_nonsorted = TRUE;
ctx->prev_seq = mail->seq;
index_sort_node_add(ctx, &node);
}
static int sort_node_zero_string_cmp(const struct mail_sort_node *n1,
const struct mail_sort_node *n2)
{
struct sort_string_context *ctx = static_zero_cmp_context;
int ret;
ret = strcmp(ctx->sort_strings[n1->seq], ctx->sort_strings[n2->seq]);
if (ret != 0)
return !ctx->reverse ? ret : -ret;
return index_sort_node_cmp_type(ctx->program,
ctx->program->sort_program + 1,
n1->seq, n2->seq);
}
static void index_sort_zeroes(struct sort_string_context *ctx)
{
enum mail_sort_type sort_type = ctx->program->sort_program[0];
string_t *str;
pool_t pool;
struct mail_sort_node *nodes;
unsigned int i, count;
ctx->sort_strings = i_new(const char *, ctx->last_seq + 1);
ctx->sort_string_pool = pool =
pool_alloconly_create("sort strings", 1024*64);
str = str_new(default_pool, 512);
nodes = array_get_modifiable(&ctx->zero_nodes, &count);
for (i = 0; i < count; i++) {
i_assert(nodes[i].seq <= ctx->last_seq);
T_BEGIN {
if (index_sort_header_get(ctx->program, nodes[i].seq,
sort_type, str) < 0) {
nodes[i].no_update = TRUE;
ctx->failed = TRUE;
}
ctx->sort_strings[nodes[i].seq] =
str_len(str) == 0 ? "" :
p_strdup(pool, str_c(str));
} T_END;
}
str_free(&str);
static_zero_cmp_context = ctx;
array_sort(&ctx->zero_nodes, sort_node_zero_string_cmp);
}
static bool
index_sort_get_expunged_string(struct sort_string_context *ctx, uint32_t idx,
string_t *str, const char **result_r)
{
enum mail_sort_type sort_type = ctx->program->sort_program[0];
const struct mail_sort_node *nodes;
const char *result = NULL;
unsigned int i, count;
uint32_t sort_id;
nodes = array_get(&ctx->nonzero_nodes, &count);
sort_id = nodes[idx].sort_id;
if (idx > 0 && nodes[idx-1].sort_id == sort_id &&
ctx->sort_strings[nodes[idx].seq] != NULL) {
*result_r = ctx->sort_strings[nodes[idx].seq];
return TRUE;
}
for (i = idx + 1; i < count; i++) {
if (nodes[i].sort_id != sort_id)
break;
if (ctx->sort_strings[nodes[i].seq] != NULL) {
result = ctx->sort_strings[nodes[i].seq];
break;
}
if (index_sort_header_get(ctx->program, nodes[i].seq,
sort_type, str) > 0) {
result = str_len(str) == 0 ? "" :
p_strdup(ctx->sort_string_pool, str_c(str));
break;
}
}
if (result == NULL) {
return FALSE;
}
for (i = idx; i > 0 && nodes[i-1].sort_id == sort_id; i--) ;
for (i = idx; i < count && nodes[i].sort_id == sort_id; i++)
ctx->sort_strings[nodes[i].seq] = result;
*result_r = result;
return TRUE;
}
static bool
index_sort_get_string(struct sort_string_context *ctx,
uint32_t idx, struct mail_sort_node *node,
const char **str_r)
{
uint32_t seq = node->seq;
int ret = 1;
if (node->no_update) {
*str_r = ctx->sort_strings[seq];
return FALSE;
}
if (ctx->sort_strings[seq] == NULL) T_BEGIN {
string_t *str;
const char *result;
str = t_str_new(256);
ret = index_sort_header_get(ctx->program, seq,
ctx->program->sort_program[0], str);
if (ret < 0)
ctx->failed = TRUE;
else if (ret == 0) {
if (!index_sort_get_expunged_string(ctx, idx, str, &result))
ctx->sort_strings[seq] = "";
else {
ctx->sort_strings[seq] = result;
ret = 1;
}
} else {
ctx->sort_strings[seq] = str_len(str) == 0 ? "" :
p_strdup(ctx->sort_string_pool, str_c(str));
}
} T_END;
if (ret <= 0)
node->no_update = TRUE;
*str_r = ctx->sort_strings[seq];
return ret > 0;
}
static void
index_sort_bsearch(struct sort_string_context *ctx, const char *key,
unsigned int start_idx, unsigned int *idx_r,
const char **prev_str_r)
{
struct mail_sort_node *nodes;
const char *str, *str2;
unsigned int idx, left_idx, right_idx, prev;
int ret;
nodes = array_get_modifiable(&ctx->nonzero_nodes, &right_idx);
i_assert(right_idx < INT_MAX);
idx = left_idx = start_idx;
while (left_idx < right_idx) {
idx = (left_idx + right_idx) / 2;
if (index_sort_get_string(ctx, idx, &nodes[idx], &str))
ret = strcmp(key, str);
else {
ret = 1;
for (prev = idx; prev > 0; ) {
prev--;
if (index_sort_get_string(ctx, prev,
&nodes[prev],
&str2)) {
ret = strcmp(key, str2);
if (ret <= 0) {
idx = prev;
str = str2;
}
break;
}
}
}
if (ret > 0)
left_idx = idx+1;
else if (ret < 0)
right_idx = idx;
else {
*idx_r = idx + 1;
*prev_str_r = str;
return;
}
}
if (left_idx > idx)
idx++;
*idx_r = idx;
if (idx > start_idx) {
bool success;
prev = idx;
do {
prev--;
success = index_sort_get_string(ctx, prev,
&nodes[prev], &str2);
} while (!success && prev > 0 &&
nodes[prev-1].sort_id == nodes[prev].sort_id);
*prev_str_r = str2;
}
}
static void index_sort_merge(struct sort_string_context *ctx)
{
const struct mail_sort_node *znodes;
struct mail_sort_node *nznodes;
const char *zstr, *nzstr, *prev_str;
unsigned int zpos, nzpos, nz_next_pos, zcount, nzcount;
int ret;
i_array_init(&ctx->sorted_nodes, array_count(&ctx->nonzero_nodes) +
array_count(&ctx->zero_nodes));
znodes = array_get(&ctx->zero_nodes, &zcount);
nznodes = array_get_modifiable(&ctx->nonzero_nodes, &nzcount);
prev_str = NULL;
for (zpos = nzpos = 0; zpos < zcount && nzpos < nzcount; ) {
zstr = ctx->sort_strings[znodes[zpos].seq];
if (index_sort_get_string(ctx, nzpos, &nznodes[nzpos], &nzstr))
ret = strcmp(zstr, nzstr);
else if (prev_str != NULL && strcmp(zstr, prev_str) == 0) {
ret = -1;
} else {
ret = 1;
}
if (ret == 0) {
ret = index_sort_node_cmp_type(ctx->program,
ctx->program->sort_program + 1,
znodes[zpos].seq, nznodes[nzpos].seq);
}
if (ret <= 0) {
array_push_back(&ctx->sorted_nodes, &znodes[zpos]);
prev_str = zstr;
zpos++;
} else {
array_push_back(&ctx->sorted_nodes, &nznodes[nzpos]);
prev_str = nzstr;
nzpos++;
if (zcount - zpos < (nzcount - nzpos)/2) {
index_sort_bsearch(ctx, zstr, nzpos,
&nz_next_pos, &prev_str);
array_append(&ctx->sorted_nodes,
&nznodes[nzpos],
nz_next_pos - nzpos);
nzpos = nz_next_pos;
}
}
}
for (; zpos < zcount; zpos++)
array_push_back(&ctx->sorted_nodes, &znodes[zpos]);
for (; nzpos < nzcount; nzpos++)
array_push_back(&ctx->sorted_nodes, &nznodes[nzpos]);
array_free(&ctx->nonzero_nodes);
ctx->nonzero_nodes = ctx->sorted_nodes;
}
static int
index_sort_add_ids_range(struct sort_string_context *ctx,
unsigned int left_idx, unsigned int right_idx,
const char **reason_r)
{
struct mail_sort_node *nodes;
unsigned int i, count, rightmost_idx, skip;
const char *left_str = NULL, *right_str = NULL, *str = NULL;
uint32_t left_sort_id, right_sort_id, diff, left_str_idx = 0;
bool no_left_str = FALSE, no_right_str = FALSE;
int ret;
nodes = array_get_modifiable(&ctx->sorted_nodes, &count);
rightmost_idx = count - 1;
left_sort_id = nodes[left_idx].sort_id;
right_sort_id = nodes[right_idx].sort_id;
if (left_sort_id == right_sort_id && left_sort_id != 0) {
for (i = left_idx + 1; i < right_idx; i++) {
nodes[i].sort_id = left_sort_id;
nodes[i].sort_id_changed = TRUE;
}
return 0;
}
if (left_sort_id == 0) {
i_assert(left_idx == 0);
left_sort_id = 1;
}
if (right_sort_id == 0) {
i_assert(right_idx == rightmost_idx);
right_sort_id = (uint32_t)-1;
}
i_assert(left_sort_id <= right_sort_id);
diff = right_sort_id - left_sort_id;
while (diff / (right_idx-left_idx + 2) == 0) {
while (left_idx > 0) {
if (nodes[--left_idx].sort_id != left_sort_id) {
left_sort_id = nodes[left_idx].sort_id;
if (left_sort_id == 0) {
i_assert(left_idx == 0);
left_sort_id = 1;
}
break;
}
}
while (right_idx < rightmost_idx) {
right_idx++;
if (nodes[right_idx].sort_id > right_sort_id)
break;
}
right_sort_id = nodes[right_idx].sort_id;
if (right_sort_id == 0) {
i_assert(right_idx == rightmost_idx);
right_sort_id = (uint32_t)-1;
}
i_assert(left_sort_id <= right_sort_id);
if (diff == right_sort_id - left_sort_id) {
i_assert(left_idx == 0 && right_idx == rightmost_idx);
if (left_sort_id > 1) {
left_sort_id = 1;
no_left_str = TRUE;
} else {
i_assert(right_sort_id != (uint32_t)-1);
right_sort_id = (uint32_t)-1;
no_right_str = TRUE;
}
}
diff = right_sort_id - left_sort_id;
}
if (nodes[left_idx].sort_id != 0 && !no_left_str) {
if (!index_sort_get_string(ctx, left_idx,
&nodes[left_idx], &left_str)) {
left_str = NULL;
} else {
left_str_idx = left_idx;
}
left_idx++;
}
if (nodes[right_idx].sort_id != 0 && !no_right_str) {
if (!index_sort_get_string(ctx, right_idx,
&nodes[right_idx], &right_str)) {
right_str = NULL;
}
right_idx--;
}
i_assert(left_idx <= right_idx);
for (i = left_idx; i <= right_idx; i++) {
if (!index_sort_get_string(ctx, i, &nodes[i], &str)) {
nodes[i].sort_id = left_sort_id;
continue;
}
ret = left_str == NULL ? 1 : strcmp(str, left_str);
if (ret <= 0) {
if (ret < 0) {
uint32_t str_uid, left_str_uid;
mail_index_lookup_uid(ctx->program->t->view,
nodes[i].seq, &str_uid);
mail_index_lookup_uid(ctx->program->t->view,
nodes[left_str_idx].seq, &left_str_uid);
*reason_r = t_strdup_printf(
"(idx=%u, seq=%u, uid=%u) '%s' < left string (idx=%u, seq=%u, uid=%u) '%s'",
i, nodes[i].seq, str_uid, str,
left_str_idx, nodes[left_str_idx].seq, left_str_uid, left_str);
return -1;
}
nodes[i].sort_id = left_sort_id;
} else if (right_str != NULL && strcmp(str, right_str) == 0) {
nodes[i].sort_id = right_sort_id;
left_sort_id = right_sort_id;
} else {
skip = (right_sort_id - left_sort_id) /
(right_idx - i + 2);
if (skip == 0) {
uint32_t uid;
mail_index_lookup_uid(ctx->program->t->view,
nodes[i].seq, &uid);
*reason_r = t_strdup_printf(
"no sort_id space for uid=%u", uid);
return -1;
}
left_sort_id += skip;
i_assert(left_sort_id < right_sort_id);
nodes[i].sort_id = left_sort_id;
left_str = str;
left_str_idx = i;
}
nodes[i].sort_id_changed = TRUE;
}
i_assert(str != NULL);
if (right_str == NULL || strcmp(str, right_str) < 0 ||
(strcmp(str, right_str) == 0 &&
nodes[i-1].sort_id == right_sort_id))
return 0;
*reason_r = t_strdup_printf("Invalid sort_id order ('%s' > '%s')",
str, right_str);
return -1;
}
static int
index_sort_add_sort_ids(struct sort_string_context *ctx, const char **reason_r)
{
const struct mail_sort_node *nodes;
unsigned int i, left_idx, right_idx, count;
nodes = array_get(&ctx->sorted_nodes, &count);
for (i = 0; i < count; i++) {
if (nodes[i].sort_id != 0)
continue;
for (right_idx = i + 1; right_idx < count; right_idx++) {
if (nodes[right_idx].sort_id != 0)
break;
}
if (right_idx == count)
right_idx--;
left_idx = i == 0 ? 0 : i - 1;
if (index_sort_add_ids_range(ctx, left_idx, right_idx, reason_r) < 0)
return -1;
}
return 0;
}
static void index_sort_write_changed_sort_ids(struct sort_string_context *ctx)
{
struct mail_index_transaction *itrans = ctx->program->t->itrans;
uint32_t ext_id = ctx->ext_id;
const struct mail_sort_node *nodes;
unsigned int i, count;
uint32_t lowest_failed_seq;
if (ctx->no_writing) {
return;
}
mail_index_ext_reset_inc(itrans, ext_id,
ctx->highest_reset_id, FALSE);
nodes = array_get(&ctx->sorted_nodes, &count);
lowest_failed_seq = (uint32_t)-1;
for (i = 0; i < count; i++) {
uint32_t seq = nodes[i].seq;
if (nodes[i].no_update && lowest_failed_seq > seq &&
!mail_index_is_expunged(ctx->program->t->view, seq))
lowest_failed_seq = seq;
}
ARRAY_TYPE(mail_sort_node) sorted_by_seq;
i_array_init(&sorted_by_seq, array_count(&ctx->sorted_nodes));
nodes = array_get(&ctx->sorted_nodes, &count);
for (i = 0; i < count; i++) {
i_assert(nodes[i].sort_id != 0);
if (!nodes[i].sort_id_changed || nodes[i].no_update ||
nodes[i].seq >= lowest_failed_seq)
continue;
array_push_back(&sorted_by_seq, &nodes[i]);
}
array_sort(&sorted_by_seq, sort_node_seq_cmp);
nodes = array_get(&sorted_by_seq, &count);
for (i = 0; i < count; i++) {
mail_index_update_ext(itrans, nodes[i].seq, ext_id,
&nodes[i].sort_id, NULL);
}
array_free(&sorted_by_seq);
}
static int sort_node_cmp(const struct mail_sort_node *n1,
const struct mail_sort_node *n2)
{
struct sort_string_context *ctx = static_zero_cmp_context;
if (n1->sort_id < n2->sort_id)
return !ctx->reverse ? -1 : 1;
if (n1->sort_id > n2->sort_id)
return !ctx->reverse ? 1 : -1;
return index_sort_node_cmp_type(ctx->program,
ctx->program->sort_program + 1,
n1->seq, n2->seq);
}
static void index_sort_add_missing(struct sort_string_context *ctx)
{
struct mail_sort_node node;
const uint32_t *seqs;
unsigned int i, count;
uint32_t seq, next_seq;
ctx->have_all_wanted = TRUE;
seqs = array_get(&ctx->program->seqs, &count);
for (i = 0, next_seq = 1; i < count; i++) {
if (seqs[i] == next_seq)
next_seq++;
else {
i_assert(next_seq < seqs[i]);
for (seq = next_seq; seq < seqs[i]; seq++) {
i_zero(&node);
node.seq = seq;
index_sort_node_add(ctx, &node);
}
next_seq = seqs[i] + 1;
}
}
if (ctx->lowest_nonexpunged_zero == 0) {
ctx->no_writing = TRUE;
}
}
static void index_sort_list_reset_broken(struct sort_string_context *ctx,
const char *reason)
{
struct mailbox *box = ctx->program->t->box;
struct mail_sort_node *node;
mailbox_set_critical(box, "Broken %s indexes, resetting: %s",
ctx->primary_sort_name, reason);
array_clear(&ctx->zero_nodes);
array_append_array(&ctx->zero_nodes,
&ctx->nonzero_nodes);
array_clear(&ctx->nonzero_nodes);
array_foreach_modifiable(&ctx->zero_nodes, node)
node->sort_id = 0;
}
void index_sort_list_finish_string(struct mail_search_sort_program *program)
{
struct sort_string_context *ctx = program->context;
const struct mail_sort_node *nodes;
unsigned int i, count;
const char *reason;
uint32_t seq;
static_zero_cmp_context = ctx;
if (array_count(&ctx->zero_nodes) == 0) {
array_sort(&ctx->nonzero_nodes, sort_node_cmp);
nodes = array_get(&ctx->nonzero_nodes, &count);
if (!array_is_created(&program->seqs))
i_array_init(&program->seqs, count);
else
array_clear(&program->seqs);
for (i = 0; i < count; i++) {
seq = nodes[i].seq;
array_push_back(&program->seqs, &seq);
}
array_free(&ctx->nonzero_nodes);
} else {
if (ctx->seqs_nonsorted) {
array_sort(&ctx->zero_nodes, sort_node_seq_cmp);
array_sort(&ctx->nonzero_nodes, sort_node_seq_cmp);
}
index_sort_generate_seqs(ctx);
index_sort_add_missing(ctx);
array_sort(&ctx->nonzero_nodes, sort_node_cmp);
for (;;) {
index_sort_zeroes(ctx);
if (ctx->reverse) {
array_reverse(&ctx->nonzero_nodes);
array_reverse(&ctx->zero_nodes);
}
index_sort_merge(ctx);
if (index_sort_add_sort_ids(ctx, &reason) == 0)
break;
index_sort_list_reset_broken(ctx, reason);
}
index_sort_write_changed_sort_ids(ctx);
if (ctx->reverse) {
array_reverse(&ctx->sorted_nodes);
}
nodes = array_get(&ctx->sorted_nodes, &count);
array_clear(&program->seqs);
for (i = 0; i < count; i++) {
if (nodes[i].wanted) {
seq = nodes[i].seq;
array_push_back(&program->seqs, &seq);
}
}
pool_unref(&ctx->sort_string_pool);
i_free(ctx->sort_strings);
array_free(&ctx->sorted_nodes);
}
if (ctx->failed)
program->failed = TRUE;
array_free(&ctx->zero_nodes);
i_free(ctx);
program->context = NULL;
}