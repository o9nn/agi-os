#include "lib.h"
#include "ioloop.h"
#include "mail-cache-private.h"
const char *mail_cache_decision_to_string(enum mail_cache_decision_type dec)
{
switch (dec & ENUM_NEGATE(MAIL_CACHE_DECISION_FORCED)) {
case MAIL_CACHE_DECISION_NO:
return "no";
case MAIL_CACHE_DECISION_TEMP:
return "temp";
case MAIL_CACHE_DECISION_YES:
return "yes";
}
i_unreached();
}
struct event_passthrough *
mail_cache_decision_changed_event(struct mail_cache *cache, struct event *event,
unsigned int field)
{
return event_create_passthrough(event)->
set_name("mail_cache_decision_changed")->
add_str("field", cache->fields[field].field.name)->
add_int("last_used", cache->fields[field].field.last_used);
}
static void
mail_cache_update_last_used(struct mail_cache *cache, unsigned int field)
{
cache->fields[field].field.last_used = ioloop_time32;
if (cache->field_file_map[field] != (uint32_t)-1)
cache->field_header_write_pending = TRUE;
}
void mail_cache_decision_state_update(struct mail_cache_view *view,
uint32_t seq, unsigned int field)
{
struct mail_cache *cache = view->cache;
enum mail_cache_decision_type dec;
const struct mail_index_header *hdr;
uint32_t uid;
i_assert(field < cache->fields_count);
if (view->no_decision_updates)
return;
dec = cache->fields[field].field.decision;
if (dec == (MAIL_CACHE_DECISION_NO | MAIL_CACHE_DECISION_FORCED)) {
return;
}
bool last_used_need_update =
ioloop_time - cache->fields[field].field.last_used > 3600*24;
if (dec == MAIL_CACHE_DECISION_NO ||
(dec & MAIL_CACHE_DECISION_FORCED) != 0) {
if (last_used_need_update)
mail_cache_update_last_used(cache, field);
return;
}
if (dec == MAIL_CACHE_DECISION_YES) {
if (!last_used_need_update)
return;
} else {
i_assert(dec == MAIL_CACHE_DECISION_TEMP);
if (last_used_need_update)
mail_cache_update_last_used(cache, field);
}
mail_index_lookup_uid(view->view, seq, &uid);
hdr = mail_index_get_header(view->view);
if (uid >= cache->fields[field].uid_highwater &&
uid >= hdr->day_first_uid[7]) {
cache->fields[field].uid_highwater = uid;
} else if (dec == MAIL_CACHE_DECISION_YES) {
i_assert(last_used_need_update);
mail_cache_update_last_used(cache, field);
} else {
i_assert(dec == MAIL_CACHE_DECISION_TEMP);
cache->fields[field].field.decision = MAIL_CACHE_DECISION_YES;
cache->fields[field].decision_dirty = TRUE;
cache->field_header_write_pending = TRUE;
const char *reason = uid < hdr->day_first_uid[7] ?
"old_mail" : "unordered_access";
struct event_passthrough *e =
mail_cache_decision_changed_event(
view->cache, view->cache->event, field)->
add_str("reason", reason)->
add_int("uid", uid)->
add_str("old_decision", "temp")->
add_str("new_decision", "yes");
e_debug(e->event(), "Changing field %s decision temp -> yes (uid=%u)",
cache->fields[field].field.name, uid);
}
}
static unsigned int mail_cache_count_alive_headers(struct mail_cache *cache)
{
unsigned int count = 0;
for (unsigned int index = 0; index < cache->fields_count; ++index) {
if (cache->fields[index].field.type == MAIL_CACHE_FIELD_HEADER &&
(cache->fields[index].field.decision &
ENUM_NEGATE(MAIL_CACHE_DECISION_FORCED)) !=
MAIL_CACHE_DECISION_NO)
++count;
}
return count;
}
bool mail_cache_headers_check_capped(struct mail_cache *cache)
{
struct mail_index_cache_optimization_settings *set =
&cache->index->optimization_set.cache;
if (set->max_headers_count == 0) return FALSE;
if (cache->headers_capped) return TRUE;
unsigned int count = mail_cache_count_alive_headers(cache);
cache->headers_capped = count >= set->max_headers_count;
return cache->headers_capped;
}
static struct event_passthrough *
mail_cache_decision_rejected_event(struct mail_cache *cache, unsigned int field,
const char *reason)
{
return event_create_passthrough(cache->event)->
set_name("mail_cache_decision_rejected")->
add_str("field", cache->fields[field].field.name)->
add_str("reason", reason);
}
void mail_cache_decision_add(struct mail_cache_view *view, uint32_t seq,
unsigned int field, bool *rejected_r)
{
struct mail_cache *cache = view->cache;
struct mail_cache_field_private *priv;
uint32_t uid;
i_assert(field < cache->fields_count);
*rejected_r = FALSE;
if (view->no_decision_updates)
return;
priv = &cache->fields[field];
if (priv->field.decision != MAIL_CACHE_DECISION_NO &&
priv->field.last_used != 0) {
return;
}
if (priv->field.decision == MAIL_CACHE_DECISION_NO) {
if (mail_cache_headers_check_capped(view->cache)) {
*rejected_r = TRUE;
const char *reason = "too_many_headers";
struct event_passthrough *e =
mail_cache_decision_rejected_event(
cache, field, reason);
e_debug(e->event(),
"Cache rejected header '%s': %s",
priv->field.name, reason);
return;
}
priv->field.decision = MAIL_CACHE_DECISION_TEMP;
}
priv->field.last_used = ioloop_time;
priv->decision_dirty = TRUE;
cache->field_header_write_pending = TRUE;
mail_index_lookup_uid(view->view, seq, &uid);
priv->uid_highwater = uid;
const char *new_decision =
mail_cache_decision_to_string(priv->field.decision);
struct event_passthrough *e =
mail_cache_decision_changed_event(cache, cache->event, field)->
add_str("reason", "add")->
add_int("uid", uid)->
add_str("old_decision", "no")->
add_str("new_decision", new_decision);
e_debug(e->event(), "Adding field %s to cache for the first time (uid=%u)",
priv->field.name, uid);
}
int mail_cache_decisions_copy(struct mail_cache *src, struct mail_cache *dst)
{
if (mail_cache_open_and_verify(src) < 0)
return -1;
if (MAIL_CACHE_IS_UNUSABLE(src))
return 0;
pool_t pool;
unsigned int count = 0;
struct mail_cache_field *fields =
mail_cache_register_get_list(src, &pool, &count);
i_assert(fields != NULL || count == 0);
if (count > 0)
mail_cache_register_fields(dst, fields, count,
unsafe_data_stack_pool);
dst->field_header_write_pending = TRUE;
pool_unref(&pool);
return mail_cache_purge(dst, 0, "copy cache decisions");
}