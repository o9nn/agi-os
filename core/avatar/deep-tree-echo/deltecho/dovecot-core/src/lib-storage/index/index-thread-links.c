#include "lib.h"
#include "array.h"
#include "message-id.h"
#include "mail-storage.h"
#include "index-thread-private.h"
static uint32_t thread_msg_add(struct mail_thread_cache *cache,
uint32_t uid, uint32_t msgid_idx)
{
struct mail_thread_node *node;
i_assert(msgid_idx != 0);
i_assert(msgid_idx < cache->first_invalid_msgid_str_idx);
node = array_idx_get_space(&cache->thread_nodes, msgid_idx);
if (node->uid == 0)
node->uid = uid;
else {
node->expunge_rebuilds = TRUE;
msgid_idx = cache->next_invalid_msgid_str_idx++;
node = array_idx_get_space(&cache->thread_nodes, msgid_idx);
node->uid = uid;
}
return msgid_idx;
}
static bool thread_node_has_ancestor(struct mail_thread_cache *cache,
const struct mail_thread_node *node,
const struct mail_thread_node *ancestor)
{
while (node != ancestor) {
if (node->parent_idx == 0)
return FALSE;
node = array_idx(&cache->thread_nodes, node->parent_idx);
}
return TRUE;
}
static void thread_link_reference(struct mail_thread_cache *cache,
uint32_t parent_idx, uint32_t child_idx)
{
struct mail_thread_node *node, *parent, *child;
uint32_t idx;
i_assert(parent_idx < cache->first_invalid_msgid_str_idx);
if (child_idx < parent_idx) {
parent = array_idx_get_space(&cache->thread_nodes, parent_idx);
child = array_idx_modifiable(&cache->thread_nodes, child_idx);
} else {
child = array_idx_get_space(&cache->thread_nodes, child_idx);
parent = array_idx_modifiable(&cache->thread_nodes, parent_idx);
}
child->parent_link_refcount++;
if (thread_node_has_ancestor(cache, parent, child)) {
if (parent == child) {
return;
}
node = parent;
do {
idx = node->parent_idx;
i_assert(idx != 0);
node = array_idx_modifiable(&cache->thread_nodes, idx);
node->child_unref_rebuilds = TRUE;
} while (node != child);
return;
} else if (child->parent_idx == parent_idx) {
return;
}
if (child->parent_idx == 0) {
child->parent_idx = parent_idx;
} else {
if (MAIL_THREAD_NODE_EXISTS(child)) {
child->expunge_rebuilds = TRUE;
} else {
child->child_unref_rebuilds = TRUE;
}
}
}
static uint32_t
thread_link_references(struct mail_thread_cache *cache, uint32_t uid,
const struct mail_index_strmap_rec *msgid_map,
unsigned int *msgid_map_idx)
{
uint32_t parent_idx;
if (msgid_map->uid != uid)
return 0;
parent_idx = msgid_map->str_idx;
msgid_map++;
*msgid_map_idx += 1;
for (; msgid_map->uid == uid; msgid_map++) {
thread_link_reference(cache, parent_idx, msgid_map->str_idx);
parent_idx = msgid_map->str_idx;
*msgid_map_idx += 1;
}
i_assert(parent_idx < cache->first_invalid_msgid_str_idx);
return parent_idx;
}
void mail_thread_add(struct mail_thread_cache *cache,
const struct mail_index_strmap_rec *msgid_map,
unsigned int *msgid_map_idx)
{
struct mail_thread_node *node;
uint32_t idx, parent_idx;
i_assert(msgid_map->ref_index == MAIL_THREAD_NODE_REF_MSGID);
i_assert(cache->last_uid <= msgid_map->uid);
cache->last_uid = msgid_map->uid;
idx = thread_msg_add(cache, msgid_map->uid, msgid_map->str_idx);
parent_idx = thread_link_references(cache, msgid_map->uid,
msgid_map + 1, msgid_map_idx);
node = array_idx_modifiable(&cache->thread_nodes, idx);
if (node->parent_idx != parent_idx && node->parent_idx != 0) {
node->parent_idx = 0;
node->expunge_rebuilds = TRUE;
}
if (parent_idx != 0)
thread_link_reference(cache, parent_idx, idx);
*msgid_map_idx += 1;
}
static bool
mail_thread_unref_link(struct mail_thread_cache *cache,
uint32_t parent_idx, uint32_t child_idx)
{
struct mail_thread_node *parent, *child;
parent = array_idx_modifiable(&cache->thread_nodes, parent_idx);
if (parent->child_unref_rebuilds)
return FALSE;
child = array_idx_modifiable(&cache->thread_nodes, child_idx);
i_assert(child->parent_link_refcount > 0);
child->parent_link_refcount--;
if (child->parent_link_refcount == 0) {
child->parent_idx = 0;
}
return TRUE;
}
bool mail_thread_remove(struct mail_thread_cache *cache,
const struct mail_index_strmap_rec *msgid_map,
unsigned int *msgid_map_idx)
{
struct mail_thread_node *node;
uint32_t idx, parent_idx;
unsigned int count = 1;
idx = msgid_map->str_idx;
i_assert(idx != 0);
if (msgid_map->uid > cache->last_uid) {
while (msgid_map[count].uid == msgid_map->uid)
count++;
*msgid_map_idx += count;
return TRUE;
}
node = array_idx_modifiable(&cache->thread_nodes, idx);
if (node->expunge_rebuilds) {
return FALSE;
}
i_assert(node->uid == msgid_map->uid);
if (msgid_map[count].uid == node->uid) {
parent_idx = msgid_map[count].str_idx;
count++;
while (msgid_map[count].uid == node->uid) {
if (!mail_thread_unref_link(cache, parent_idx,
msgid_map[count].str_idx))
return FALSE;
parent_idx = msgid_map[count].str_idx;
count++;
}
if (!mail_thread_unref_link(cache, parent_idx, idx))
return FALSE;
}
node->uid = 0;
*msgid_map_idx += count;
return TRUE;
}