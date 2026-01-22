#ifndef AUTH_CACHE_H
#define AUTH_CACHE_H
struct auth_cache_node {
struct auth_cache_node *prev, *next;
time_t created;
uint32_t alloc_size:31;
bool last_success:1;
char data[];
};
struct auth_cache;
struct auth_request;
char *auth_cache_parse_key(pool_t pool, const char *query);
struct auth_cache *auth_cache_new(size_t max_size, unsigned int ttl_secs,
unsigned int neg_ttl_secs);
void auth_cache_free(struct auth_cache **cache);
unsigned int ATTR_NOWARN_UNUSED_RESULT
auth_cache_clear(struct auth_cache *cache);
unsigned int auth_cache_clear_users(struct auth_cache *cache,
const char *const *usernames);
const char *
auth_cache_lookup(struct auth_cache *cache, const struct auth_request *request,
const char *key, struct auth_cache_node **node_r,
bool *expired_r, bool *neg_expired_r);
void auth_cache_insert(struct auth_cache *cache, struct auth_request *request,
const char *key, const char *value, bool last_success);
void auth_cache_remove(struct auth_cache *cache,
const struct auth_request *request,
const char *key);
#endif