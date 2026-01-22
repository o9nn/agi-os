#ifndef ACL_CACHE_H
#define ACL_CACHE_H
struct acl_backend;
struct acl_rights_update;
struct acl_mask {
pool_t pool;
unsigned int size;
unsigned char mask[1];
};
#define SIZEOF_ACL_MASK(bitmask_size) \
(MALLOC_ADD((bitmask_size), sizeof(pool_t) + sizeof(unsigned int)))
struct acl_cache *acl_cache_init(struct acl_backend *backend,
size_t validity_rec_size);
void acl_cache_deinit(struct acl_cache **cache);
struct acl_mask *acl_cache_mask_init(struct acl_cache *cache, pool_t pool,
const char *const *rights);
void acl_cache_mask_deinit(struct acl_mask **mask);
unsigned int acl_cache_right_lookup(struct acl_cache *cache,
const char *right);
void acl_cache_flush(struct acl_cache *cache, const char *objname);
void acl_cache_flush_all(struct acl_cache *cache);
void acl_cache_update(struct acl_cache *cache, const char *objname,
const struct acl_rights_update *update);
void *acl_cache_get_validity(struct acl_cache *cache, const char *objname);
void acl_cache_set_validity(struct acl_cache *cache, const char *objname,
const void *validity);
const char *const *acl_cache_get_names(struct acl_cache *cache,
unsigned int *count_r);
const struct acl_mask *
acl_cache_get_my_rights(struct acl_cache *cache, const char *objname);
bool acl_cache_mask_isset(const struct acl_mask *mask, unsigned int right_idx);
#endif