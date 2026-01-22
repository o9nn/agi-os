#ifndef gsuid_INCLUDED
# define gsuid_INCLUDED
#ifndef gs_uid_DEFINED
# define gs_uid_DEFINED
typedef struct gs_uid_s gs_uid;
#endif
struct gs_uid_s {
long id;
long *xvalues;
};
#define no_UniqueID max_long
#define uid_is_valid(puid)\
((puid)->id != no_UniqueID)
#define uid_set_invalid(puid)\
((puid)->id = no_UniqueID, (puid)->xvalues = 0)
#define uid_is_UniqueID(puid)\
(((puid)->id & ~0xffffff) == 0)
#define uid_is_XUID(puid)\
((puid)->id < 0)
#define uid_set_UniqueID(puid, idv)\
((puid)->id = idv, (puid)->xvalues = 0)
#define uid_set_XUID(puid, pvalues, siz)\
((puid)->id = -(long)(siz), (puid)->xvalues = pvalues)
#define uid_XUID_size(puid) ((uint)(-(puid)->id))
#define uid_XUID_values(puid) ((puid)->xvalues)
bool uid_equal(const gs_uid *, const gs_uid *);
int uid_copy(gs_uid *puid, gs_memory_t *mem, client_name_t cname);
#define uid_free(puid, mem, cname)\
gs_free_object(mem, (puid)->xvalues, cname)
#endif