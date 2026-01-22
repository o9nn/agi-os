#ifndef __DC_HASH_H__
#define __DC_HASH_H__
#ifdef __cplusplus
extern "C"
{
#endif
typedef struct _dc_hash       dc_hash_t;
typedef struct _dc_hashelem   dc_hashelem_t;
struct _dc_hash
{
char              keyClass;
char              copyKey;
int               count;
dc_hashelem_t     *first;
int               htsize;
struct _ht
{
int           count;
dc_hashelem_t *chain;
} *ht;
};
struct _dc_hashelem
{
dc_hashelem_t     *next, *prev;
void*             data;
void*             pKey;
int               nKey;
};
#define DC_HASH_INT       1
#define DC_HASH_POINTER   2
#define DC_HASH_STRING    3
#define DC_HASH_BINARY    4
#define DC_HASH_COPY_KEY  1
void    dc_hash_init     (dc_hash_t*, int keytype, int copyKey);
void*   dc_hash_insert   (dc_hash_t*, const void *pKey, int nKey, void *pData);
void*   dc_hash_find     (const dc_hash_t*, const void *pKey, int nKey);
void    dc_hash_clear    (dc_hash_t*);
#define dc_hash_find_str(H, s) dc_hash_find((H), (s), strlen((s)))
#define dc_hash_insert_str(H, s, d) dc_hash_insert((H), (s), strlen((s)), (d))
#define dc_hash_first(H)      ((H)->first)
#define dc_hash_next(E)       ((E)->next)
#define dc_hash_data(E)       ((E)->data)
#define dc_hash_key(E)        ((E)->pKey)
#define dc_hash_keysize(E)    ((E)->nKey)
#define dc_hash_cnt(H)        ((H)->count)
#ifdef __cplusplus
};
#endif
#endif