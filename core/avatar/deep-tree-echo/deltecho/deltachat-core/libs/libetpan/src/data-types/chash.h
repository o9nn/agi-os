#ifndef CHASH_H
#define CHASH_H
#ifdef __cplusplus
extern "C" {
#endif
#ifndef LIBETPAN_CONFIG_H
# include <libetpan/libetpan-config.h>
#endif
typedef struct {
void * data;
unsigned int len;
} chashdatum;
struct chash {
unsigned int size;
unsigned int count;
int copyvalue;
int copykey;
struct chashcell ** cells;
};
typedef struct chash chash;
struct chashcell {
unsigned int func;
chashdatum key;
chashdatum value;
struct chashcell * next;
};
typedef struct chashcell chashiter;
#define CHASH_COPYNONE 0
#define CHASH_COPYKEY 1
#define CHASH_COPYVALUE 2
#define CHASH_COPYALL (CHASH_COPYKEY | CHASH_COPYVALUE)
#define CHASH_DEFAULTSIZE 13
LIBETPAN_EXPORT
chash * chash_new(unsigned int size, int flags);
LIBETPAN_EXPORT
void chash_free(chash * hash);
LIBETPAN_EXPORT
void chash_clear(chash * hash);
LIBETPAN_EXPORT
int chash_set(chash * hash,
chashdatum * key,
chashdatum * value,
chashdatum * oldvalue);
LIBETPAN_EXPORT
int chash_get(chash * hash,
chashdatum * key, chashdatum * result);
LIBETPAN_EXPORT
int chash_delete(chash * hash,
chashdatum * key,
chashdatum * oldvalue);
LIBETPAN_EXPORT
int chash_resize(chash * hash, unsigned int size);
LIBETPAN_EXPORT
chashiter * chash_begin(chash * hash);
LIBETPAN_EXPORT
chashiter * chash_next(chash * hash, chashiter * iter);
#ifdef NO_MACROS
LIBETPAN_EXPORT
unsigned int chash_size(chash * hash);
LIBETPAN_EXPORT
unsigned int chash_count(chash * hash);
LIBETPAN_EXPORT
void chash_key(chashiter * iter, chashdatum * result);
LIBETPAN_EXPORT
void chash_value(chashiter * iter, chashdatum * result);
#else
#ifndef INLINE
#ifdef _MSC_VER
#define INLINE __inline
#else
#define INLINE inline
#endif
#endif
static INLINE unsigned int chash_size(chash * hash)
{
return hash->size;
}
static INLINE unsigned int chash_count(chash * hash)
{
return hash->count;
}
static INLINE void chash_key(chashiter * iter, chashdatum * result)
{
* result = iter->key;
}
static INLINE void chash_value(chashiter * iter, chashdatum * result)
{
* result = iter->value;
}
#endif
#ifdef __cplusplus
}
#endif
#endif