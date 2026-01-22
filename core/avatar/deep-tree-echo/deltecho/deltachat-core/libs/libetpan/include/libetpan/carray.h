#ifndef CARRAY_H
#define CARRAY_H
#ifndef LIBETPAN_CONFIG_H
# include <libetpan/libetpan-config.h>
#endif
#ifdef __cplusplus
extern "C" {
#endif
struct carray_s {
void ** array;
unsigned int len;
unsigned int max;
};
typedef struct carray_s carray;
LIBETPAN_EXPORT
carray * carray_new(unsigned int initsize);
LIBETPAN_EXPORT
int carray_add(carray * array, void * data, unsigned int * indx);
LIBETPAN_EXPORT
int carray_set_size(carray * array, unsigned int new_size);
LIBETPAN_EXPORT
int carray_delete(carray * array, unsigned int indx);
LIBETPAN_EXPORT
int carray_delete_slow(carray * array, unsigned int indx);
LIBETPAN_EXPORT
int carray_delete_fast(carray * array, unsigned int indx);
#ifdef NO_MACROS
LIBETPAN_EXPORT
void ** carray_data(carray *);
LIBETPAN_EXPORT
unsigned int carray_count(carray *);
LIBETPAN_EXPORT
void * carray_get(carray * array, unsigned int indx);
LIBETPAN_EXPORT
void carray_set(carray * array, unsigned int indx, void * value);
#else
#if 0
#define carray_data(a) (a->array)
#define carray_count(a) (a->len)
#define carray_get(a, indx) (a->array[indx])
#define carray_set(a, indx, v) do { a->array[indx]=v; } while(0)
#endif
#ifndef INLINE
#ifdef _MSC_VER
#define INLINE __inline
#else
#define INLINE inline
#endif
#endif
static INLINE void ** carray_data(carray * array)
{
return array->array;
}
static INLINE unsigned int carray_count(carray * array)
{
return array->len;
}
static INLINE void * carray_get(carray * array, unsigned int indx)
{
return array->array[indx];
}
static INLINE void carray_set(carray * array, unsigned int indx, void * value)
{
array->array[indx] = value;
}
#endif
LIBETPAN_EXPORT
void carray_free(carray * array);
#ifdef __cplusplus
}
#endif
#endif