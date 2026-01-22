#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include <stdlib.h>
#include <string.h>
#include "carray.h"
#define MIN_ARRAY_SIZE 4
LIBETPAN_EXPORT
carray * carray_new(unsigned int initsize) {
carray * array;
array = (carray *) malloc(sizeof(carray));
if (!array) return NULL;
if (initsize < MIN_ARRAY_SIZE)
initsize = MIN_ARRAY_SIZE;
array->len = 0;
array->max = initsize;
array->array = (void **) malloc(sizeof(void *) * initsize);
if (!array->array) {
free(array);
return NULL;
}
return array;
}
LIBETPAN_EXPORT
int carray_add(carray * array, void * data, unsigned int * indx) {
int r;
r = carray_set_size(array, array->len + 1);
if (r < 0)
return r;
array->array[array->len - 1] = data;
if (indx != NULL)
* indx = array->len - 1;
return 0;
}
LIBETPAN_EXPORT
int carray_set_size(carray * array, unsigned int new_size)
{
if (new_size > array->max) {
unsigned int n = array->max * 2;
void * new;
while (n <= new_size)
n *= 2;
new = (void **) realloc(array->array, sizeof(void *) * n);
if (!new)
return -1;
array->array = new;
array->max = n;
}
array->len = new_size;
return 0;
}
LIBETPAN_EXPORT
int carray_delete_fast(carray * array, unsigned int indx) {
if (indx >= array->len)
return -1;
array->array[indx] = NULL;
return 0;
}
LIBETPAN_EXPORT
int carray_delete(carray * array, unsigned int indx) {
if (indx >= array->len)
return -1;
if (indx != --array->len)
array->array[indx] = array->array[array->len];
return 0;
}
LIBETPAN_EXPORT
int carray_delete_slow(carray * array, unsigned int indx) {
if (indx >= array->len)
return -1;
if (indx != --array->len)
memmove(array->array + indx, array->array + indx + 1,
(array->len - indx) * sizeof(void *));
return 0;
}
#ifdef NO_MACROS
LIBETPAN_EXPORT
void ** carray_data(carray * array) {
return array->array;
}
LIBETPAN_EXPORT
unsigned int carray_count(carray * array) {
return array->len;
}
LIBETPAN_EXPORT
void * carray_get(carray * array, unsigned int indx) {
return array->array[indx];
}
LIBETPAN_EXPORT
void carray_set(carray * array, unsigned int indx, void * value) {
array->array[indx] = value;
}
#endif
LIBETPAN_EXPORT
void carray_free(carray * array) {
free(array->array);
free(array);
}