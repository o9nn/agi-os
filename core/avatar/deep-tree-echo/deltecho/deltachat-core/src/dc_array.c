#include "dc_context.h"
#include "dc_array.h"
#define DC_ARRAY_MAGIC 0x000a11aa
dc_array_t* dc_array_new_typed(dc_context_t* context, int type, size_t initsize)
{
dc_array_t* array = NULL;
array = (dc_array_t*) calloc(1, sizeof(dc_array_t));
if (array==NULL) {
exit(47);
}
array->magic     = DC_ARRAY_MAGIC;
array->context   = context;
array->count     = 0;
array->allocated = initsize<1? 1 : initsize;
array->type      = type;
array->array     = malloc(array->allocated * sizeof(uintptr_t));
if (array->array==NULL) {
exit(48);
}
return array;
}
dc_array_t* dc_array_new(dc_context_t* context, size_t initsize)
{
return dc_array_new_typed(context, 0, initsize);
}
void dc_array_unref(dc_array_t* array)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC) {
return;
}
if (array->type==DC_ARRAY_LOCATIONS) {
dc_array_free_ptr(array);
}
free(array->array);
array->magic = 0;
free(array);
}
void dc_array_free_ptr(dc_array_t* array)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC) {
return;
}
for (size_t i = 0; i<array->count; i++) {
if (array->type==DC_ARRAY_LOCATIONS) {
free(((struct _dc_location*)array->array[i])->marker);
}
free((void*)array->array[i]);
array->array[i] = 0;
}
}
dc_array_t* dc_array_duplicate(const dc_array_t* array)
{
dc_array_t* ret = NULL;
if (array==NULL || array->magic!=DC_ARRAY_MAGIC) {
return NULL;
}
ret = dc_array_new(array->context, array->allocated);
ret->count = array->count;
memcpy(ret->array, array->array, array->count * sizeof(uintptr_t));
return ret;
}
static int cmp_intptr_t(const void* p1, const void* p2)
{
uintptr_t v1 = *(uintptr_t*)p1;
uintptr_t v2 = *(uintptr_t*)p2;
return (v1<v2)? -1 : ((v1>v2)? 1 : 0);
}
void dc_array_sort_ids(dc_array_t* array)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || array->count <= 1) {
return;
}
qsort(array->array, array->count, sizeof(uintptr_t), cmp_intptr_t);
}
static int cmp_strings_t(const void* p1, const void* p2)
{
const char* v1 = *(const char **)p1;
const char* v2 = *(const char **)p2;
return strcmp(v1, v2);
}
void dc_array_sort_strings(dc_array_t* array)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || array->count <= 1) {
return;
}
qsort(array->array, array->count, sizeof(char*), cmp_strings_t);
}
void dc_array_empty(dc_array_t* array)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC) {
return;
}
array->count = 0;
}
void dc_array_add_uint(dc_array_t* array, uintptr_t item)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC) {
return;
}
if (array->count==array->allocated) {
int newsize = (array->allocated * 2) + 10;
if ((array->array=realloc(array->array, newsize*sizeof(uintptr_t)))==NULL) {
exit(49);
}
array->allocated = newsize;
}
array->array[array->count] = item;
array->count++;
}
void dc_array_add_id(dc_array_t* array, uint32_t item)
{
dc_array_add_uint(array, item);
}
void dc_array_add_ptr(dc_array_t* array, void* item)
{
dc_array_add_uint(array, (uintptr_t)item);
}
size_t dc_array_get_cnt(const dc_array_t* array)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC) {
return 0;
}
return array->count;
}
uintptr_t dc_array_get_uint(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count) {
return 0;
}
return array->array[index];
}
uint32_t dc_array_get_id(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count) {
return 0;
}
if (array->type==DC_ARRAY_LOCATIONS) {
return ((struct _dc_location*)array->array[index])->location_id;
}
return (uint32_t)array->array[index];
}
void* dc_array_get_ptr(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count) {
return 0;
}
return (void*)array->array[index];
}
double dc_array_get_latitude(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return ((struct _dc_location*)array->array[index])->latitude;
}
double dc_array_get_longitude(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return ((struct _dc_location*)array->array[index])->longitude;
}
double dc_array_get_accuracy(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return ((struct _dc_location*)array->array[index])->accuracy;
}
time_t dc_array_get_timestamp(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return ((struct _dc_location*)array->array[index])->timestamp;
}
uint32_t dc_array_get_msg_id(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return ((struct _dc_location*)array->array[index])->msg_id;
}
uint32_t dc_array_get_chat_id(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return ((struct _dc_location*)array->array[index])->chat_id;
}
uint32_t dc_array_get_contact_id(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return ((struct _dc_location*)array->array[index])->contact_id;
}
char* dc_array_get_marker(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return dc_strdup_keep_null(((struct _dc_location*)array->array[index])->marker);
}
int dc_array_is_independent(const dc_array_t* array, size_t index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || index>=array->count
|| array->type!=DC_ARRAY_LOCATIONS || array->array[index]==0 ) {
return 0;
}
return ((struct _dc_location*)array->array[index])->independent;
}
int dc_array_search_id(const dc_array_t* array, uint32_t needle, size_t* ret_index)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC) {
return 0;
}
uintptr_t* data = array->array;
size_t i, cnt = array->count;
for (i=0; i<cnt; i++)
{
if (data[i]==needle) {
if (ret_index) {
*ret_index = i;
}
return 1;
}
}
return 0;
}
const uintptr_t* dc_array_get_raw(const dc_array_t* array)
{
if (array==NULL || array->magic!=DC_ARRAY_MAGIC) {
return NULL;
}
return array->array;
}
char* dc_arr_to_string(const uint32_t* arr, int cnt)
{
char*       ret = NULL;
const char* sep = ",";
if (arr==NULL || cnt <= 0) {
return dc_strdup("");
}
#define INT_ARR_TO_STR(a, c) { \
int i; \
ret = malloc((c)*(11+strlen(sep))+1); \
if (ret==NULL) { exit(35); } \
ret[0] = 0; \
for (i=0; i<(c); i++) { \
if (i) { \
strcat(ret, sep); \
} \
sprintf(&ret[strlen(ret)], "%lu", (unsigned long)(a)[i]); \
} \
}
INT_ARR_TO_STR(arr, cnt);
return ret;
}
char* dc_array_get_string(const dc_array_t* array, const char* sep)
{
char* ret = NULL;
if (array==NULL || array->magic!=DC_ARRAY_MAGIC || sep==NULL) {
return dc_strdup("");
}
INT_ARR_TO_STR(array->array, array->count);
return ret;
}