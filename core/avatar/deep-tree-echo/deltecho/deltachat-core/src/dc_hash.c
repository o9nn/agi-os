#include <assert.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <stdint.h>
#include "dc_context.h"
#include "dc_hash.h"
#define Addr(X) ((uintptr_t)X)
static void* sjhashMalloc(long bytes) { void* p=malloc(bytes); if (p) memset(p, 0, bytes); return p; }
#define sjhashMallocRaw(a) malloc((a))
#define sjhashFree(a) free((a))
static const unsigned char sjhashUpperToLower[] = {
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53,
54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 97, 98, 99,100,101,102,103,
104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,
122, 91, 92, 93, 94, 95, 96, 97, 98, 99,100,101,102,103,104,105,106,107,
108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,
126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,
144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,
162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,
180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,
198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,
216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,
234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,
252,253,254,255
};
static int sjhashStrNICmp(const char *zLeft, const char *zRight, int N)
{
register unsigned char *a, *b;
a = (unsigned char *)zLeft;
b = (unsigned char *)zRight;
while (N-- > 0 && *a!=0 && sjhashUpperToLower[*a]==sjhashUpperToLower[*b]) { a++; b++; }
return N<0 ? 0 : sjhashUpperToLower[*a] - sjhashUpperToLower[*b];
}
static int sjhashNoCase(const char *z, int n)
{
int h = 0;
if (n<=0) n = strlen(z);
while (n > 0) {
h = (h<<3) ^ h ^ sjhashUpperToLower[(unsigned char)*z++];
n--;
}
return h & 0x7fffffff;
}
void dc_hash_init(dc_hash_t *pNew, int keyClass, int copyKey)
{
assert( pNew!=0);
assert( keyClass>=DC_HASH_INT && keyClass<=DC_HASH_BINARY);
pNew->keyClass = keyClass;
if (keyClass==DC_HASH_POINTER || keyClass==DC_HASH_INT) copyKey = 0;
pNew->copyKey = copyKey;
pNew->first = 0;
pNew->count = 0;
pNew->htsize = 0;
pNew->ht = 0;
}
void dc_hash_clear(dc_hash_t *pH)
{
dc_hashelem_t *elem;
if (pH == NULL) {
return;
}
elem = pH->first;
pH->first = 0;
if (pH->ht) sjhashFree(pH->ht);
pH->ht = 0;
pH->htsize = 0;
while (elem)
{
dc_hashelem_t *next_elem = elem->next;
if (pH->copyKey && elem->pKey)
{
sjhashFree(elem->pKey);
}
sjhashFree(elem);
elem = next_elem;
}
pH->count = 0;
}
static int intHash(const void *pKey, int nKey)
{
return nKey ^ (nKey<<8) ^ (nKey>>8);
}
static int intCompare(const void *pKey1, int n1, const void *pKey2, int n2)
{
return n2 - n1;
}
static int ptrHash(const void *pKey, int nKey)
{
uintptr_t x = Addr(pKey);
return x ^ (x<<8) ^ (x>>8);
}
static int ptrCompare(const void *pKey1, int n1, const void *pKey2, int n2)
{
if (pKey1==pKey2) return 0;
if (pKey1<pKey2) return -1;
return 1;
}
static int strHash(const void *pKey, int nKey)
{
return sjhashNoCase((const char*)pKey, nKey);
}
static int strCompare(const void *pKey1, int n1, const void *pKey2, int n2)
{
if (n1!=n2) return 1;
return sjhashStrNICmp((const char*)pKey1,(const char*)pKey2,n1);
}
static int binHash(const void *pKey, int nKey)
{
int h = 0;
const char *z = (const char *)pKey;
while (nKey-- > 0)
{
h = (h<<3) ^ h ^ *(z++);
}
return h & 0x7fffffff;
}
static int binCompare(const void *pKey1, int n1, const void *pKey2, int n2)
{
if (n1!=n2) return 1;
return memcmp(pKey1,pKey2,n1);
}
static int (*hashFunction(int keyClass))(const void*,int)
{
switch (keyClass)
{
case DC_HASH_INT: return &intHash;
case DC_HASH_POINTER:return &ptrHash;
case DC_HASH_STRING: return &strHash;
case DC_HASH_BINARY: return &binHash;;
default: break;
}
return 0;
}
static int (*compareFunction(int keyClass))(const void*,int,const void*,int)
{
switch (keyClass)
{
case DC_HASH_INT: return &intCompare;
case DC_HASH_POINTER: return &ptrCompare;
case DC_HASH_STRING: return &strCompare;
case DC_HASH_BINARY: return &binCompare;
default: break;
}
return 0;
}
static void insertElement(dc_hash_t *pH,
struct _ht *pEntry,
dc_hashelem_t *pNew)
{
dc_hashelem_t *pHead;
pHead = pEntry->chain;
if (pHead)
{
pNew->next = pHead;
pNew->prev = pHead->prev;
if (pHead->prev) { pHead->prev->next = pNew; }
else { pH->first = pNew; }
pHead->prev = pNew;
}
else
{
pNew->next = pH->first;
if (pH->first) { pH->first->prev = pNew; }
pNew->prev = 0;
pH->first = pNew;
}
pEntry->count++;
pEntry->chain = pNew;
}
static void rehash(dc_hash_t *pH, int new_size)
{
struct _ht *new_ht;
dc_hashelem_t *elem, *next_elem;
int (*xHash)(const void*,int);
assert( (new_size & (new_size-1))==0);
new_ht = (struct _ht *)sjhashMalloc( new_size*sizeof(struct _ht));
if (new_ht==0) return;
if (pH->ht) sjhashFree(pH->ht);
pH->ht = new_ht;
pH->htsize = new_size;
xHash = hashFunction(pH->keyClass);
for(elem=pH->first, pH->first=0; elem; elem = next_elem)
{
int h = (*xHash)(elem->pKey, elem->nKey) & (new_size-1);
next_elem = elem->next;
insertElement(pH, &new_ht[h], elem);
}
}
static dc_hashelem_t *findElementGivenHash(const dc_hash_t *pH,
const void *pKey,
int nKey,
int h)
{
dc_hashelem_t *elem;
int count;
int (*xCompare)(const void*,int,const void*,int);
if (pH->ht)
{
struct _ht *pEntry = &pH->ht[h];
elem = pEntry->chain;
count = pEntry->count;
xCompare = compareFunction(pH->keyClass);
while (count-- && elem)
{
if ((*xCompare)(elem->pKey,elem->nKey,pKey,nKey)==0)
{
return elem;
}
elem = elem->next;
}
}
return 0;
}
static void removeElementGivenHash(dc_hash_t *pH,
dc_hashelem_t* elem,
int h)
{
struct _ht *pEntry;
if (elem->prev)
{
elem->prev->next = elem->next;
}
else
{
pH->first = elem->next;
}
if (elem->next)
{
elem->next->prev = elem->prev;
}
pEntry = &pH->ht[h];
if (pEntry->chain==elem)
{
pEntry->chain = elem->next;
}
pEntry->count--;
if (pEntry->count<=0)
{
pEntry->chain = 0;
}
if (pH->copyKey && elem->pKey)
{
sjhashFree(elem->pKey);
}
sjhashFree( elem);
pH->count--;
}
void* dc_hash_find(const dc_hash_t *pH, const void *pKey, int nKey)
{
int h;
dc_hashelem_t *elem;
int (*xHash)(const void*,int);
if (pH==0 || pH->ht==0) return 0;
xHash = hashFunction(pH->keyClass);
assert( xHash!=0);
h = (*xHash)(pKey,nKey);
assert( (pH->htsize & (pH->htsize-1))==0);
elem = findElementGivenHash(pH,pKey,nKey, h & (pH->htsize-1));
return elem ? elem->data : 0;
}
void* dc_hash_insert(dc_hash_t *pH, const void *pKey, int nKey, void *data)
{
int hraw;
int h;
dc_hashelem_t *elem;
dc_hashelem_t *new_elem;
int (*xHash)(const void*,int);
assert( pH!=0);
xHash = hashFunction(pH->keyClass);
assert( xHash!=0);
hraw = (*xHash)(pKey, nKey);
assert( (pH->htsize & (pH->htsize-1))==0);
h = hraw & (pH->htsize-1);
elem = findElementGivenHash(pH,pKey,nKey,h);
if (elem)
{
void *old_data = elem->data;
if (data==0)
{
removeElementGivenHash(pH,elem,h);
}
else
{
elem->data = data;
}
return old_data;
}
if (data==0) return 0;
new_elem = (dc_hashelem_t*)sjhashMalloc( sizeof(dc_hashelem_t));
if (new_elem==0) return data;
if (pH->copyKey && pKey!=0)
{
new_elem->pKey = sjhashMallocRaw( nKey);
if (new_elem->pKey==0)
{
sjhashFree(new_elem);
return data;
}
memcpy((void*)new_elem->pKey, pKey, nKey);
}
else
{
new_elem->pKey = (void*)pKey;
}
new_elem->nKey = nKey;
pH->count++;
if (pH->htsize==0)
{
rehash(pH,8);
if (pH->htsize==0)
{
pH->count = 0;
sjhashFree(new_elem);
return data;
}
}
if (pH->count > pH->htsize)
{
rehash(pH,pH->htsize*2);
}
assert( pH->htsize>0);
assert( (pH->htsize & (pH->htsize-1))==0);
h = hraw & (pH->htsize-1);
insertElement(pH, &pH->ht[h], new_elem);
new_elem->data = data;
return 0;
}