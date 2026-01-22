#include <stdlib.h>
#include <string.h>
#include <openssl/opensslconf.h>
#include <openssl/md4.h>
#ifndef MD4_LONG_LOG2
# define MD4_LONG_LOG2 2
#endif
void md4_block_data_order(MD4_CTX *c, const void *p, size_t num);
#define DATA_ORDER_IS_LITTLE_ENDIAN
#define HASH_LONG MD4_LONG
#define HASH_CTX MD4_CTX
#define HASH_CBLOCK MD4_CBLOCK
#define HASH_UPDATE MD4_Update
#define HASH_TRANSFORM MD4_Transform
#define HASH_FINAL MD4_Final
#define HASH_MAKE_STRING(c,s) do { \
unsigned long ll; \
ll=(c)->A; (void)HOST_l2c(ll,(s)); \
ll=(c)->B; (void)HOST_l2c(ll,(s)); \
ll=(c)->C; (void)HOST_l2c(ll,(s)); \
ll=(c)->D; (void)HOST_l2c(ll,(s)); \
} while (0)
#define HASH_BLOCK_DATA_ORDER md4_block_data_order
#include "md32_common.h"
#define F(b,c,d) ((((c) ^ (d)) & (b)) ^ (d))
#define G(b,c,d) (((b) & (c)) | ((b) & (d)) | ((c) & (d)))
#define H(b,c,d) ((b) ^ (c) ^ (d))
#define R0(a,b,c,d,k,s,t) { \
a+=((k)+(t)+F((b),(c),(d))); \
a=ROTATE(a,s); };
#define R1(a,b,c,d,k,s,t) { \
a+=((k)+(t)+G((b),(c),(d))); \
a=ROTATE(a,s); };\
#define R2(a,b,c,d,k,s,t) { \
a+=((k)+(t)+H((b),(c),(d))); \
a=ROTATE(a,s); };