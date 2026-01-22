#include "md5global.h"
#ifndef MD5_H
#define MD5_H
#ifdef __cplusplus
extern "C" {
#endif
typedef struct {
UINT4 state[4];
UINT4 count[2];
unsigned char buffer[64];
} MD5_CTX;
void MD5Init PROTO_LIST ((MD5_CTX *));
void MD5Update PROTO_LIST
((MD5_CTX *, const unsigned char *, unsigned int));
void MD5Final PROTO_LIST ((unsigned char [16], MD5_CTX *));
void hmac_md5 PROTO_LIST ((const unsigned char *, int, const unsigned char *, int, unsigned char *));
#ifdef __cplusplus
}
#endif
#endif