#ifndef MD5GLOBAL_H
#define MD5GLOBAL_H
#include "md5namespace.h"
#ifdef __cplusplus
extern "C" {
#endif
#ifndef PROTOTYPES
#define PROTOTYPES 1
#endif
typedef unsigned char *POINTER;
typedef const unsigned char *CONST_POINTER;
typedef unsigned short int UINT2;
typedef unsigned long int UINT4;
#if PROTOTYPES
#define PROTO_LIST(list) list
#else
#define PROTO_LIST(list) ()
#endif
#ifdef __cplusplus
}
#endif
#endif