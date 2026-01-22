#ifndef MD5GLOBAL_H
#define MD5GLOBAL_H
#ifndef PROTOTYPES
#define PROTOTYPES 0
#endif
typedef unsigned char *POINTER;
typedef signed char INT1;
typedef short INT2;
typedef int INT4;
typedef unsigned char UINT1;
typedef unsigned short UINT2;
typedef unsigned int UINT4;
#if PROTOTYPES
#define PROTO_LIST(list) list
#else
#define PROTO_LIST(list) ()
#endif
#endif