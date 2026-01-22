#if !defined _NETINET_IN_H && !defined _ENDIAN_H
# error "Never use <bits/uintn-identity.h> directly; include <netinet/in.h> or <endian.h> instead."
#endif
#ifndef _BITS_UINTN_IDENTITY_H
#define _BITS_UINTN_IDENTITY_H 1
#include <bits/types.h>
static __inline __uint16_t
__uint16_identity (__uint16_t __x)
{
return __x;
}
static __inline __uint32_t
__uint32_identity (__uint32_t __x)
{
return __x;
}
static __inline __uint64_t
__uint64_identity (__uint64_t __x)
{
return __x;
}
#endif