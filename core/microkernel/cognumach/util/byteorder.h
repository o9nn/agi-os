#ifndef _UTIL_BYTEORDER_H_
#define _UTIL_BYTEORDER_H_
#include <stdint.h>
uint16_t ntohs(uint16_t netshort);
uint32_t ntohl(uint32_t netlong);
uint16_t htons(uint16_t hostshort);
uint32_t htonl(uint32_t hostlong);
#endif