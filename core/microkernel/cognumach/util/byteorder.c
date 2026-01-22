#include "util/byteorder.h"
uint16_t ntohs(uint16_t netshort) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
return __builtin_bswap16(netshort);
#else
return netshort;
#endif
}
uint32_t ntohl(uint32_t netlong) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
return __builtin_bswap32(netlong);
#else
return netlong;
#endif
}
uint16_t htons(uint16_t hostshort) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
return __builtin_bswap16(hostshort);
#else
return hostshort;
#endif
}
uint32_t htonl(uint32_t hostlong) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
return __builtin_bswap32(hostlong);
#else
return hostlong;
#endif
}