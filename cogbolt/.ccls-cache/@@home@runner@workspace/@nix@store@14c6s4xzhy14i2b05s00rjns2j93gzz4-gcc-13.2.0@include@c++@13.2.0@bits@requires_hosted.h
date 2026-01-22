#ifndef _REQUIRES_FREESTANDING_H
#define _REQUIRES_FREESTANDING_H 1
#include <bits/c++config.h>
#if !_GLIBCXX_HOSTED
# error "This header is not available in freestanding mode."
#endif
#endif