#ifndef GENERIC_CACHE_TYPE_H
#define GENERIC_CACHE_TYPE_H
#include <libetpan/carray.h>
#include <libetpan/chash.h>
#ifdef __cplusplus
extern "C" {
#endif
struct mail_flags_store {
carray * fls_tab;
chash * fls_hash;
};
#ifdef __cplusplus
}
#endif
#endif