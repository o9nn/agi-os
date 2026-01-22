#ifndef __DC_KEYRING_H__
#define __DC_KEYRING_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "dc_key.h"
typedef struct _dc_keyring dc_keyring_t;
struct _dc_keyring
{
dc_key_t** keys;
int        count;
int        allocated;
};
dc_keyring_t* dc_keyring_new  ();
void          dc_keyring_unref();
void          dc_keyring_add  (dc_keyring_t*, dc_key_t*);
int           dc_keyring_load_self_private_for_decrypting(dc_keyring_t*, const char* self_addr, dc_sqlite3_t* sql);
#ifdef __cplusplus
}
#endif
#endif