#ifndef HEADER_STORE_LOCL_H
# define HEADER_STORE_LOCL_H
# include <openssl/crypto.h>
# include <openssl/store.h>
#ifdef __cplusplus
extern "C" {
#endif
struct store_method_st {
char *name;
STORE_INITIALISE_FUNC_PTR init;
STORE_CLEANUP_FUNC_PTR clean;
STORE_GENERATE_OBJECT_FUNC_PTR generate_object;
STORE_GET_OBJECT_FUNC_PTR get_object;
STORE_STORE_OBJECT_FUNC_PTR store_object;
STORE_MODIFY_OBJECT_FUNC_PTR modify_object;
STORE_HANDLE_OBJECT_FUNC_PTR revoke_object;
STORE_HANDLE_OBJECT_FUNC_PTR delete_object;
STORE_START_OBJECT_FUNC_PTR list_object_start;
STORE_NEXT_OBJECT_FUNC_PTR list_object_next;
STORE_END_OBJECT_FUNC_PTR list_object_end;
STORE_END_OBJECT_FUNC_PTR list_object_endp;
STORE_GENERIC_FUNC_PTR update_store;
STORE_GENERIC_FUNC_PTR lock_store;
STORE_GENERIC_FUNC_PTR unlock_store;
STORE_CTRL_FUNC_PTR ctrl;
};
struct store_st {
const STORE_METHOD *meth;
ENGINE *engine;
CRYPTO_EX_DATA ex_data;
int references;
};
#ifdef __cplusplus
}
#endif
#endif