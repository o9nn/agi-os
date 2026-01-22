#ifndef HEADER_ENGINE_INT_H
# define HEADER_ENGINE_INT_H
# include "cryptlib.h"
# include <openssl/engine.h>
#ifdef __cplusplus
extern "C" {
#endif
# ifdef ENGINE_REF_COUNT_DEBUG
# define engine_ref_debug(e, isfunct, diff) \
fprintf(stderr, "engine: %08x %s from %d to %d (%s:%d)\n", \
(unsigned int)(e), (isfunct ? "funct" : "struct"), \
((isfunct) ? ((e)->funct_ref - (diff)) : ((e)->struct_ref - (diff))), \
((isfunct) ? (e)->funct_ref : (e)->struct_ref), \
(__FILE__), (__LINE__));
# else
# define engine_ref_debug(e, isfunct, diff)
# endif
typedef void (ENGINE_CLEANUP_CB) (void);
typedef struct st_engine_cleanup_item {
ENGINE_CLEANUP_CB *cb;
} ENGINE_CLEANUP_ITEM;
DECLARE_STACK_OF(ENGINE_CLEANUP_ITEM)
void engine_cleanup_add_first(ENGINE_CLEANUP_CB *cb);
void engine_cleanup_add_last(ENGINE_CLEANUP_CB *cb);
DECLARE_STACK_OF(ENGINE)
typedef struct st_engine_table ENGINE_TABLE;
int engine_table_register(ENGINE_TABLE **table, ENGINE_CLEANUP_CB *cleanup,
ENGINE *e, const int *nids, int num_nids,
int setdefault);
void engine_table_unregister(ENGINE_TABLE **table, ENGINE *e);
void engine_table_cleanup(ENGINE_TABLE **table);
# ifndef ENGINE_TABLE_DEBUG
ENGINE *engine_table_select(ENGINE_TABLE **table, int nid);
# else
ENGINE *engine_table_select_tmp(ENGINE_TABLE **table, int nid, const char *f,
int l);
# define engine_table_select(t,n) engine_table_select_tmp(t,n,__FILE__,__LINE__)
# endif
typedef void (engine_table_doall_cb) (int nid, STACK_OF(ENGINE) *sk,
ENGINE *def, void *arg);
void engine_table_doall(ENGINE_TABLE *table, engine_table_doall_cb *cb,
void *arg);
int engine_unlocked_init(ENGINE *e);
int engine_unlocked_finish(ENGINE *e, int unlock_for_handlers);
int engine_free_util(ENGINE *e, int locked);
void engine_set_all_null(ENGINE *e);
void engine_pkey_meths_free(ENGINE *e);
void engine_pkey_asn1_meths_free(ENGINE *e);
struct engine_st {
const char *id;
const char *name;
const RSA_METHOD *rsa_meth;
const DSA_METHOD *dsa_meth;
const DH_METHOD *dh_meth;
const ECDH_METHOD *ecdh_meth;
const ECDSA_METHOD *ecdsa_meth;
const RAND_METHOD *rand_meth;
const STORE_METHOD *store_meth;
ENGINE_CIPHERS_PTR ciphers;
ENGINE_DIGESTS_PTR digests;
ENGINE_PKEY_METHS_PTR pkey_meths;
ENGINE_PKEY_ASN1_METHS_PTR pkey_asn1_meths;
ENGINE_GEN_INT_FUNC_PTR destroy;
ENGINE_GEN_INT_FUNC_PTR init;
ENGINE_GEN_INT_FUNC_PTR finish;
ENGINE_CTRL_FUNC_PTR ctrl;
ENGINE_LOAD_KEY_PTR load_privkey;
ENGINE_LOAD_KEY_PTR load_pubkey;
ENGINE_SSL_CLIENT_CERT_PTR load_ssl_client_cert;
const ENGINE_CMD_DEFN *cmd_defns;
int flags;
int struct_ref;
int funct_ref;
CRYPTO_EX_DATA ex_data;
struct engine_st *prev;
struct engine_st *next;
};
#ifdef __cplusplus
}
#endif
#endif