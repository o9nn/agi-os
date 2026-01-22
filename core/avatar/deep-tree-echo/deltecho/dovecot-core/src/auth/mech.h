#ifndef MECH_H
#define MECH_H
#include "auth-client-interface.h"
struct auth_settings;
struct auth_request;
#include "auth-request.h"
#include "auth-request-handler.h"
#define MAX_MECH_NAME_LEN 64
enum mech_passdb_need {
MECH_PASSDB_NEED_NOTHING = 0,
MECH_PASSDB_NEED_VERIFY_PLAIN,
MECH_PASSDB_NEED_VERIFY_RESPONSE,
MECH_PASSDB_NEED_LOOKUP_CREDENTIALS,
MECH_PASSDB_NEED_SET_CREDENTIALS
};
struct mech_module {
const char *mech_name;
enum mech_security_flags flags;
enum mech_passdb_need passdb_need;
struct auth_request *(*auth_new)(void);
void (*auth_initial)(struct auth_request *request,
const unsigned char *data, size_t data_size);
void (*auth_continue)(struct auth_request *request,
const unsigned char *data, size_t data_size);
void (*auth_free)(struct auth_request *request);
};
struct mech_module_list {
struct mech_module_list *next;
struct mech_module module;
};
struct mechanisms_register {
pool_t pool;
const struct auth_settings *set;
struct mech_module_list *modules;
buffer_t *handshake;
};
extern const struct mech_module mech_dovecot_token;
void mech_register_module(const struct mech_module *module);
void mech_unregister_module(const struct mech_module *module);
const struct mech_module *mech_module_find(const char *name);
void mech_generic_auth_initial(struct auth_request *request,
const unsigned char *data, size_t data_size);
void mech_generic_auth_free(struct auth_request *request);
struct mechanisms_register *
mech_register_init(const struct auth_settings *set);
void mech_register_deinit(struct mechanisms_register **reg);
const struct mech_module *
mech_register_find(const struct mechanisms_register *reg, const char *name);
void mech_init(const struct auth_settings *set);
void mech_deinit(const struct auth_settings *set);
#endif