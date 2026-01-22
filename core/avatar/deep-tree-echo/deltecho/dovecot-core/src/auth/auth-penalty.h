#ifndef AUTH_PENALTY_H
#define AUTH_PENALTY_H
struct auth_request;
#define AUTH_PENALTY_INIT_SECS 2
#define AUTH_PENALTY_MAX_SECS 15
#define AUTH_PENALTY_TIMEOUT \
(AUTH_PENALTY_INIT_SECS + 4 + 8 + AUTH_PENALTY_MAX_SECS)
#define AUTH_PENALTY_MAX_PENALTY 4
typedef void auth_penalty_callback_t(unsigned int penalty,
struct auth_request *request);
struct auth_penalty *auth_penalty_init(const char *path);
void auth_penalty_deinit(struct auth_penalty **penalty);
unsigned int auth_penalty_to_secs(unsigned int penalty);
void auth_penalty_lookup(struct auth_penalty *penalty,
struct auth_request *auth_request,
auth_penalty_callback_t *callback);
void auth_penalty_update(struct auth_penalty *penalty,
struct auth_request *auth_request, unsigned int value);
#endif