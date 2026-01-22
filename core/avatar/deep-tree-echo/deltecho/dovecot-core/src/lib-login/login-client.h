#ifndef LOGIN_CLIENT_H
#define LOGIN_CLIENT_H
#include "login-interface.h"
struct master_service;
struct login_client_request_params {
int client_fd;
const char *socket_path;
struct login_request request;
const unsigned char *data;
};
typedef void login_client_request_callback_t(const struct login_reply *reply,
void *context);
struct login_client_list *
login_client_list_init(struct master_service *service, const char *path);
void login_client_list_deinit(struct login_client_list **list);
void login_client_request(struct login_client_list *list,
const struct login_client_request_params *params,
login_client_request_callback_t *callback,
void *context, unsigned int *tag_r);
void login_client_request_abort(struct login_client_list *list,
unsigned int tag);
#endif