#ifndef MASTER_ADMIN_CLIENT_H
#define MASTER_ADMIN_CLIENT_H
#include "guid.h"
struct master_service_connection;
struct master_admin_client;
struct master_admin_client_callback {
bool (*cmd)(struct master_admin_client *client,
const char *cmd, const char *const *args);
unsigned int (*cmd_kick_user)(const char *user,
const guid_128_t conn_guid);
};
void master_admin_client_create(struct master_service_connection *master_conn);
void master_admin_client_send_reply(struct master_admin_client *client,
const char *reply);
bool master_admin_client_can_accept(const char *name);
void master_admin_clients_init(const struct master_admin_client_callback *callbacks);
#endif