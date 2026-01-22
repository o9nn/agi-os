#ifndef SERVICE_LISTEN_H
#define SERVICE_LISTEN_H
int services_listen(struct service_list *service_list);
int services_listen_using(struct service_list *new_service_list,
struct service_list *old_service_list);
int service_listener_listen(struct service_listener *l);
int service_unix_listener_listen(struct service_listener *l, const char *path,
bool verify_addrinuse, const char **error_r);
#endif