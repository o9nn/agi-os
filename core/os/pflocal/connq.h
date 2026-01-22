#ifndef __CONNQ_H__
#define __CONNQ_H__
#include <errno.h>
struct connq;
struct sock;
error_t connq_create (struct connq **cq);
void connq_destroy (struct connq *cq);
error_t connq_listen (struct connq *cq, struct timespec *tsp,
struct sock **sock);
error_t connq_connect (struct connq *cq, int noblock);
void connq_connect_complete (struct connq *cq, struct sock *sock);
void connq_connect_cancel (struct connq *cq);
error_t connq_set_length (struct connq *cq, int length);
#endif