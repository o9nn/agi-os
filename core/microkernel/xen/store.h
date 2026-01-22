#ifndef XEN_STORE_H
#define XEN_STORE_H
#include <machine/xen.h>
#include <xen/public/io/xenbus.h>
typedef uint32_t hyp_store_transaction_t;
#define hyp_store_state_unknown "0"
#define hyp_store_state_initializing "1"
#define hyp_store_state_init_wait "2"
#define hyp_store_state_initialized "3"
#define hyp_store_state_connected "4"
#define hyp_store_state_closing "5"
#define hyp_store_state_closed "6"
void hyp_store_init(void);
extern const char *hyp_store_error;
hyp_store_transaction_t hyp_store_transaction_start(void);
int hyp_store_transaction_stop(hyp_store_transaction_t t);
char **hyp_store_ls(hyp_store_transaction_t t, int n, ...);
void *hyp_store_read(hyp_store_transaction_t t, int n, ...);
int hyp_store_read_int(hyp_store_transaction_t t, int n, ...);
char *hyp_store_write(hyp_store_transaction_t t, const char *data, int n, ...);
#endif