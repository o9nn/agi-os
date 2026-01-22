#ifndef	_KERN_HOST_H_
#define _KERN_HOST_H_
struct	host {
struct ipc_port *host_self;
struct ipc_port *host_priv_self;
};
typedef struct host	*host_t;
typedef struct host	host_data_t;
#define HOST_NULL	((host_t)0)
extern host_data_t	realhost;
#endif