#ifndef _LINUX_IF_EQL_H
#define _LINUX_IF_EQL_H
#include <linux/timer.h>
#define EQL_DEFAULT_SLAVE_PRIORITY 28800
#define EQL_DEFAULT_MAX_SLAVES     4
#define EQL_DEFAULT_MTU            576
#define EQL_DEFAULT_RESCHED_IVAL   100
#define EQL_ENSLAVE     (SIOCDEVPRIVATE)
#define EQL_EMANCIPATE  (SIOCDEVPRIVATE + 1)
#define EQL_GETSLAVECFG (SIOCDEVPRIVATE + 2)
#define EQL_SETSLAVECFG (SIOCDEVPRIVATE + 3)
#define EQL_GETMASTRCFG (SIOCDEVPRIVATE + 4)
#define EQL_SETMASTRCFG (SIOCDEVPRIVATE + 5)
typedef struct slave {
struct device *dev;
long priority;
long priority_bps;
long priority_Bps;
long bytes_queued;
struct slave *next;
} slave_t;
typedef struct slave_queue {
slave_t *head;
slave_t *best_slave;
int num_slaves;
struct device *master_dev;
char lock;
} slave_queue_t;
typedef struct equalizer {
slave_queue_t *queue;
int min_slaves;
int max_slaves;
struct enet_statistics *stats;
struct timer_list timer;
char timer_on;
} equalizer_t;
typedef struct master_config {
char master_name[16];
int max_slaves;
int min_slaves;
} master_config_t;
typedef struct slave_config {
char slave_name[16];
long priority;
} slave_config_t;
typedef struct slaving_request {
char slave_name[16];
long priority;
} slaving_request_t;
#endif