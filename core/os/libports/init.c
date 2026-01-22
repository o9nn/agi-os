#include "ports.h"
#include <stddef.h>
pthread_mutex_t _ports_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t _ports_block = PTHREAD_COND_INITIALIZER;
struct hurd_ihash _ports_htable =
HURD_IHASH_INITIALIZER (offsetof (struct port_info, ports_htable_entry));
pthread_rwlock_t _ports_htable_lock = PTHREAD_RWLOCK_INITIALIZER;
int _ports_total_rpcs;
int _ports_flags;