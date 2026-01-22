#ifndef	_KERN_SHUTTLE_H_
#define _KERN_SHUTTLE_H_
#include <kern/lock.h>
struct Shuttle {
queue_chain_t	links;
run_queue_t	runq;
struct Shuttle *next;
int priority;
void		*message;
int foobar[1];
};
typedef struct Shuttle Shuttle;
#endif