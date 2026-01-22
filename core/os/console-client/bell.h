#ifndef _BELL_H_
#define _BELL_H_ 1
#include <errno.h>
struct bell_ops;
typedef struct bell_ops *bell_ops_t;
error_t driver_add_bell (bell_ops_t ops, void *handle);
error_t driver_remove_bell (bell_ops_t ops, void *handle);
struct bell_ops
{
error_t (*beep) (void *handle);
void (*deprecated) (void *handle, unsigned int key);
};
#endif