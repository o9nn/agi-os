#ifndef MAILSEM_H
#define MAILSEM_H
#include <libetpan/libetpan-config.h>
struct mailsem {
void * sem_sem;
int sem_kind;
};
LIBETPAN_EXPORT
struct mailsem * mailsem_new(void);
LIBETPAN_EXPORT
void mailsem_free(struct mailsem * sem);
LIBETPAN_EXPORT
int mailsem_up(struct mailsem * sem);
LIBETPAN_EXPORT
int mailsem_down(struct mailsem * sem);
#endif