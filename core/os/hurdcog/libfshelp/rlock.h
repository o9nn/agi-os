#ifndef FSHELP_RLOCK_H
#define FSHELP_RLOCK_H
#ifdef FSHELP_DEFINE_EXTERN_INLINE
#define FSHELP_EXTERN_INLINE
#else
#define FSHELP_EXTERN_INLINE __extern_inline
#endif
#include <pthread.h>
#include <string.h>
struct rlock_linked_list
{
struct rlock_list *next;
struct rlock_list **prevp;
};
struct rlock_list
{
loff_t start;
loff_t len;
int type;
struct rlock_linked_list node;
struct rlock_linked_list po;
pthread_cond_t wait;
int waiting;
void *po_id;
};
FSHELP_EXTERN_INLINE error_t
rlock_list_init (struct rlock_peropen *po, struct rlock_list *l)
{
memset (l, 0, sizeof (struct rlock_list));
pthread_cond_init (&l->wait, NULL);
l->po_id = po->locks;
return 0;
}
#define list_link(X, head, node)				\
do							\
{							\
struct rlock_list **e;				\
for (e = head;					\
*e && ((*e)->start < node->start);		\
e = &(*e)->X.next)				\
;							\
node->X.next = *e;					\
if (node->X.next)					\
node->X.next->X.prevp = &node->X.next;		\
node->X.prevp = e;					\
*e = node;						\
}							\
while (0)
#define list_unlink(X, node)					\
do							\
{							\
*node->X.prevp = node->X.next;			\
if (node->X.next)					\
node->X.next->X.prevp = node->X.prevp;		\
}							\
while (0)
#endif