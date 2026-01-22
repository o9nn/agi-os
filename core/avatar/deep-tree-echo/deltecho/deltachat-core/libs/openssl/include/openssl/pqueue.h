#ifndef HEADER_PQUEUE_H
# define HEADER_PQUEUE_H
# include <stdio.h>
# include <stdlib.h>
# include <string.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef struct _pqueue *pqueue;
typedef struct _pitem {
unsigned char priority[8];
void *data;
struct _pitem *next;
} pitem;
typedef struct _pitem *piterator;
pitem *pitem_new(unsigned char *prio64be, void *data);
void pitem_free(pitem *item);
pqueue pqueue_new(void);
void pqueue_free(pqueue pq);
pitem *pqueue_insert(pqueue pq, pitem *item);
pitem *pqueue_peek(pqueue pq);
pitem *pqueue_pop(pqueue pq);
pitem *pqueue_find(pqueue pq, unsigned char *prio64be);
pitem *pqueue_iterator(pqueue pq);
pitem *pqueue_next(piterator *iter);
void pqueue_print(pqueue pq);
int pqueue_size(pqueue pq);
#ifdef __cplusplus
}
#endif
#endif