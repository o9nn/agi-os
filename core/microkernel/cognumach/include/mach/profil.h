#ifndef _MACH_PROFIL_H_
#define _MACH_PROFIL_H_
#include <mach/boolean.h>
#include <ipc/ipc_object.h>
#include <vm/vm_kern.h>
#define	NB_PROF_BUFFER		2
#define	SIZE_PROF_BUFFER	100
struct	prof_data {
ipc_object_t	prof_port;
struct buffer {
int	*p_zone;
int			p_index;
boolean_t		p_full;
} prof_area[NB_PROF_BUFFER];
int		prof_index;
};
typedef struct prof_data	*prof_data_t;
#define NULLPBUF ((prof_data_t) 0)
typedef struct buffer		*buffer_t;
#define	set_pbuf_nb(pbuf, nb) \
(((nb) >= 0 && (nb) < NB_PROF_BUFFER) \
? (pbuf)->prof_index = (nb), 1 \
: 0)
#define	get_pbuf_nb(pbuf) \
(pbuf)->prof_index
extern vm_map_t kernel_map;
#define dealloc_pbuf_area(pbuf) \
do { \
register int i; \
\
for(i=0; i < NB_PROF_BUFFER ; i++)  \
kmem_free(kernel_map, \
(vm_offset_t) (pbuf)->prof_area[i].p_zone, \
SIZE_PROF_BUFFER*sizeof(int)); \
kmem_free(kernel_map, \
(vm_offset_t)(pbuf), \
sizeof(struct prof_data)); \
} while(0)
#define alloc_pbuf_area(pbuf, vmpbuf) \
do { \
(vmpbuf) = (vm_offset_t) 0; \
if (kmem_alloc(kernel_map, &(vmpbuf) , sizeof(struct prof_data)) == \
KERN_SUCCESS) { \
register int i; \
register boolean_t end; \
\
(pbuf) = (prof_data_t) (vmpbuf); \
for(i=0, end=FALSE; i < NB_PROF_BUFFER && end == FALSE; i++) { \
(vmpbuf) = (vm_offset_t) 0; \
if (kmem_alloc(kernel_map,&(vmpbuf),SIZE_PROF_BUFFER*sizeof(int)) == KERN_SUCCESS) { \
(pbuf)->prof_area[i].p_zone = (int *) (vmpbuf); \
(pbuf)->prof_area[i].p_full = FALSE; \
} \
else { \
(pbuf) = NULLPBUF; \
end = TRUE; \
} \
} \
} \
else \
(pbuf) = NULLPBUF; \
} while(0)
#define set_pbuf_value(pbuf, val) \
do { \
register buffer_t a = &((pbuf)->prof_area[(pbuf)->prof_index]); \
register int i = a->p_index++; \
register boolean_t f = a->p_full; \
\
if (f == TRUE ) \
*(val) = 0; \
else { \
a->p_zone[i] = *(val); \
if (i == SIZE_PROF_BUFFER-1) { \
a->p_full = TRUE; \
*(val) = 2; \
} \
else \
*(val) = 1; \
} \
} while(0)
#define	reset_pbuf_area(pbuf) \
do { \
register int *i = &((pbuf)->prof_index); \
\
*i = (*i == NB_PROF_BUFFER-1) ? 0 : ++(*i); \
(pbuf)->prof_area[*i].p_index = 0; \
} while(0)
#define	thread_t int *
struct buf_to_send {
queue_chain_t list;
thread_t thread;
int number;
char wakeme;
}	;
#undef	thread_t
typedef struct buf_to_send *buf_to_send_t;
#define	NULLBTS		((buf_to_send_t) 0)
mpqueue_head_t prof_queue;
#endif