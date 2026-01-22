#ifndef _HURD_SLAB_H
#define _HURD_SLAB_H	1
#include <errno.h>
#include <stdbool.h>
#include <pthread.h>
typedef error_t (*hurd_slab_allocate_buffer_t) (void *hook, size_t size,
void **ptr);
typedef error_t (*hurd_slab_deallocate_buffer_t) (void *hook, void *buffer,
size_t size);
typedef error_t (*hurd_slab_constructor_t) (void *hook, void *object);
typedef void (*hurd_slab_destructor_t) (void *hook, void *object);
typedef struct hurd_slab_space *hurd_slab_space_t;
struct hurd_slab_space
{
bool initialized;
pthread_mutex_t lock;
size_t requested_size;
size_t requested_align;
size_t slab_size;
hurd_slab_allocate_buffer_t allocate_buffer;
hurd_slab_deallocate_buffer_t deallocate_buffer;
hurd_slab_constructor_t constructor;
hurd_slab_destructor_t destructor;
void *hook;
struct hurd_slab *slab_first;
struct hurd_slab *slab_last;
struct hurd_slab *first_free;
int full_refcount;
size_t size;
};
#define HURD_SLAB_SPACE_INITIALIZER(TYPE, ALLOC, DEALLOC, CTOR,	\
DTOR, HOOK)			\
{								\
false,							\
PTHREAD_MUTEX_INITIALIZER, 					\
sizeof (TYPE),						\
__alignof__ (TYPE),						\
ALLOC,							\
DEALLOC,							\
CTOR,							\
DTOR,							\
HOOK							\
\
}
error_t hurd_slab_create (size_t size, size_t alignment,
hurd_slab_allocate_buffer_t allocate_buffer,
hurd_slab_deallocate_buffer_t deallocate_buffer,
hurd_slab_constructor_t constructor,
hurd_slab_destructor_t destructor,
void *hook,
hurd_slab_space_t *space);
error_t hurd_slab_free (hurd_slab_space_t space);
error_t hurd_slab_init (hurd_slab_space_t space, size_t size, size_t alignment,
hurd_slab_allocate_buffer_t allocate_buffer,
hurd_slab_deallocate_buffer_t deallocate_buffer,
hurd_slab_constructor_t constructor,
hurd_slab_destructor_t destructor,
void *hook);
error_t hurd_slab_destroy (hurd_slab_space_t space);
error_t hurd_slab_alloc (hurd_slab_space_t space, void **buffer);
void hurd_slab_dealloc (hurd_slab_space_t space, void *buffer);
#define SLAB_CLASS(name, element_type)                                       \
struct hurd_##name##_slab_space						     \
{									     \
struct hurd_slab_space space;						     \
};									     \
typedef struct hurd_##name##_slab_space *hurd_##name##_slab_space_t;	     \
\
typedef error_t (*hurd_##name##_slab_constructor_t) (void *hook,	     \
element_type *buffer);  \
\
typedef void (*hurd_##name##_slab_destructor_t) (void *hook,		     \
element_type *buffer);	     \
\
static inline error_t							     \
hurd_##name##_slab_create (hurd_slab_allocate_buffer_t allocate_buffer,	     \
hurd_slab_deallocate_buffer_t deallocate_buffer,  \
hurd_##name##_slab_constructor_t constructor,     \
hurd_##name##_slab_destructor_t destructor,	     \
void *hook,					     \
hurd_##name##_slab_space_t *space)		     \
{									     \
union									     \
{									     \
hurd_##name##_slab_constructor_t t;					     \
hurd_slab_constructor_t u;						     \
} con;								     \
union									     \
{									     \
hurd_##name##_slab_destructor_t t;					     \
hurd_slab_destructor_t u;						     \
} des;								     \
union									     \
{									     \
hurd_##name##_slab_space_t *t;					     \
hurd_slab_space_t *u;						     \
} foo;								     \
con.t = constructor;							     \
des.t = destructor;							     \
foo.t = space;							     \
\
return hurd_slab_create(sizeof (element_type), __alignof__ (element_type), \
allocate_buffer, deallocate_buffer,		     \
con.u, des.u, hook, foo.u);			     \
}									     \
\
static inline error_t							     \
hurd_##name##_slab_free (hurd_##name##_slab_space_t space)		     \
{									     \
return hurd_slab_free (&space->space);				     \
}									     \
\
static inline error_t							     \
hurd_##name##_slab_init (hurd_##name##_slab_space_t space,		     \
hurd_slab_allocate_buffer_t allocate_buffer,	     \
hurd_slab_deallocate_buffer_t deallocate_buffer,    \
hurd_##name##_slab_constructor_t constructor,	     \
hurd_##name##_slab_destructor_t destructor,	     \
void *hook)					     \
{									     \
union									     \
{									     \
hurd_##name##_slab_constructor_t t;					     \
hurd_slab_constructor_t u;						     \
} con;								     \
union									     \
{									     \
hurd_##name##_slab_destructor_t t;					     \
hurd_slab_destructor_t u;						     \
} des;								     \
con.t = constructor;							     \
des.t = destructor;							     \
\
return hurd_slab_init (&space->space,					     \
sizeof (element_type), __alignof__ (element_type),  \
allocate_buffer, deallocate_buffer,		     \
con.u, des.u, hook);				     \
}									     \
\
static inline error_t							     \
hurd_##name##_slab_destroy (hurd_##name##_slab_space_t space)		     \
{									     \
return hurd_slab_destroy (&space->space);				     \
}									     \
\
static inline error_t							     \
hurd_##name##_slab_alloc (hurd_##name##_slab_space_t space,		     \
element_type **buffer)			     \
{									     \
union									     \
{									     \
element_type **e;							     \
void **v;								     \
} foo;								     \
foo.e = buffer;							     \
\
return hurd_slab_alloc (&space->space, foo.v);			     \
}									     \
\
static inline void							     \
hurd_##name##_slab_dealloc (hurd_##name##_slab_space_t space,		     \
element_type *buffer)			     \
{									     \
union									     \
{									     \
element_type *e;							     \
void *v;								     \
} foo;								     \
foo.e = buffer;							     \
\
hurd_slab_dealloc (&space->space, foo.v);				     \
}
#endif