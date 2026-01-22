#ifndef DEFS_H_
#define DEFS_H_
#include <sys/types.h>
#include <sys/param.h>
#ifdef HAVE_INTTYPES_H
#include <inttypes.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define NEWARRAY(type,ptr,size,where,action) do {			\
if ((ptr = calloc(sizeof(type), (unsigned)(size))) == NULL) {	\
(void) fprintf(stderr, "%s: can't allocate %lu bytes\n", \
where, (unsigned long)(size * sizeof(type)));	\
action;							\
}								\
} while(  0)
#define RENEW(type,ptr,size,where,action) do {				\
type *_newptr;							\
_newptr = realloc(ptr, (size_t)(sizeof(type) * (size)));	\
if (_newptr == NULL) {						\
(void) fprintf(stderr, "%s: can't realloc %lu bytes\n",	\
where, (unsigned long)(size * sizeof(type)));	\
action;							\
} else {							\
ptr = _newptr;						\
}								\
} while(  0)
#define NEW(type, ptr, where, action)	NEWARRAY(type, ptr, 1, where, action)
#define FREE(ptr)	(void) free(ptr)
#define ALLOC(type, v, size, c, init, incr, where, action) do {		\
uint32_t	_newsize = size;				\
if (size == 0) {						\
_newsize = init;					\
NEWARRAY(type, v, _newsize, where ": new", action);	\
} else if (c == size) {						\
_newsize = size + incr;					\
RENEW(type, v, _newsize, where ": renew", action);	\
}								\
size = _newsize;						\
} while(  0)
#define DEFINE_ARRAY(name, type)					\
typedef struct name {							\
uint32_t	c;						\
uint32_t	size;						\
type	       *v;						\
} name
#endif