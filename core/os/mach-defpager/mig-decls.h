#ifndef __MACH_DEFPAGER_MIG_DECLS_H__
#define __MACH_DEFPAGER_MIG_DECLS_H__
#include "priv.h"
static inline struct dstruct * __attribute__ ((unused))
begin_using_default_pager (mach_port_t port)
{
return (default_pager_t) hurd_ihash_find (&all_pagers.htable,
(hurd_ihash_key_t) port);
}
static inline struct dstruct * __attribute__ ((unused))
begin_using_default_pager_payload (uintptr_t payload)
{
return (default_pager_t) payload;
}
#endif