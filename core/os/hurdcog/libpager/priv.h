#ifndef _LIBPAGER_PRIV_H
#define _LIBPAGER_PRIV_H
#include <mach.h>
#include <hurd.h>
#include <sys/mman.h>
#include <pthread.h>
#include "pager.h"
#include <hurd/ports.h>
struct pager
{
struct port_info port;
struct user_pager_info *upi;
enum
{
NOTINIT,
NORMAL,
SHUTDOWN,
} pager_state;
pthread_mutex_t interlock;
pthread_cond_t wakeup;
struct lock_request *lock_requests;
struct attribute_request *attribute_requests;
boolean_t may_cache;
memory_object_copy_strategy_t copy_strategy;
boolean_t notify_on_evict;
memory_object_control_t memobjcntl;
memory_object_name_t memobjname;
int noterm;
int termwaiting:1;
#ifdef KERNEL_INIT_RACE
struct pending_init *init_head, *init_tail;
#endif
short *pagemap;
vm_size_t pagemapsize;
};
struct lock_request
{
struct lock_request *next, **prevp;
vm_address_t start, end;
int pending_writes;
int locks_pending;
int threads_waiting;
};
struct attribute_request
{
struct attribute_request *next, **prevp;
boolean_t may_cache;
memory_object_copy_strategy_t copy_strategy;
int threads_waiting;
int attrs_pending;
};
#ifdef KERNEL_INIT_RACE
struct pending_init
{
mach_port_t control;
mach_port_t name;
struct pending_init *next;
};
#endif
enum page_errors
{
PAGE_NOERR,
PAGE_ENOSPC,
PAGE_EIO,
PAGE_EDQUOT,
};
extern int _pager_page_errors[];
#define PM_WRITEWAIT  0x0200
#define PM_INIT       0x0100
#define PM_INCORE     0x0080
#define PM_PAGINGOUT  0x0040
#define PM_PAGEINWAIT 0x0020
#define PM_INVALID    0x0010
#define PM_ERROR(byte) (((byte) & 0xc) >> 2)
#define SET_PM_ERROR(byte,err) (((byte) & ~0xc) | ((err) << 2))
#define PM_NEXTERROR(byte) ((byte) & 0x3)
#define SET_PM_NEXTERROR(byte,err) (((byte) & ~0x3) | (err))
extern struct port_class *_pager_class;
void _pager_block_termination (struct pager *);
void _pager_allow_termination (struct pager *);
error_t _pager_pagemap_resize (struct pager *, vm_address_t);
void _pager_mark_next_request_error (struct pager *, vm_address_t,
vm_size_t, error_t);
void _pager_mark_object_error (struct pager *, vm_address_t,
vm_size_t, error_t);
void _pager_lock_object (struct pager *, vm_offset_t, vm_size_t, int, int,
vm_prot_t, int);
void _pager_free_structure (struct pager *);
void _pager_clean (void *arg);
void _pager_real_dropweak (void *arg);
#endif