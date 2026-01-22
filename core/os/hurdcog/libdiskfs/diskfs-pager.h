#ifndef _HURD_DISKFS_PAGER_H
#define _HURD_DISKFS_PAGER_H 1
#include <hurd/pager.h>
#include <hurd/ports.h>
#include <setjmp.h>
#include <pthread.h>
#include <errno.h>
#include <assert-backtrace.h>
#include <stdlib.h>
extern __thread struct disk_image_user *diskfs_exception_diu;
extern void diskfs_start_disk_pager (struct user_pager_info *info,
struct port_bucket *pager_bucket,
int may_cache, int notify_on_evict,
size_t size, void **image);
extern struct pager *diskfs_disk_pager;
extern struct pager_requests *diskfs_disk_pager_requests;
struct disk_image_user
{
jmp_buf env;
struct disk_image_user *next;
};
#define diskfs_catch_exception()					      \
({									      \
struct disk_image_user *diu = alloca (sizeof *diu);			      \
error_t err;							      \
diu->next = diskfs_exception_diu;					      \
err = setjmp (diu->env);						      \
if (err == 0)							      \
diskfs_exception_diu = diu;					      \
err;								      \
})
#define diskfs_end_catch_exception()					      \
({									      \
struct disk_image_user *diu = diskfs_exception_diu; 		      \
diskfs_exception_diu = diu->next;					      \
})
#endif