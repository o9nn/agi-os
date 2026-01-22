#ifndef LINUX_NBD_H
#define LINUX_NBD_H
#define NBD_SET_SOCK	_IO( 0xab, 0 )
#define NBD_SET_BLKSIZE	_IO( 0xab, 1 )
#define NBD_SET_SIZE	_IO( 0xab, 2 )
#define NBD_DO_IT	_IO( 0xab, 3 )
#define NBD_CLEAR_SOCK	_IO( 0xab, 4 )
#define NBD_CLEAR_QUE	_IO( 0xab, 5 )
#define NBD_PRINT_DEBUG	_IO( 0xab, 6 )
#define NBD_SET_SIZE_BLOCKS	_IO( 0xab, 7 )
#ifdef MAJOR_NR
#include <linux/locks.h>
#include <asm/semaphore.h>
#define LOCAL_END_REQUEST
#include <linux/blk.h>
#ifdef PARANOIA
extern int requests_in;
extern int requests_out;
#endif
static void
nbd_end_request(struct request *req)
{
#ifdef PARANOIA
requests_out++;
#endif
if (end_that_request_first( req, !req->errors, "nbd" ))
return;
end_that_request_last( req );
}
#define MAX_NBD 128
struct nbd_device {
int refcnt;
int flags;
int harderror;
#define NBD_READ_ONLY 0x0001
#define NBD_WRITE_NOCHK 0x0002
#define NBD_INITIALISED 0x0004
struct socket * sock;
struct file * file;
int magic;
struct request *head;
struct request *tail;
struct semaphore queue_lock;
};
#endif
#define NBD_REQUEST_MAGIC 0x25609513
#define NBD_REPLY_MAGIC 0x67446698
struct nbd_request {
u32 magic;
u32 type;
char handle[8];
u64 from;
u32 len;
}
#ifdef __GNUC__
__attribute__ ((packed))
#endif
;
struct nbd_reply {
u32 magic;
u32 error;
char handle[8];
};
#endif