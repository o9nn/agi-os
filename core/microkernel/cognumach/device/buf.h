#ifndef _DEVICE_BUF_H_
#define _DEVICE_BUF_H_
#include <device/io_req.h>
#define	buf	io_req
#define	b_flags		io_op
#define	b_bcount	io_count
#define	b_error		io_error
#define	b_dev		io_unit
#define	b_blkno		io_recnum
#define	b_resid		io_residual
#define	b_un		io_un
#define	b_addr		data
#define	av_forw		io_next
#define	av_back		io_prev
#define b_physblock     io_physrec
#define b_blocktotal    io_rectotal
#define	b_actf		io_next
#define	b_actl		io_prev
#define	b_forw		io_link
#define	b_back		io_rlink
#define	b_active	io_count
#define	b_errcnt	io_residual
#define	b_bufsize	io_alloc_size
#define	B_WRITE		IO_WRITE
#define	B_READ		IO_READ
#define	B_OPEN		IO_OPEN
#define	B_DONE		IO_DONE
#define	B_ERROR		IO_ERROR
#define	B_BUSY		IO_BUSY
#define	B_WANTED	IO_WANTED
#define	B_BAD		IO_BAD
#define	B_CALL		IO_CALL
#define	B_MD1		IO_SPARE_START
extern void minphys(io_req_t);
#define	biodone	iodone
#define biowait iowait
#endif