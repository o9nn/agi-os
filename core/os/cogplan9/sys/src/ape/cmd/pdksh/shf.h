#ifndef SHF_H
# define SHF_H
#define SHF_BSIZE	512
#define shf_fileno(shf)	((shf)->fd)
#define shf_setfileno(shf,nfd)	((shf)->fd = (nfd))
#define shf_getc(shf) ((shf)->rnleft > 0 ? (shf)->rnleft--, *(shf)->rp++ : \
shf_getchar(shf))
#define shf_putc(c, shf)	((shf)->wnleft == 0 ? shf_putchar((c), (shf)) \
: ((shf)->wnleft--, *(shf)->wp++ = (c)))
#define shf_eof(shf)		((shf)->flags & SHF_EOF)
#define shf_error(shf)		((shf)->flags & SHF_ERROR)
#define shf_errno(shf)		((shf)->errno_)
#define shf_clearerr(shf)	((shf)->flags &= ~(SHF_EOF | SHF_ERROR))
#define SHF_RD		0x0001
#define SHF_WR		0x0002
#define SHF_RDWR	  (SHF_RD|SHF_WR)
#define SHF_ACCMODE	  0x0003
#define SHF_GETFL	0x0004
#define SHF_UNBUF	0x0008
#define SHF_CLEXEC	0x0010
#define SHF_MAPHI	0x0020
#define SHF_DYNAMIC	0x0040
#define SHF_INTERRUPT	0x0080
#define SHF_STRING	0x0100
#define SHF_ALLOCS	0x0200
#define SHF_ALLOCB	0x0400
#define SHF_ERROR	0x0800
#define SHF_EOF		0x1000
#define SHF_READING	0x2000
#define SHF_WRITING	0x4000
struct shf {
int flags;
unsigned char *rp;
int rbsize;
int rnleft;
unsigned char *wp;
int wbsize;
int wnleft;
unsigned char *buf;
int fd;
int errno_;
int bsize;
Area *areap;
};
extern struct shf shf_iob[];
struct shf *shf_open	ARGS((const char *name, int oflags, int mode,
int sflags));
struct shf *shf_fdopen	ARGS((int fd, int sflags, struct shf *shf));
struct shf *shf_reopen  ARGS((int fd, int sflags, struct shf *shf));
struct shf *shf_sopen	ARGS((char *buf, int bsize, int sflags,
struct shf *shf));
int	    shf_close	ARGS((struct shf *shf));
int	    shf_fdclose	ARGS((struct shf *shf));
char	   *shf_sclose	ARGS((struct shf *shf));
int	    shf_finish	ARGS((struct shf *shf));
int	    shf_flush	ARGS((struct shf *shf));
int	    shf_seek	ARGS((struct shf *shf, off_t where, int from));
int	    shf_read	ARGS((char *buf, int bsize, struct shf *shf));
char	   *shf_getse	ARGS((char *buf, int bsize, struct shf *shf));
int	    shf_getchar	ARGS((struct shf *shf));
int	    shf_ungetc	ARGS((int c, struct shf *shf));
int	    shf_putchar	ARGS((int c, struct shf *shf));
int	    shf_puts	ARGS((const char *s, struct shf *shf));
int	    shf_write	ARGS((const char *buf, int nbytes, struct shf *shf));
int	    shf_fprintf ARGS((struct shf *shf, const char *fmt, ...));
int	    shf_snprintf ARGS((char *buf, int bsize, const char *fmt, ...));
char	    *shf_smprintf ARGS((const char *fmt, ...));
int	    shf_vfprintf ARGS((struct shf *, const char *fmt, va_list args));
#endif