#ifndef	_DEVICE_CIRBUF_H_
#define	_DEVICE_CIRBUF_H_
struct cirbuf {
char *	c_start;
char *	c_end;
char *	c_cf;
char *	c_cl;
short	c_cc;
short	c_hog;
};
extern int	putc(int, struct cirbuf *);
extern int	getc(struct cirbuf *);
extern int	q_to_b(struct cirbuf *, char *, int);
extern int	b_to_q(char *, int, struct cirbuf *);
extern void	ndflush(struct cirbuf *, int);
extern void	cb_clear(struct cirbuf *);
extern void	cb_alloc(struct cirbuf *, vm_size_t);
extern void	cb_free(struct cirbuf *);
#endif