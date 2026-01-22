#ifndef	_I386_SETJMP_H_
#define	_I386_SETJMP_H_
typedef	struct jmp_buf {
#ifdef __i386__
int	jmp_buf[6];
#else
long	jmp_buf[8];
#endif
} jmp_buf_t;
extern int _setjmp(jmp_buf_t*);
extern void _longjmp(jmp_buf_t*, int) __attribute__ ((noreturn));
#endif