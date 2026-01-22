#ifndef NDEBUG
#define MACH_ASSERT 1
#else
#define MACH_ASSERT 0
#endif
#include <mach_tr.h>
#define	TRACE_BUFFER	(MACH_TR)
#ifndef	_DDB_TR_H_
#define	_DDB_TR_H_
#if	TRACE_BUFFER
#include <machine/db_machdep.h>
#define	__ui__			(unsigned int)
#define	TR_INIT()		tr_init()
#define TR_SHOW(a,b,c)		show_tr((a),(b),(c))
#define	TR_DECL(funcname)	char	*__ntr_func_name__ = funcname
#define	tr1(msg)							\
tr(__ntr_func_name__, __FILE__, __LINE__, (msg),		\
0,0,0,0)
#define	tr2(msg,tag1)							\
tr(__ntr_func_name__, __FILE__, __LINE__, (msg),		\
__ui__(tag1),0,0,0)
#define	tr3(msg,tag1,tag2)						\
tr(__ntr_func_name__, __FILE__, __LINE__, (msg),		\
__ui__(tag1),__ui__(tag2),0,0)
#define	tr4(msg,tag1,tag2,tag3)						\
tr(__ntr_func_name__, __FILE__, __LINE__, (msg),		\
__ui__(tag1),__ui__(tag2),__ui__(tag3),0)
#define	tr5(msg,tag1,tag2,tag3,tag4)					\
tr(__ntr_func_name__, __FILE__, __LINE__, (msg),		\
__ui__(tag1),__ui__(tag2),__ui__(tag3),__ui__(tag4))
extern int tr_indent;
#define	tr_start()	tr_indent++
#define tr_stop()	tr_indent--
extern void	tr_init(void);
extern void	tr(
char		*funcname,
char		*file,
unsigned int	lineno,
char		*fmt,
unsigned int	tag1,
unsigned int	tag2,
unsigned int	tag3,
unsigned int	tag4);
extern void db_show_tr(
db_expr_t	addr,
boolean_t	have_addr,
db_expr_t	count,
char *		modif);
#else
#define	TR_INIT()
#define TR_SHOW(a,b,c)
#define	TR_DECL(funcname)
#define tr1(msg)
#define tr2(msg, tag1)
#define tr3(msg, tag1, tag2)
#define tr4(msg, tag1, tag2, tag3)
#define tr5(msg, tag1, tag2, tag3, tag4)
#define	tr_start()
#define tr_stop()
#endif
#endif