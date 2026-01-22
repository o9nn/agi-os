#ifndef _DDB_DB_COMMAND_H_
#define _DDB_DB_COMMAND_H_
#if MACH_KDB
#include <machine/db_machdep.h>
#include <machine/setjmp.h>
extern void		db_command_loop(void);
extern boolean_t	db_option(const char *, int) __attribute__ ((pure));
extern void		db_error(const char *) __attribute__ ((noreturn));
extern db_addr_t	db_dot;
extern db_addr_t	db_last_addr;
extern db_addr_t	db_prev;
extern db_addr_t	db_next;
extern jmp_buf_t *	db_recover;
typedef void (*db_command_fun_t)(db_expr_t, boolean_t, db_expr_t, const char *);
struct db_command {
char *	name;
db_command_fun_t fcn;
int	flag;
#define	CS_OWN		0x1
#define	CS_MORE		0x2
#define	CS_SET_DOT	0x100
struct db_command *more;
};
extern boolean_t db_exec_cmd_nest(char *cmd, int size);
void db_fncall(void);
void db_help_cmd(void);
#endif
#endif