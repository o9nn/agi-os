#ifndef	_DB_VARIABLES_H_
#define	_DB_VARIABLES_H_
#include <kern/thread.h>
#include <machine/db_machdep.h>
struct db_var_aux_param;
typedef struct db_var_aux_param	*db_var_aux_param_t;
struct db_variable {
char	*name;
db_expr_t *valuep;
void	(*fcn)(struct db_variable *, db_expr_t *, int, db_var_aux_param_t);
short	min_level;
short	max_level;
short	low;
short	high;
#define DB_VAR_GET	0
#define DB_VAR_SET	1
};
#define	FCN_NULL	((void (*)(struct db_variable *, db_expr_t *, int, db_var_aux_param_t))0)
#define DB_VAR_LEVEL	3
#define db_read_variable(vp, valuep)	\
db_read_write_variable(vp, valuep, DB_VAR_GET, 0)
#define db_write_variable(vp, valuep)	\
db_read_write_variable(vp, valuep, DB_VAR_SET, 0)
struct db_var_aux_param {
char		*modif;
short		level;
short		suffix[DB_VAR_LEVEL];
thread_t	thread;
};
extern struct db_variable	db_vars[];
extern struct db_variable	*db_evars;
extern struct db_variable	db_regs[];
extern struct db_variable	*db_eregs;
extern int db_get_variable(db_expr_t *valuep);
void db_set_cmd(void);
void db_read_write_variable(struct db_variable *, db_expr_t *, int, struct db_var_aux_param *);
#endif