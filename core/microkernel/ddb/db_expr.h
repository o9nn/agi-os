#ifndef	_DDB_DB_EXPR_H_
#define	_DDB_DB_EXPR_H_
#include <mach/boolean.h>
#include <machine/db_machdep.h>
int db_size_option(
const char	*modif,
boolean_t	*u_option,
boolean_t	*t_option);
int db_expression(db_expr_t *valuep);
#endif