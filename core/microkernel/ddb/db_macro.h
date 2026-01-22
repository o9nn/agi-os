#ifndef _DDB_DB_MACRO_H_
#define _DDB_DB_MACRO_H_
#include <sys/types.h>
#include <ddb/db_variables.h>
extern void db_def_macro_cmd (
db_expr_t	addr,
boolean_t	have_addr,
db_expr_t	count,
const char *	modif);
extern void db_del_macro_cmd (
db_expr_t	addr,
boolean_t	have_addr,
db_expr_t	count,
const char *	modif);
extern void db_show_macro (
db_expr_t	addr,
boolean_t	have_addr,
db_expr_t	count,
const char *	modif);
extern int db_exec_macro (const char *name);
extern void db_arg_variable (
struct db_variable *vp,
db_expr_t *valuep,
int flag,
db_var_aux_param_t ap);
#endif