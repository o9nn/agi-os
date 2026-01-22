#ifndef _DDB_DB_COND_H_
#define _DDB_DB_COND_H_
#include <sys/types.h>
#include <machine/db_machdep.h>
extern void db_cond_free (const db_thread_breakpoint_t bkpt);
extern boolean_t db_cond_check (db_thread_breakpoint_t bkpt);
extern void db_cond_print (db_thread_breakpoint_t bkpt);
extern void db_cond_cmd (
db_expr_t	addr,
boolean_t	have_addr,
db_expr_t	count,
const char *	modif);
#endif