#ifndef _DDB_DB_PRINT_H_
#define _DDB_DB_PRINT_H_
#include <mach/boolean.h>
#include <machine/db_machdep.h>
void db_show_regs(
db_expr_t addr,
boolean_t have_addr,
db_expr_t count,
char *modif);
void db_show_one_task(
db_expr_t addr,
boolean_t have_addr,
db_expr_t count,
const char * modif);
void db_show_port_id(
db_expr_t addr,
boolean_t have_addr,
db_expr_t count,
const char * modif);
void db_show_one_thread(
db_expr_t addr,
int have_addr,
db_expr_t count,
const char * modif);
void db_show_all_tasks(
db_expr_t addr,
int have_addr,
db_expr_t count,
const char * modif);
void db_show_all_threads(
db_expr_t addr,
int have_addr,
db_expr_t count,
const char * modif);
void db_show_all_runqs(
db_expr_t addr,
int have_addr,
db_expr_t count,
const char * modif);
db_addr_t db_task_from_space(
ipc_space_t space,
int *task_id);
void db_print_thread(
thread_t thread,
int thread_id,
int flag);
#endif