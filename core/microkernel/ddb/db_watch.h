#if MACH_KDB
#ifndef	_DDB_DB_WATCH_
#define	_DDB_DB_WATCH_
#include <mach/machine/vm_types.h>
#include <kern/task.h>
#include <machine/db_machdep.h>
typedef struct db_watchpoint {
task_t    task;
db_addr_t loaddr;
db_addr_t hiaddr;
struct db_watchpoint *link;
} *db_watchpoint_t;
extern boolean_t db_find_watchpoint(vm_map_t map, db_addr_t addr,
db_regs_t *regs);
extern void db_set_watchpoints(void);
extern void db_clear_watchpoints(void);
extern void db_set_watchpoint(const task_t task, db_addr_t addr, vm_size_t size);
extern void db_delete_watchpoint(const task_t task, db_addr_t addr);
extern void db_list_watchpoints(void);
void db_listwatch_cmd(
db_expr_t	addr,
boolean_t	have_addr,
db_expr_t	count,
const char *	modif);
void db_deletewatch_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif);
void db_watchpoint_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif);
#endif
#endif