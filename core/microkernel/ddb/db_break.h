#ifndef	_DDB_DB_BREAK_H_
#define	_DDB_DB_BREAK_H_
#include <machine/db_machdep.h>
#include <kern/thread.h>
#include <kern/task.h>
#include <mach/boolean.h>
struct db_thread_breakpoint {
vm_offset_t tb_task_thd;
boolean_t tb_is_task;
short	 tb_number;
short	 tb_init_count;
short	 tb_count;
short	 tb_cond;
struct	 db_thread_breakpoint *tb_next;
};
typedef struct db_thread_breakpoint *db_thread_breakpoint_t;
struct db_breakpoint {
task_t	  task;
db_addr_t address;
db_thread_breakpoint_t threads;
int	flags;
#define	BKPT_SINGLE_STEP	0x2
#define	BKPT_TEMP		0x4
#define BKPT_USR_GLOBAL		0x8
#define BKPT_SET_IN_MEM		0x10
#define BKPT_1ST_SET		0x20
#define BKPT_EXTERNAL		0x40
vm_size_t	bkpt_inst;
struct db_breakpoint *link;
};
typedef struct db_breakpoint *db_breakpoint_t;
extern db_breakpoint_t	db_find_breakpoint( const task_t task, db_addr_t addr) __attribute__ ((pure));
extern boolean_t	db_find_breakpoint_here( const task_t task, db_addr_t addr);
extern void		db_set_breakpoints(void);
extern void		db_clear_breakpoints(void);
extern db_thread_breakpoint_t	db_find_thread_breakpoint_here
( const task_t task, db_addr_t addr );
extern db_thread_breakpoint_t	db_find_breakpoint_number
( int num, db_breakpoint_t *bkptp);
extern db_breakpoint_t	db_set_temp_breakpoint( task_t task, db_addr_t addr);
extern void		db_delete_temp_breakpoint
( task_t task, db_breakpoint_t bkpt);
extern db_breakpoint_t  db_set_breakpoint(const task_t task, db_addr_t addr,
int count, const thread_t thread,
boolean_t task_bpt);
void db_listbreak_cmd(
db_expr_t	addr,
boolean_t	have_addr,
db_expr_t	count,
const char	*modif);
void db_delete_cmd(
db_expr_t	addr,
boolean_t	have_addr,
db_expr_t	count,
const char *	modif);
void db_breakpoint_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif);
extern void db_check_breakpoint_valid(void);
#endif