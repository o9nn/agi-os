#ifndef _DDB_DB_ACCESS_H_
#define _DDB_DB_ACCESS_H_
#include <mach/boolean.h>
#include <machine/db_machdep.h>
#include <ddb/db_task_thread.h>
#include <machine/vm_param.h>
#define	DB_ACCESS_KERNEL	0
#define DB_ACCESS_CURRENT	1
#define DB_ACCESS_ANY		2
#ifndef	DB_ACCESS_LEVEL
#define DB_ACCESS_LEVEL		DB_ACCESS_KERNEL
#endif
#ifndef DB_VALID_KERN_ADDR
#define DB_VALID_KERN_ADDR(addr)	((addr) >= VM_MIN_KERNEL_ADDRESS \
&& (addr) < VM_MAX_KERNEL_ADDRESS)
#define DB_VALID_ADDRESS(addr,user)	((user != 0) ^ DB_VALID_KERN_ADDR(addr))
#define DB_PHYS_EQ(task1,addr1,task2,addr2)	0
#define DB_CHECK_ACCESS(addr,size,task)	db_is_current_task(task)
#endif
extern int db_access_level;
extern db_expr_t db_get_value(	db_addr_t addr,
int size,
boolean_t is_signed );
extern void	 db_put_value(	db_addr_t addr,
int size,
db_expr_t value );
extern db_expr_t db_get_task_value(	db_addr_t addr,
int size,
boolean_t is_signed,
task_t task );
extern void	 db_put_task_value(	db_addr_t addr,
int size,
db_expr_t value,
task_t task );
#endif