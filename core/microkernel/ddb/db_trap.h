#ifndef _DDB_DB_TRAP_H_
#define _DDB_DB_TRAP_H_
#include <sys/types.h>
#include <machine/db_machdep.h>
extern void db_task_trap (
int type,
int code,
boolean_t user_space);
extern void db_trap (int type, int code);
#endif