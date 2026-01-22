#ifndef _DDB_DB_INPUT_H_
#define _DDB_DB_INPUT_H_
#include <sys/types.h>
extern void kdb_kintr(void);
extern int db_readline (char *lstart, int lsize);
extern void db_check_interrupt(void);
#endif