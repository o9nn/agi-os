#ifndef _DDB_DB_MP_H_
#define _DDB_DB_MP_H_
void	remote_db(void);
int	lock_db(void);
void	unlock_db(int);
void	db_on(int i);
#if CONSOLE_ON_MASTER
void db_console(void);
#endif
boolean_t db_enter(void);
void remote_db_enter(void);
void db_leave(void);
#endif