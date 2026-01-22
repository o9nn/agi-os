#ifndef _DDB_DB_OUTPUT_H_
#define _DDB_DB_OUTPUT_H_
extern void	db_force_whitespace(void);
extern int	db_print_position(void) __attribute__ ((pure));
extern void	db_end_line(void);
extern int	db_printf(const char *fmt, ...);
#define kdbprintf db_printf
extern void	db_putchar(int c);
#endif