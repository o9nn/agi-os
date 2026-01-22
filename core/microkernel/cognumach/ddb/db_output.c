#if MACH_KDB
#include <kern/printf.h>
#include <stdarg.h>
#include <mach/boolean.h>
#include <machine/db_machdep.h>
#include <device/cons.h>
#include <ddb/db_command.h>
#include <ddb/db_lex.h>
#include <ddb/db_output.h>
#include <ddb/db_input.h>
#ifndef	DB_MAX_LINE
#define	DB_MAX_LINE		24
#define DB_MAX_WIDTH		80
#endif
#define DB_MIN_MAX_WIDTH	20
#define DB_MIN_MAX_LINE		3
#define CTRL(c)			((c) & 0xff)
int	db_output_position = 0;
int	db_output_line = 0;
int	db_last_non_space = 0;
int	db_tab_stop_width = 8;
#define	NEXT_TAB(i) \
((((i) + db_tab_stop_width) / db_tab_stop_width) * db_tab_stop_width)
int	db_max_line = DB_MAX_LINE;
int	db_max_width = DB_MAX_WIDTH;
void
db_force_whitespace(void)
{
int last_print, next_tab;
last_print = db_last_non_space;
while (last_print < db_output_position) {
next_tab = NEXT_TAB(last_print);
if (next_tab <= db_output_position) {
cnputc('\t');
last_print = next_tab;
}
else {
cnputc(' ');
last_print++;
}
}
db_last_non_space = db_output_position;
}
static void
db_more(void)
{
char *p;
boolean_t quit_output = FALSE;
for (p = "--db_more--"; *p; p++)
cnputc(*p);
switch(cngetc()) {
case ' ':
db_output_line = 0;
break;
case 'q':
case CTRL('c'):
db_output_line = 0;
quit_output = TRUE;
break;
default:
db_output_line--;
break;
}
p = "\b\b\b\b\b\b\b\b\b\b\b           \b\b\b\b\b\b\b\b\b\b\b";
while (*p)
cnputc(*p++);
if (quit_output) {
db_error(0);
}
}
void
db_putchar(int c)
{
if (db_max_line >= DB_MIN_MAX_LINE && db_output_line >= db_max_line-1)
db_more();
if (c > ' ' && c <= '~') {
db_force_whitespace();
cnputc(c);
db_output_position++;
if (db_max_width >= DB_MIN_MAX_WIDTH
&& db_output_position >= db_max_width) {
cnputc('\n');
db_output_position = 0;
db_last_non_space = 0;
db_output_line++;
}
db_last_non_space = db_output_position;
}
else if (c == '\n') {
cnputc(c);
db_output_position = 0;
db_last_non_space = 0;
db_output_line++;
db_check_interrupt();
}
else if (c == '\t') {
db_output_position = NEXT_TAB(db_output_position);
}
else if (c == ' ') {
db_output_position++;
}
else if (c == '\007') {
cnputc(c);
}
}
static void
db_id_putc(char c, vm_offset_t dummy)
{
db_putchar(c);
}
int __attribute__ ((pure))
db_print_position(void)
{
return (db_output_position);
}
void db_end_line(void)
{
if (db_output_position >= db_max_width-1)
db_printf("\n");
}
int
db_printf(const char *fmt, ...)
{
va_list	listp;
va_start(listp, fmt);
_doprnt(fmt, listp, db_id_putc, db_radix, 0);
va_end(listp);
return 0;
}
#endif