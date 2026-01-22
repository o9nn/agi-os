#if MACH_KDB
#include <machine/db_machdep.h>
#include <machine/setjmp.h>
#include <ddb/db_lex.h>
#include <ddb/db_break.h>
#include <ddb/db_command.h>
#include <ddb/db_cond.h>
#include <ddb/db_expr.h>
#include <ddb/db_output.h>
#include <kern/debug.h>
#define DB_MAX_COND 10
int db_ncond_free = DB_MAX_COND;
struct db_cond {
int c_size;
char c_cond_cmd[DB_LEX_LINE_SIZE];
} db_cond[DB_MAX_COND];
void
db_cond_free(db_thread_breakpoint_t bkpt)
{
if (bkpt->tb_cond > 0) {
db_cond[bkpt->tb_cond-1].c_size = 0;
db_ncond_free++;
bkpt->tb_cond = 0;
}
}
boolean_t
db_cond_check(db_thread_breakpoint_t bkpt)
{
struct db_cond *cp;
db_expr_t value;
int t;
jmp_buf_t db_jmpbuf;
extern jmp_buf_t *db_recover;
if (bkpt->tb_cond <= 0)
return(TRUE);
db_dot = PC_REGS(DDB_REGS);
db_prev = db_dot;
db_next = db_dot;
if (_setjmp(db_recover = &db_jmpbuf)) {
return(TRUE);
}
cp = &db_cond[bkpt->tb_cond - 1];
db_switch_input(cp->c_cond_cmd, cp->c_size);
if (!db_expression(&value)) {
db_printf("error: condition evaluation error\n");
return(TRUE);
}
if (value == 0 || --(bkpt->tb_count) > 0)
return(FALSE);
bkpt->tb_count = bkpt->tb_init_count;
if ((t = db_read_token()) != tEOL) {
db_unread_token(t);
return(db_exec_cmd_nest(0, 0));
}
return(TRUE);
}
void
db_cond_print(const db_thread_breakpoint_t bkpt)
{
char *p, *ep;
struct db_cond *cp;
if (bkpt->tb_cond <= 0)
return;
cp = &db_cond[bkpt->tb_cond-1];
p = cp->c_cond_cmd;
ep = p + cp->c_size;
while (p < ep) {
if (*p == '\n' || *p == 0)
break;
db_putchar(*p++);
}
}
void
db_cond_cmd(
db_expr_t addr,
int have_addr,
db_expr_t count,
const char * modif)
{
int c;
struct db_cond *cp;
char *p;
db_expr_t value;
db_thread_breakpoint_t bkpt;
if (db_read_token() != tHASH || db_read_token() != tNUMBER) {
db_printf("#<number> expected instead of \"%s\"\n", db_tok_string);
db_error(0);
return;
}
if ((bkpt = db_find_breakpoint_number(db_tok_number, 0)) == 0) {
db_printf("No such break point #%d\n", db_tok_number);
db_error(0);
return;
}
if (bkpt->tb_cond > 0) {
cp = &db_cond[bkpt->tb_cond - 1];
db_cond_free(bkpt);
} else {
if (db_ncond_free <= 0) {
db_error("Too many conditions\n");
return;
}
for (cp = db_cond; cp < &db_cond[DB_MAX_COND]; cp++)
if (cp->c_size == 0)
break;
if (cp >= &db_cond[DB_MAX_COND])
panic("bad db_cond_free");
}
for (c = db_read_char(); c == ' ' || c == '\t'; c = db_read_char());
for (p = cp->c_cond_cmd; c >= 0; c = db_read_char())
*p++ = c;
db_switch_input(cp->c_cond_cmd, p - cp->c_cond_cmd);
if (!db_expression(&value)) {
db_flush_lex();
return;
}
db_flush_lex();
db_ncond_free--;
cp->c_size = p - cp->c_cond_cmd;
bkpt->tb_cond = (cp - db_cond) + 1;
}
#endif