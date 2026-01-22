#if MACH_KDB
#if	NCPUS > 1
#include <mach/boolean.h>
#include <mach/machine.h>
#include <kern/cpu_number.h>
#include <kern/lock.h>
#include <machine/db_machdep.h>
#include <machine/db_interface.h>
#include <ddb/db_command.h>
#include <ddb/db_input.h>
#include <ddb/db_run.h>
#include <ddb/db_mp.h>
#include <ddb/db_output.h>
int	db_spl;
def_simple_lock_irq_data(static,db_lock)
volatile int	db_cpu = -1;
int	db_active[NCPUS] = { 0 };
int	db_slave[NCPUS] = { 0 };
boolean_t	db_enter_debug = FALSE;
boolean_t
db_enter(void)
{
int	mycpu = cpu_number();
db_active[mycpu]++;
db_spl = lock_db();
if (db_enter_debug)
db_printf(
"db_enter: cpu %d[%d], master %d, db_cpu %d, run mode %d\n",
mycpu, db_slave[mycpu], master_cpu, db_cpu, db_run_mode);
if (db_cpu == -1 && !db_slave[mycpu]) {
remote_db();
db_cpu = mycpu;
return TRUE;
}
else if (db_cpu == mycpu)
return TRUE;
else
return FALSE;
}
void
db_leave(void)
{
int	mycpu = cpu_number();
if (db_run_mode == STEP_CONTINUE)
db_cpu = -1;
if (db_slave[mycpu])
db_slave[mycpu]--;
if (db_enter_debug)
db_printf("db_leave: cpu %d[%d], db_cpu %d, run_mode %d\n",
mycpu, db_slave[mycpu], db_cpu, db_run_mode);
unlock_db(db_spl);
db_active[mycpu]--;
}
void
remote_db(void) {
int	my_cpu = cpu_number();
int	i;
for (i = 0; i < NCPUS; i++) {
if (i != my_cpu &&
machine_slot[i].is_cpu &&
machine_slot[i].running)
{
cpu_interrupt_to_db(i);
}
}
}
#ifdef	__STDC__
#define DB_SAVE(type, name) extern type name; type name##_save = name
#define DB_RESTORE(name) name = name##_save
#else
#define DB_SAVE(type, name) extern type name; type name_save = name
#define DB_RESTORE(name) name = name_save
#endif
#define DB_SAVE_CTXT() \
DB_SAVE(int, db_run_mode); \
DB_SAVE(boolean_t, db_sstep_print); \
DB_SAVE(int, db_loop_count); \
DB_SAVE(int, db_call_depth); \
DB_SAVE(int, db_inst_count); \
DB_SAVE(int, db_last_inst_count); \
DB_SAVE(int, db_load_count); \
DB_SAVE(int, db_store_count); \
DB_SAVE(boolean_t, db_cmd_loop_done); \
DB_SAVE(jmp_buf_t *, db_recover); \
DB_SAVE(db_addr_t, db_dot); \
DB_SAVE(db_addr_t, db_last_addr); \
DB_SAVE(db_addr_t, db_prev); \
DB_SAVE(db_addr_t, db_next); \
SAVE_DDB_REGS
#define DB_RESTORE_CTXT() \
DB_RESTORE(db_run_mode); \
DB_RESTORE(db_sstep_print); \
DB_RESTORE(db_loop_count); \
DB_RESTORE(db_call_depth); \
DB_RESTORE(db_inst_count); \
DB_RESTORE(db_last_inst_count); \
DB_RESTORE(db_load_count); \
DB_RESTORE(db_store_count); \
DB_RESTORE(db_cmd_loop_done); \
DB_RESTORE(db_recover); \
DB_RESTORE(db_dot); \
DB_RESTORE(db_last_addr); \
DB_RESTORE(db_prev); \
DB_RESTORE(db_next); \
RESTORE_DDB_REGS
void
db_on(int cpu)
{
DB_SAVE_CTXT();
if (cpu < 0 || cpu >= NCPUS || !db_active[cpu])
return;
db_cpu = cpu;
unlock_db(db_spl);
db_spl = lock_db();
DB_RESTORE_CTXT();
if (db_cpu == -1)
db_continue_cmd(0, 0, 0, "");
}
void
remote_db_enter(void)
{
db_slave[cpu_number()]++;
kdb_kintr();
}
int
lock_db(void)
{
int	my_cpu = cpu_number();
int	s;
for (;;) {
#if	CONSOLE_ON_MASTER
if (my_cpu == master_cpu) {
db_console();
}
#endif
if (db_cpu != -1 && db_cpu != my_cpu)
continue;
#if	CONSOLE_ON_MASTER
if (my_cpu == master_cpu) {
if (!(s = simple_lock_try_irq(&db_lock)))
continue;
}
else {
s = simple_lock_irq(&db_lock);
}
#else
s = simple_lock_irq(&db_lock);
#endif
if (db_cpu == -1 || db_cpu == my_cpu)
break;
unlock_db(s);
}
return s;
}
void
unlock_db(int s)
{
simple_unlock_irq(s, &db_lock);
}
#if CONSOLE_ON_MASTER
void
db_console(void)
{
if (i_bit(CBUS_PUT_CHAR, my_word)) {
volatile u_char c = cbus_ochar;
i_bit_clear(CBUS_PUT_CHAR, my_word);
cnputc(c);
} else if (i_bit(CBUS_GET_CHAR, my_word)) {
if (cbus_wait_char)
cbus_ichar = cngetc();
else
cbus_ichar = cnmaygetc();
i_bit_clear(CBUS_GET_CHAR, my_word);
#ifndef	notdef
} else if (!cnmaygetc()) {
#else
} else if (com_is_char() && !com_getc(TRUE)) {
#endif
simple_unlock(&db_lock);
db_cpu = my_cpu;
}
}
#endif
#endif
#endif