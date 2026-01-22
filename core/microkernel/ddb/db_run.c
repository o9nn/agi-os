#if MACH_KDB
#include <mach/boolean.h>
#include <machine/db_machdep.h>
#include <ddb/db_lex.h>
#include <ddb/db_break.h>
#include <ddb/db_access.h>
#include <ddb/db_run.h>
#include <ddb/db_task_thread.h>
#include <ddb/db_command.h>
#include <ddb/db_examine.h>
#include <ddb/db_output.h>
#include <ddb/db_watch.h>
#include <ddb/db_cond.h>
int	db_run_mode;
boolean_t	db_sstep_print;
int		db_loop_count;
int		db_call_depth;
int		db_inst_count;
int		db_last_inst_count;
int		db_load_count;
int		db_store_count;
boolean_t
db_stop_at_pc(
boolean_t	*is_breakpoint,
task_t		task)
{
db_addr_t		pc;
db_thread_breakpoint_t  bkpt;
db_clear_task_single_step(DDB_REGS, task);
db_clear_breakpoints();
db_clear_watchpoints();
pc = PC_REGS(DDB_REGS);
#ifdef	FIXUP_PC_AFTER_BREAK
if (*is_breakpoint) {
FIXUP_PC_AFTER_BREAK
pc = PC_REGS(DDB_REGS);
}
#endif
bkpt = db_find_thread_breakpoint_here(task, pc);
if (bkpt) {
if (db_cond_check(bkpt)) {
*is_breakpoint = TRUE;
return (TRUE);
}
}
*is_breakpoint = FALSE;
if (db_run_mode == STEP_INVISIBLE) {
db_run_mode = STEP_CONTINUE;
return (FALSE);
}
if (db_run_mode == STEP_COUNT) {
return (FALSE);
}
if (db_run_mode == STEP_ONCE) {
if (--db_loop_count > 0) {
if (db_sstep_print) {
db_print_loc_and_inst(pc, task);
}
return (FALSE);
}
}
if (db_run_mode == STEP_RETURN) {
db_expr_t ins = db_get_task_value(pc, sizeof(int), FALSE, task);
if (!inst_trap_return(ins) &&
(!inst_return(ins) || --db_call_depth != 0)) {
if (db_sstep_print) {
if (inst_call(ins) || inst_return(ins)) {
int i;
db_printf("[after %6d /%4d] ",
db_inst_count,
db_inst_count - db_last_inst_count);
db_last_inst_count = db_inst_count;
for (i = db_call_depth; --i > 0; )
db_printf("  ");
db_print_loc_and_inst(pc, task);
db_printf("\n");
}
}
if (inst_call(ins))
db_call_depth++;
return (FALSE);
}
}
if (db_run_mode == STEP_CALLT) {
db_expr_t ins = db_get_task_value(pc, sizeof(int), FALSE, task);
if (!inst_call(ins) &&
!inst_return(ins) &&
!inst_trap_return(ins)) {
return (FALSE);
}
}
if (db_find_breakpoint_here(task, pc))
return(FALSE);
db_run_mode = STEP_NONE;
return (TRUE);
}
void
db_restart_at_pc(
boolean_t watchpt,
task_t	  task)
{
db_addr_t pc = PC_REGS(DDB_REGS);
if ((db_run_mode == STEP_COUNT) ||
(db_run_mode == STEP_RETURN) ||
(db_run_mode == STEP_CALLT)) {
db_get_task_value(pc, sizeof(int), FALSE, task);
db_inst_count++;
db_load_count += inst_load(ins);
db_store_count += inst_store(ins);
#ifdef	SOFTWARE_SSTEP
db_addr_t brpc;
brpc = next_instr_address(pc, 1, task);
if ((brpc != pc) && (inst_branch(ins) || inst_call(ins))) {
db_get_task_value(brpc, sizeof(int), FALSE, task);
db_inst_count++;
db_load_count += inst_load(ins);
db_store_count += inst_store(ins);
}
#endif
}
if (db_run_mode == STEP_CONTINUE) {
if (watchpt || db_find_breakpoint_here(task, pc)) {
db_run_mode = STEP_INVISIBLE;
db_set_task_single_step(DDB_REGS, task);
} else {
db_set_breakpoints();
db_set_watchpoints();
}
} else {
db_set_task_single_step(DDB_REGS, task);
}
}
void
db_single_step(
db_regs_t *regs,
task_t	  task)
{
if (db_run_mode == STEP_CONTINUE) {
db_run_mode = STEP_INVISIBLE;
db_set_task_single_step(regs, task);
}
}
#ifdef	SOFTWARE_SSTEP
db_breakpoint_t	db_not_taken_bkpt = 0;
db_breakpoint_t	db_taken_bkpt = 0;
db_breakpoint_t __attribute__ ((pure))
db_find_temp_breakpoint(const task_t task, db_addr_t addr)
{
if (db_taken_bkpt && (db_taken_bkpt->address == addr) &&
db_taken_bkpt->task == task)
return db_taken_bkpt;
if (db_not_taken_bkpt && (db_not_taken_bkpt->address == addr) &&
db_not_taken_bkpt->task == task)
return db_not_taken_bkpt;
return 0;
}
void
db_set_task_single_step(
db_regs_t 	*regs,
task_t		task)
{
db_addr_t pc = PC_REGS(regs), brpc;
unsigned int	inst;
boolean_t       unconditional;
inst = db_get_task_value(pc, sizeof(int), FALSE, task);
if (inst_branch(inst) || inst_call(inst)) {
extern db_expr_t getreg_val();
brpc = branch_taken(inst, pc, getreg_val, regs);
if (brpc != pc) {
db_taken_bkpt = db_set_temp_breakpoint(task, brpc);
} else
db_taken_bkpt = 0;
pc = next_instr_address(pc,1,task);
}
unconditional = inst_unconditional_flow_transfer(inst);
pc = next_instr_address(pc,0,task);
if (!unconditional && db_find_breakpoint_here(task, pc) == 0) {
db_not_taken_bkpt = db_set_temp_breakpoint(task, pc);
}
else
db_not_taken_bkpt = 0;
}
void
db_clear_task_single_step(const db_regs_t *regs, task_t task)
{
if (db_taken_bkpt != 0) {
db_delete_temp_breakpoint(task, db_taken_bkpt);
db_taken_bkpt = 0;
}
if (db_not_taken_bkpt != 0) {
db_delete_temp_breakpoint(task, db_not_taken_bkpt);
db_not_taken_bkpt = 0;
}
}
#endif
extern int	db_cmd_loop_done;
void
db_single_step_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif)
{
boolean_t	print = FALSE;
if (count == -1)
count = 1;
if (modif[0] == 'p')
print = TRUE;
db_run_mode = STEP_ONCE;
db_loop_count = count;
db_sstep_print = print;
db_inst_count = 0;
db_last_inst_count = 0;
db_load_count = 0;
db_store_count = 0;
db_cmd_loop_done = 1;
}
void
db_trace_until_call_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif)
{
boolean_t	print = FALSE;
if (modif[0] == 'p')
print = TRUE;
db_run_mode = STEP_CALLT;
db_sstep_print = print;
db_inst_count = 0;
db_last_inst_count = 0;
db_load_count = 0;
db_store_count = 0;
db_cmd_loop_done = 1;
}
void
db_trace_until_matching_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif)
{
boolean_t	print = FALSE;
if (modif[0] == 'p')
print = TRUE;
db_run_mode = STEP_RETURN;
db_call_depth = 1;
db_sstep_print = print;
db_inst_count = 0;
db_last_inst_count = 0;
db_load_count = 0;
db_store_count = 0;
db_cmd_loop_done = 1;
}
void
db_continue_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif)
{
if (modif[0] == 'c')
db_run_mode = STEP_COUNT;
else
db_run_mode = STEP_CONTINUE;
db_inst_count = 0;
db_last_inst_count = 0;
db_load_count = 0;
db_store_count = 0;
db_cmd_loop_done = 1;
}
boolean_t
db_in_single_step(void)
{
return(db_run_mode != STEP_NONE && db_run_mode != STEP_CONTINUE);
}
#endif