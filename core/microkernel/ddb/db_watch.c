#if MACH_KDB
#include <mach/boolean.h>
#include <mach/vm_param.h>
#include <mach/machine/vm_types.h>
#include <mach/machine/vm_param.h>
#include <vm/vm_map.h>
#include <machine/db_machdep.h>
#include <machine/db_interface.h>
#include <ddb/db_command.h>
#include <ddb/db_lex.h>
#include <ddb/db_watch.h>
#include <ddb/db_access.h>
#include <ddb/db_expr.h>
#include <ddb/db_output.h>
#include <ddb/db_run.h>
#include <ddb/db_sym.h>
#include <ddb/db_task_thread.h>
boolean_t	db_watchpoints_inserted = TRUE;
#define	NWATCHPOINTS	100
struct db_watchpoint	db_watch_table[NWATCHPOINTS];
db_watchpoint_t		db_next_free_watchpoint = &db_watch_table[0];
db_watchpoint_t		db_free_watchpoints = 0;
db_watchpoint_t		db_watchpoint_list = 0;
extern vm_map_t		kernel_map;
static db_watchpoint_t
db_watchpoint_alloc(void)
{
db_watchpoint_t	watch;
if ((watch = db_free_watchpoints) != 0) {
db_free_watchpoints = watch->link;
return (watch);
}
if (db_next_free_watchpoint == &db_watch_table[NWATCHPOINTS]) {
db_printf("All watchpoints used.\n");
return (0);
}
watch = db_next_free_watchpoint;
db_next_free_watchpoint++;
return (watch);
}
static void
db_watchpoint_free(db_watchpoint_t watch)
{
watch->link = db_free_watchpoints;
db_free_watchpoints = watch;
}
void
db_set_watchpoint(
const task_t	task,
db_addr_t	addr,
vm_size_t	size)
{
db_watchpoint_t	watch;
for (watch = db_watchpoint_list; watch != 0; watch = watch->link) {
if (watch->task == task &&
(watch->loaddr == addr) &&
(watch->hiaddr == addr+size)) {
db_printf("Already set.\n");
return;
}
}
watch = db_watchpoint_alloc();
if (watch == 0) {
db_printf("Too many watchpoints.\n");
return;
}
watch->task = task;
watch->loaddr = addr;
watch->hiaddr = addr+size;
watch->link = db_watchpoint_list;
db_watchpoint_list = watch;
db_watchpoints_inserted = FALSE;
}
void
db_delete_watchpoint(const task_t task, db_addr_t addr)
{
db_watchpoint_t	watch;
db_watchpoint_t	*prev;
for (prev = &db_watchpoint_list; (watch = *prev) != 0;
prev = &watch->link) {
if (watch->task == task &&
(watch->loaddr <= addr) &&
(addr < watch->hiaddr)) {
*prev = watch->link;
db_watchpoint_free(watch);
return;
}
}
db_printf("Not set.\n");
}
void
db_list_watchpoints(void)
{
db_watchpoint_t watch;
int	 	task_id;
if (db_watchpoint_list == 0) {
db_printf("No watchpoints set\n");
return;
}
db_printf("Space      Address  Size\n");
for (watch = db_watchpoint_list; watch != 0; watch = watch->link)  {
if (watch->task == TASK_NULL)
db_printf("kernel  ");
else {
task_id = db_lookup_task(watch->task);
if (task_id < 0)
db_printf("%*X", 2*sizeof(vm_offset_t), watch->task);
else
db_printf("task%-3d ", task_id);
}
db_printf("  %*X  %X\n", 2*sizeof(vm_offset_t), watch->loaddr,
watch->hiaddr - watch->loaddr);
}
}
static int
db_get_task(const char *modif, task_t *taskp, db_addr_t addr)
{
task_t		task = TASK_NULL;
db_expr_t	value;
boolean_t	user_space;
user_space = db_option(modif, 'T');
if (user_space) {
if (db_expression(&value)) {
task = (task_t)value;
if (db_lookup_task(task) < 0) {
db_printf("bad task address %X\n", task);
return(-1);
}
} else {
task = db_default_task;
if (task == TASK_NULL) {
if ((task = db_current_task()) == TASK_NULL) {
db_printf("no task\n");
return(-1);
}
}
}
}
if (!DB_VALID_ADDRESS(addr, user_space)) {
db_printf("Address %#X is not in %s space\n", addr,
(user_space)? "user": "kernel");
return(-1);
}
*taskp = task;
return(0);
}
void
db_deletewatch_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif)
{
task_t		task;
if (db_get_task(modif, &task, addr) < 0)
return;
db_delete_watchpoint(task, addr);
}
void
db_watchpoint_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif)
{
vm_size_t	size;
db_expr_t	value;
task_t		task;
if (db_get_task(modif, &task, addr) < 0)
return;
if (db_expression(&value))
size = (vm_size_t) value;
else
size = sizeof(int);
db_set_watchpoint(task, addr, size);
}
void
db_listwatch_cmd(
db_expr_t	addr,
int		have_addr,
db_expr_t	count,
const char *	modif)
{
db_list_watchpoints();
}
void
db_set_watchpoints(void)
{
db_watchpoint_t		watch;
vm_map_t		map;
unsigned hw_idx = 0;
if (!db_watchpoints_inserted) {
for (watch = db_watchpoint_list; watch != 0; watch = watch->link) {
if (db_set_hw_watchpoint(watch, hw_idx)) {
hw_idx++;
continue;
}
map = (watch->task)? watch->task->map: kernel_map;
pmap_protect(map->pmap,
trunc_page(watch->loaddr),
round_page(watch->hiaddr),
VM_PROT_READ);
}
db_watchpoints_inserted = TRUE;
}
}
void
db_clear_watchpoints(void)
{
unsigned hw_idx = 0;
while (db_clear_hw_watchpoint(hw_idx))
hw_idx++;
db_watchpoints_inserted = FALSE;
}
boolean_t
db_find_watchpoint(
vm_map_t	map,
db_addr_t	addr,
db_regs_t	*regs)
{
db_watchpoint_t watch;
db_watchpoint_t found = 0;
task_t		task_space;
task_space = (map == kernel_map)? TASK_NULL: db_current_task();
for (watch = db_watchpoint_list; watch != 0; watch = watch->link) {
if (watch->task == task_space) {
if ((watch->loaddr <= addr) && (addr < watch->hiaddr))
return (TRUE);
else if ((trunc_page(watch->loaddr) <= addr) &&
(addr < round_page(watch->hiaddr)))
found = watch;
}
}
if (found) {
db_watchpoints_inserted = FALSE;
db_single_step(regs, task_space);
}
return (FALSE);
}
#endif