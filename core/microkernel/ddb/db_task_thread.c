#if MACH_KDB
#include <machine/db_machdep.h>
#include <ddb/db_command.h>
#include <ddb/db_expr.h>
#include <ddb/db_lex.h>
#include <ddb/db_output.h>
#include <ddb/db_task_thread.h>
#include <ddb/db_variables.h>
#define DB_MAX_TASKID 0x10000
#define DB_MAX_THREADID 0x10000
#define DB_MAX_PSETS 0x10000
task_t db_default_task;
thread_t db_default_thread;
int
db_lookup_task(const task_t target_task)
{
task_t task;
int task_id;
processor_set_t pset;
int npset = 0;
task_id = 0;
if (queue_first(&all_psets) == 0)
return(-1);
queue_iterate(&all_psets, pset, processor_set_t, all_psets) {
if (npset++ >= DB_MAX_PSETS)
return(-1);
if (queue_first(&pset->tasks) == 0)
continue;
queue_iterate(&pset->tasks, task, task_t, pset_tasks) {
if (target_task == task)
return(task_id);
if (task_id++ >= DB_MAX_TASKID)
return(-1);
}
}
return(-1);
}
int
db_lookup_task_thread(const task_t task, const thread_t target_thread)
{
thread_t thread;
int thread_id;
thread_id = 0;
if (queue_first(&task->thread_list) == 0)
return(-1);
queue_iterate(&task->thread_list, thread, thread_t, thread_list) {
if (target_thread == thread)
return(thread_id);
if (thread_id++ >= DB_MAX_THREADID)
return(-1);
}
return(-1);
}
int
db_lookup_thread(const thread_t target_thread)
{
int thread_id;
task_t task;
processor_set_t pset;
int ntask = 0;
int npset = 0;
if (queue_first(&all_psets) == 0)
return(-1);
queue_iterate(&all_psets, pset, processor_set_t, all_psets) {
if (npset++ >= DB_MAX_PSETS)
return(-1);
if (queue_first(&pset->tasks) == 0)
continue;
queue_iterate(&pset->tasks, task, task_t, pset_tasks) {
if (ntask++ > DB_MAX_TASKID)
return(-1);
if (task->thread_count == 0)
continue;
thread_id = db_lookup_task_thread(task, target_thread);
if (thread_id >= 0)
return(thread_id);
}
}
return(-1);
}
boolean_t
db_check_thread_address_valid(const thread_t thread)
{
if (db_lookup_thread(thread) < 0) {
db_printf("Bad thread address 0x%x\n", thread);
db_flush_lex();
return(FALSE);
} else
return(TRUE);
}
static task_t
db_lookup_task_id(int task_id)
{
task_t task;
processor_set_t pset;
int npset = 0;
if (task_id > DB_MAX_TASKID)
return(TASK_NULL);
if (queue_first(&all_psets) == 0)
return(TASK_NULL);
queue_iterate(&all_psets, pset, processor_set_t, all_psets) {
if (npset++ >= DB_MAX_PSETS)
return(TASK_NULL);
if (queue_first(&pset->tasks) == 0)
continue;
queue_iterate(&pset->tasks, task, task_t, pset_tasks) {
if (task_id-- <= 0)
return(task);
}
}
return(TASK_NULL);
}
static thread_t
db_lookup_thread_id(
task_t task,
int thread_id)
{
thread_t thread;
if (thread_id > DB_MAX_THREADID)
return(THREAD_NULL);
if (queue_first(&task->thread_list) == 0)
return(THREAD_NULL);
queue_iterate(&task->thread_list, thread, thread_t, thread_list) {
if (thread_id-- <= 0)
return(thread);
}
return(THREAD_NULL);
}
boolean_t
db_get_next_thread(
thread_t *threadp,
int position)
{
db_expr_t value;
thread_t thread;
*threadp = THREAD_NULL;
if (db_expression(&value)) {
thread = (thread_t) value;
if (!db_check_thread_address_valid(thread)) {
db_flush_lex();
return(FALSE);
}
} else if (position <= 0) {
thread = db_default_thread;
} else
return(FALSE);
*threadp = thread;
return(TRUE);
}
void
db_init_default_thread(void)
{
if (db_lookup_thread(db_default_thread) < 0) {
db_default_thread = THREAD_NULL;
db_default_task = TASK_NULL;
} else
db_default_task = db_default_thread->task;
}
void
db_set_default_thread(
struct db_variable *vp,
db_expr_t *valuep,
int flag,
db_var_aux_param_t ap)
{
thread_t thread;
if (flag != DB_VAR_SET) {
*valuep = (db_expr_t) db_default_thread;
return;
}
thread = (thread_t) *valuep;
if (thread != THREAD_NULL && !db_check_thread_address_valid(thread))
db_error(0);
db_default_thread = thread;
if (thread)
db_default_task = thread->task;
return;
}
void
db_get_task_thread(
struct db_variable *vp,
db_expr_t *valuep,
int flag,
db_var_aux_param_t ap)
{
task_t task;
thread_t thread;
if (flag != DB_VAR_GET) {
db_error("Cannot set to $task variable\n");
}
if ((task = db_lookup_task_id(ap->suffix[0])) == TASK_NULL) {
db_printf("no such task($task%d)\n", ap->suffix[0]);
db_error(0);
}
if (ap->level <= 1) {
*valuep = (db_expr_t) task;
return;
}
if ((thread = db_lookup_thread_id(task, ap->suffix[1])) == THREAD_NULL){
db_printf("no such thread($task%d.%d)\n",
ap->suffix[0], ap->suffix[1]);
db_error(0);
}
*valuep = (db_expr_t) thread;
return;
}
void
db_get_map(struct db_variable *vp,
db_expr_t *valuep,
int flag,
db_var_aux_param_t ap)
{
task_t task;
if (flag != DB_VAR_GET) {
db_error("Cannot set to $map variable\n");
}
if ((task = db_lookup_task_id(ap->suffix[0])) == TASK_NULL) {
db_printf("no such map($map%d)\n", ap->suffix[0]);
db_error(0);
}
*valuep = (db_expr_t) task->map;
}
#endif