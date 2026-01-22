#ifndef _I386_DB_TRACE_H_
#define _I386_DB_TRACE_H_
struct i386_frame;
void
db_i386_stack_trace(
thread_t		th,
struct i386_frame 	*frame,
db_addr_t		sp,
db_addr_t		callpc,
db_expr_t		count,
int			flags);
#endif