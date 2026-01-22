#if MACH_KDB
#include <mach/boolean.h>
#include <machine/db_machdep.h>
#include <machine/db_interface.h>
#include <machine/setjmp.h>
#include <kern/task.h>
#include <ddb/db_access.h>
int db_access_level = DB_ACCESS_LEVEL;
static int db_extend[sizeof(int)+1] = {
0,
0xFFFFFF80,
0xFFFF8000,
0xFF800000,
0x80000000
};
db_expr_t
db_get_task_value(
db_addr_t	addr,
int		size,
boolean_t	is_signed,
task_t		task)
{
char		data[sizeof(db_expr_t)];
db_expr_t 	value;
int		i;
if (!db_read_bytes(addr, size, data, task))
return 0;
value = 0;
#if	BYTE_MSF
for (i = 0; i < size; i++)
#else
for (i = size - 1; i >= 0; i--)
#endif
{
value = (value << 8) + (data[i] & 0xFF);
}
if (size <= sizeof(int)) {
if (is_signed && (value & db_extend[size]) != 0)
value |= db_extend[size];
}
return (value);
}
void
db_put_task_value(
db_addr_t	addr,
int		size,
db_expr_t 	value,
task_t		task)
{
char		data[sizeof(db_expr_t)];
int		i;
#if	BYTE_MSF
for (i = size - 1; i >= 0; i--)
#else
for (i = 0; i < size; i++)
#endif
{
data[i] = value & 0xFF;
value >>= 8;
}
db_write_bytes(addr, size, data, task);
}
db_expr_t
db_get_value(
db_addr_t	addr,
int		size,
boolean_t	is_signed)
{
return(db_get_task_value(addr, size, is_signed, TASK_NULL));
}
void
db_put_value(
db_addr_t	addr,
int		size,
db_expr_t	value)
{
db_put_task_value(addr, size, value, TASK_NULL);
}
#endif