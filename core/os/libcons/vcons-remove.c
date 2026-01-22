#include <assert-backtrace.h>
#include "cons.h"
void __attribute__ ((weak))
cons_vcons_remove (cons_t cons, vcons_list_t vcons_entry)
{
assert_backtrace (!vcons_entry->vcons);
}