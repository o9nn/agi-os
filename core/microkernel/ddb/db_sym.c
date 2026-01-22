#if MACH_KDB
#include <string.h>
#include <mach/std_types.h>
#include <machine/db_machdep.h>
#include <ddb/db_command.h>
#include <ddb/db_output.h>
#include <ddb/db_sym.h>
#include <ddb/db_task_thread.h>
#include <ddb/db_elf.h>
#include <vm/vm_map.h>
#define	MAXNOSYMTABS	5
db_symtab_t	db_symtabs[MAXNOSYMTABS] = {{0,},};
int db_nsymtab = 0;
db_symtab_t	*db_last_symtab;
boolean_t
db_add_symbol_table(
int  type,
char *start,
char *end,
const char *name,
char *ref,
char *map_pointer)
{
db_symtab_t *st;
extern vm_map_t kernel_map;
if (db_nsymtab >= MAXNOSYMTABS)
return (FALSE);
st = &db_symtabs[db_nsymtab];
st->type = type;
st->start = start;
st->end = end;
st->private = ref;
st->map_pointer = (map_pointer == (char *)kernel_map)? 0: map_pointer;
strncpy(st->name, name, sizeof st->name - 1);
st->name[sizeof st->name - 1] = '\0';
db_nsymtab++;
return (TRUE);
}
static char * __attribute__ ((pure))
db_qualify(const char *symname, const char *symtabname)
{
static char     tmp[256];
char		*s;
s = tmp;
while ((*s++ = *symtabname++)) {
}
s[-1] = ':';
*s++ = ':';
while ((*s++ = *symname++)) {
}
return tmp;
}
boolean_t
db_eqname( const char* src, const char* dst, char c )
{
if (!strcmp(src, dst))
return (TRUE);
if (src[0] == c)
return (!strcmp(src+1,dst));
return (FALSE);
}
boolean_t
db_value_of_name(
char		*name,
db_expr_t	*valuep)
{
db_sym_t	sym;
sym = db_lookup(name);
if (sym == DB_SYM_NULL)
return (FALSE);
db_symbol_values(0, sym, &name, valuep);
db_free_symbol(sym);
return (TRUE);
}
db_sym_t
db_lookup(char *symstr)
{
db_sym_t sp;
int i;
int symtab_start = 0;
int symtab_end = db_nsymtab;
char *cp;
for (cp = symstr; *cp; cp++) {
if (*cp == ':' && cp[1] == ':') {
*cp = '\0';
for (i = 0; i < db_nsymtab; i++) {
if (! strcmp(symstr, db_symtabs[i].name)) {
symtab_start = i;
symtab_end = i + 1;
break;
}
}
*cp = ':';
if (i == db_nsymtab)
db_error("Invalid symbol table name\n");
symstr = cp+2;
}
}
for (i = symtab_start; i < symtab_end; i++) {
if ((sp = X_db_lookup(&db_symtabs[i], symstr))) {
db_last_symtab = &db_symtabs[i];
return sp;
}
db_free_symbol(sp);
}
return 0;
}
db_sym_t
db_sym_parse_and_lookup(
db_sym_t	(*func) (db_symtab_t *, const char*, const char*, int),
db_symtab_t	*symtab,
char		*symstr)
{
char 		*p;
int 		n;
int	 	n_name;
int	 	line_number;
char	 	*file_name = 0;
char	 	*sym_name = 0;
char		*component[3];
db_sym_t 	found = DB_SYM_NULL;
component[0] = symstr;
component[1] = component[2] = 0;
for (p = symstr, n = 1; *p; p++) {
if (*p == ':') {
if (n >= 3)
break;
*p = 0;
component[n++] = p+1;
}
}
if (*p != 0)
goto out;
line_number = 0;
n_name = n;
p = component[n-1];
if (*p >= '0' && *p <= '9') {
if (n == 1)
goto out;
for (line_number = 0; *p; p++) {
if (*p < '0' || *p > '9')
goto out;
line_number = line_number*10 + *p - '0';
}
n_name--;
} else if (n >= 3)
goto out;
if (n_name == 1) {
for (p = component[0]; *p && *p != '.'; p++);
if (*p == '.') {
file_name = component[0];
sym_name = 0;
} else {
file_name = 0;
sym_name = component[0];
}
} else {
file_name = component[0];
sym_name = component[1];
}
found = func(symtab, file_name, sym_name, line_number);
out:
while (--n >= 1)
component[n][-1] = ':';
return(found);
}
boolean_t db_qualify_ambiguous_names = FALSE;
static boolean_t
db_name_is_ambiguous(char *sym_name)
{
int		i;
boolean_t	found_once = FALSE;
if (!db_qualify_ambiguous_names)
return FALSE;
for (i = 0; i < db_nsymtab; i++) {
db_sym_t sp = X_db_lookup(&db_symtabs[i], sym_name);
if (sp) {
if (found_once)
{
db_free_symbol(sp);
return TRUE;
}
found_once = TRUE;
}
db_free_symbol(sp);
}
return FALSE;
}
db_sym_t
db_search_task_symbol(
db_addr_t		val,
db_strategy_t		strategy,
db_addr_t		*offp,
task_t			task)
{
db_sym_t ret;
if (task != TASK_NULL)
ret = db_search_in_task_symbol(val, strategy, offp, task);
else
{
ret = db_search_in_task_symbol(val, strategy, offp, task);
if (ret == DB_SYM_NULL || (*offp) > 0x1000000)
{
db_free_symbol(ret);
task = db_current_task();
ret = db_search_in_task_symbol(val, strategy, offp, task);
}
}
return ret;
}
db_sym_t
db_search_in_task_symbol(
db_addr_t		val,
db_strategy_t		strategy,
db_addr_t		*offp,
task_t			task)
{
vm_size_t 	diff;
vm_size_t	newdiff;
int		i;
db_symtab_t	*sp;
db_sym_t	ret = DB_SYM_NULL, sym;
vm_map_t	map_for_val;
map_for_val = (task == TASK_NULL)? VM_MAP_NULL: task->map;
newdiff = diff = ~0;
db_last_symtab = (db_symtab_t *) 0;
for (sp = &db_symtabs[0], i = 0; i < db_nsymtab;  sp++, i++)
{
newdiff = ~0;
if ((vm_map_t)sp->map_pointer == VM_MAP_NULL ||
(vm_map_t)sp->map_pointer == map_for_val)
{
sym = X_db_search_symbol(sp, val, strategy, (db_expr_t*)&newdiff);
if (sym == DB_SYM_NULL)
continue;
if (db_last_symtab == (db_symtab_t *) 0)
{
db_last_symtab = sp;
diff = newdiff;
db_free_symbol(ret);
ret = sym;
continue;
}
if ((vm_map_t) sp->map_pointer == VM_MAP_NULL &&
(vm_map_t) db_last_symtab->map_pointer == VM_MAP_NULL &&
newdiff < diff )
{
db_last_symtab = sp;
diff = newdiff;
db_free_symbol(ret);
ret = sym;
continue;
}
if ((vm_map_t) sp->map_pointer != VM_MAP_NULL &&
(newdiff < 0x100000) &&
((vm_map_t) db_last_symtab->map_pointer == VM_MAP_NULL ||
newdiff < diff ))
{
db_last_symtab = sp;
diff = newdiff;
db_free_symbol(ret);
ret = sym;
continue;
}
}
}
*offp = diff;
return ret;
}
void
db_symbol_values(
db_symtab_t	*stab,
db_sym_t	sym,
char		**namep,
db_expr_t	*valuep)
{
db_expr_t	value;
char		*name;
if (sym == DB_SYM_NULL) {
*namep = 0;
return;
}
if (stab == 0)
stab = db_last_symtab;
X_db_symbol_values(stab, sym, &name, &value);
if (db_name_is_ambiguous(name))
*namep = db_qualify(name, db_last_symtab->name);
else
*namep = name;
if (valuep)
*valuep = value;
}
unsigned long	db_maxoff = 0x4000;
void
db_task_printsym(
db_addr_t	off,
db_strategy_t	strategy,
task_t		task)
{
db_addr_t	d;
char 		*filename;
char		*name;
db_expr_t	value;
int 		linenum;
db_sym_t	cursym;
cursym = db_search_task_symbol(off, strategy, &d, task);
db_symbol_values(0, cursym, &name, &value);
if (name == 0 || d >= db_maxoff || value == 0 || *name == 0) {
db_printf("%#n", off);
db_free_symbol(cursym);
return;
}
db_printf("%s", name);
if (d)
db_printf("+0x%x", d);
if (strategy == DB_STGY_PROC) {
if (db_line_at_pc(cursym, &filename, &linenum, off)) {
db_printf(" [%s", filename);
if (linenum > 0)
db_printf(":%d", linenum);
db_printf("]");
}
}
db_free_symbol(cursym);
}
void
db_printsym(
db_expr_t	off,
db_strategy_t	strategy)
{
db_task_printsym(off, strategy, TASK_NULL);
}
boolean_t
db_line_at_pc(
db_sym_t	sym,
char		**filename,
int		*linenum,
db_addr_t	pc)
{
return (db_last_symtab) ?
X_db_line_at_pc( db_last_symtab, sym, filename, linenum, pc) :
FALSE;
}
void db_free_symbol(db_sym_t s)
{
return (db_last_symtab) ?
X_db_free_symbol( db_last_symtab, s) :
FALSE;
}
static void dummy_db_free_symbol(db_sym_t symbol) { }
static boolean_t dummy_db_sym_init(char *a, char *b, const char *c, char *d) {
return FALSE;
}
struct db_sym_switch x_db[] = {
{ 0,},
{ 0,},
{ 0,},
#ifdef	DB_NO_ELF
{ 0,},
#else
{ dummy_db_sym_init, elf_db_lookup, elf_db_search_symbol,
elf_db_line_at_pc, elf_db_symbol_values, dummy_db_free_symbol },
#endif
};
#endif