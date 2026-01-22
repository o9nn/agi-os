#ifndef _DDB_DB_SYM_H_
#define _DDB_DB_SYM_H_
#include <mach/boolean.h>
#include <mach/machine/vm_types.h>
#include <machine/db_machdep.h>
#define SYMTAB_NAME_LEN 32
typedef struct {
int type;
#define SYMTAB_AOUT 0
#define SYMTAB_COFF 1
#define SYMTAB_MACHDEP 2
#define SYMTAB_ELF 3
char *start;
char *end;
char *private;
char *map_pointer;
char name[SYMTAB_NAME_LEN];
} db_symtab_t;
extern db_symtab_t *db_last_symtab;
typedef char * db_sym_t;
#define DB_SYM_NULL ((db_sym_t)0)
typedef int db_strategy_t;
#define DB_STGY_ANY 0
#define DB_STGY_XTRN 1
#define DB_STGY_PROC 2
extern boolean_t db_qualify_ambiguous_names;
extern boolean_t db_add_symbol_table( int type,
char * start,
char * end,
const char *name,
char *ref,
char *map_pointer );
extern int db_value_of_name( char* name, db_expr_t* valuep);
extern db_sym_t db_search_task_symbol( db_addr_t val,
db_strategy_t strategy,
db_addr_t *offp,
task_t task );
extern void db_symbol_values( db_symtab_t *stab,
db_sym_t sym,
char** namep,
db_expr_t* valuep);
#define db_search_symbol(val,strgy,offp) \
db_search_task_symbol(val,strgy,offp,0)
#define db_find_sym_and_offset(val,namep,offp) \
do { \
db_sym_t s; \
db_symbol_values(0, s = db_search_symbol(val,DB_STGY_ANY,offp) \
,namep,0); \
db_free_symbol(s); \
} while(0);
#define db_find_xtrn_sym_and_offset(val,namep,offp) \
do { \
db_sym_t s; \
db_symbol_values(0, s = db_search_symbol(val,DB_STGY_XTRN,offp) \
,namep,0); \
db_free_symbol(s); \
} while(0);
#define db_find_task_sym_and_offset(val,namep,offp,task) \
do { \
db_sym_t s; \
db_symbol_values(0, s = db_search_task_symbol(val,DB_STGY_ANY \
,offp,task), \
namep, 0); \
db_free_symbol(s); \
} while(0);
#define db_find_xtrn_task_sym_and_offset(val,namep,offp,task) \
do { \
db_sym_t s; \
db_symbol_values(0, s = db_search_task_symbol(val,DB_STGY_XTRN \
,offp,task), \
namep,0); \
db_free_symbol(s); \
} while(0);
extern boolean_t db_eqname( const char* src, const char* dst, char c );
extern void db_task_printsym( db_addr_t off,
db_strategy_t strategy,
task_t task);
extern void db_printsym( db_expr_t off, db_strategy_t strategy);
extern void db_free_symbol(db_sym_t s);
extern struct db_sym_switch {
boolean_t (*init)(
char *start,
char *end,
const char *name,
char *task_addr
);
db_sym_t (*lookup)(
db_symtab_t *stab,
char *symstr
);
db_sym_t (*search_symbol)(
db_symtab_t *stab,
db_addr_t off,
db_strategy_t strategy,
db_expr_t *diffp
);
boolean_t (*line_at_pc)(
db_symtab_t *stab,
db_sym_t sym,
char **file,
int *line,
db_addr_t pc
);
void (*symbol_values)(
db_symtab_t *stab,
db_sym_t sym,
char **namep,
db_expr_t *valuep
);
void (*free_symbol)(
db_sym_t sym
);
} x_db[];
#ifndef symtab_type
#define symtab_type(s) SYMTAB_ELF
#endif
#define X_db_sym_init(s,e,n,t) x_db[symtab_type(s)].init(s,e,n,t)
#define X_db_lookup(s,n) x_db[(s)->type].lookup(s,n)
#define X_db_search_symbol(s,o,t,d) x_db[(s)->type].search_symbol(s,o,t,d)
#define X_db_line_at_pc(s,p,f,l,a) x_db[(s)->type].line_at_pc(s,p,f,l,a)
#define X_db_symbol_values(s,p,n,v) x_db[(s)->type].symbol_values(s,p,n,v)
#define X_db_free_symbol(s,m) x_db[(s)->type].free_symbol(m)
extern boolean_t db_line_at_pc(
db_sym_t sym,
char **filename,
int *linenum,
db_addr_t pc);
extern boolean_t elf_db_sym_init (
unsigned shdr_num,
vm_size_t shdr_size,
vm_offset_t shdr_addr,
unsigned shdr_shndx,
char *name,
char *task_addr);
db_sym_t db_lookup(char *);
db_sym_t
db_search_in_task_symbol(
db_addr_t val,
db_strategy_t strategy,
db_addr_t *offp,
task_t task);
extern db_sym_t
db_sym_parse_and_lookup(
db_sym_t (*func) (db_symtab_t *, const char*, const char*, int),
db_symtab_t *symtab,
char *symstr);
#endif