#ifndef _DDB_DB_ELF_H_
#define _DDB_DB_ELF_H_
#include <ddb/db_sym.h>
#include <machine/db_machdep.h>
extern boolean_t
elf_db_line_at_pc(
db_symtab_t *stab,
db_sym_t sym,
char **file,
int *line,
db_addr_t pc);
extern db_sym_t
elf_db_lookup(
db_symtab_t *stab,
char * symstr);
extern db_sym_t
elf_db_search_symbol(
db_symtab_t * symtab,
db_addr_t off,
db_strategy_t strategy,
db_expr_t *diffp);
extern void
elf_db_symbol_values(
db_symtab_t *stab,
db_sym_t sym,
char **namep,
db_expr_t *valuep);
#endif