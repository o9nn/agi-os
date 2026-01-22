#if MACH_KDB
#include <string.h>
#include <mach/std_types.h>
#include <mach/exec/elf.h>
#include <machine/db_machdep.h>
#include <machine/vm_param.h>
#include <ddb/db_output.h>
#include <ddb/db_sym.h>
#include <ddb/db_elf.h>
#ifndef DB_NO_ELF
struct db_symtab_elf {
int type;
Elf_Sym *start;
Elf_Sym *end;
char *strings;
char *map_pointer;
char name[SYMTAB_NAME_LEN];
};
boolean_t
elf_db_sym_init (unsigned shdr_num,
vm_size_t shdr_size,
vm_offset_t shdr_addr,
unsigned shdr_shndx,
char *name,
char *task_addr)
{
Elf_Shdr *shdr, *symtab, *strtab;
const char *shstrtab;
unsigned i;
if (shdr_num == 0)
return FALSE;
if (shdr_size != sizeof *shdr)
return FALSE;
shdr = (Elf_Shdr *) shdr_addr;
if (shdr[shdr_shndx].sh_type != SHT_STRTAB)
return FALSE;
shstrtab = (const char *) phystokv (shdr[shdr_shndx].sh_addr);
symtab = strtab = NULL;
for (i = 0; i < shdr_num; i++)
switch (shdr[i].sh_type) {
case SHT_SYMTAB:
if (symtab)
db_printf ("Ignoring additional ELF symbol table at %d\n", i);
else
symtab = &shdr[i];
break;
case SHT_STRTAB:
if (strcmp (&shstrtab[shdr[i].sh_name], ".strtab") == 0) {
if (strtab)
db_printf ("Ignoring additional ELF string table at %d\n", i);
else
strtab = &shdr[i];
}
break;
}
if (symtab == NULL || strtab == NULL)
return FALSE;
if (db_add_symbol_table (SYMTAB_ELF,
(char *) phystokv (symtab->sh_addr),
(char *) phystokv (symtab->sh_addr)+symtab->sh_size,
name,
(char *) phystokv (strtab->sh_addr),
task_addr)) {
db_printf ("Loaded ELF symbol table for %s (%d symbols)\n",
name, symtab->sh_size / sizeof (Elf_Sym));
return TRUE;
}
return FALSE;
}
db_sym_t
elf_db_lookup (db_symtab_t *stab,
char *symstr)
{
struct db_symtab_elf *self = (struct db_symtab_elf *) stab;
Elf_Sym *s;
for (s = self->start; s < self->end; s++)
if (strcmp (symstr, &self->strings[s->st_name]) == 0)
return (db_sym_t) s;
return NULL;
}
db_sym_t
elf_db_search_symbol (db_symtab_t *stab,
db_addr_t off,
db_strategy_t strategy,
db_expr_t *diffp)
{
struct db_symtab_elf *self = (struct db_symtab_elf *) stab;
unsigned long diff = *diffp;
Elf_Sym *s, *symp = NULL;
for (s = self->start; s < self->end; s++) {
if (s->st_name == 0)
continue;
if (strategy == DB_STGY_XTRN && (ELF_ST_BIND(s->st_info) != STB_GLOBAL))
continue;
if (off >= s->st_value) {
if (ELF_ST_TYPE(s->st_info) != STT_FUNC)
continue;
if (off - s->st_value < diff) {
diff = off - s->st_value;
symp = s;
if (diff == 0 && (ELF_ST_BIND(s->st_info) == STB_GLOBAL))
break;
} else if (off - s->st_value == diff) {
if (symp == NULL)
symp = s;
else if ((ELF_ST_BIND(symp->st_info) != STB_GLOBAL)
&& (ELF_ST_BIND(s->st_info) == STB_GLOBAL))
symp = s;
}
}
}
if (symp == NULL)
*diffp = off;
else
*diffp = diff;
return (db_sym_t) symp;
}
void
elf_db_symbol_values (db_symtab_t *stab,
db_sym_t sym,
char **namep,
db_expr_t *valuep)
{
struct db_symtab_elf *self = (struct db_symtab_elf *) stab;
Elf_Sym *s = (Elf_Sym *) sym;
if (namep)
*namep = &self->strings[s->st_name];
if (valuep)
*valuep = s->st_value;
}
boolean_t
elf_db_line_at_pc (db_symtab_t *stab,
db_sym_t sym,
char **file,
int *line,
db_addr_t pc)
{
return FALSE;
}
#endif
#endif