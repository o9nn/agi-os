#ifndef igcstr_INCLUDED
# define igcstr_INCLUDED
chunk_t *gc_locate(const void *, gc_state_t *);
void gc_strings_set_marks(chunk_t *, bool);
bool gc_string_mark(const byte *, uint, bool, gc_state_t *);
void gc_strings_clear_reloc(chunk_t *);
void gc_strings_set_reloc(chunk_t *);
void gc_strings_compact(chunk_t *);
string_proc_reloc(igc_reloc_string);
const_string_proc_reloc(igc_reloc_const_string);
param_string_proc_reloc(igc_reloc_param_string);
#endif