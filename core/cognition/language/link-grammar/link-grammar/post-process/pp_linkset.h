#include <stdbool.h>
#include "pp-structures.h"
pp_linkset *pp_linkset_open(int size);
void pp_linkset_close (pp_linkset *ls);
void pp_linkset_clear (pp_linkset *ls);
bool pp_linkset_add (pp_linkset *ls, const char *str);
bool pp_linkset_match (pp_linkset *ls, const char *str);
bool pp_linkset_match_bw (pp_linkset *ls, const char *str);
size_t pp_linkset_population(pp_linkset *ls);