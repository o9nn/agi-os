#ifndef gpmisc_INCLUDED
# define gpmisc_INCLUDED
int gp_gettmpdir(char *ptr, int *plen);
FILE *gp_fopentemp(const char *fname, const char *mode);
gp_file_name_combine_result gp_file_name_combine_generic(const char *prefix, uint plen,
const char *fname, uint flen, bool no_sibling, char *buffer, uint *blen);
gp_file_name_combine_result gp_file_name_reduce(const char *fname, uint flen,
char *buffer, uint *blen);
bool gp_file_name_is_absolute(const char *fname, uint flen);
uint gp_file_name_parents(const char *fname, uint flen);
uint gp_file_name_cwds(const char *fname, uint flen);
#endif