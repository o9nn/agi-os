#ifndef dwreg_INCLUDED
# define dwreg_INCLUDED
int win_get_reg_value(const char *name, char *ptr, int *plen);
int win_set_reg_value(const char *name, const char *value);
#endif