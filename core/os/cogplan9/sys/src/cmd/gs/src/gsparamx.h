#ifndef gsparamx_INCLUDED
#  define gsparamx_INCLUDED
bool gs_param_string_eq(const gs_param_string *pcs, const char *str);
int param_put_enum(gs_param_list * plist, gs_param_name param_name,
int *pvalue, const char *const pnames[], int ecode);
int param_put_bool(gs_param_list * plist, gs_param_name param_name,
bool * pval, int ecode);
int param_put_int(gs_param_list * plist, gs_param_name param_name,
int * pval, int ecode);
int param_put_long(gs_param_list * plist, gs_param_name param_name,
long * pval, int ecode);
int param_list_copy(gs_param_list *plto, gs_param_list *plfrom);
#endif