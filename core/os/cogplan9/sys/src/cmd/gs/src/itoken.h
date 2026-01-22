#ifndef itoken_INCLUDED
# define itoken_INCLUDED
int ztokenexec_continue(i_ctx_t *i_ctx_p);
#ifndef scanner_state_DEFINED
# define scanner_state_DEFINED
typedef struct scanner_state_s scanner_state;
#endif
int ztoken_handle_comment(i_ctx_t *i_ctx_p, const ref *fop,
scanner_state *sstate, const ref *ptoken,
int scan_code, bool save, bool push_file,
op_proc_t cont);
int ztoken_scanner_options(const ref *upref, int old_options);
#endif