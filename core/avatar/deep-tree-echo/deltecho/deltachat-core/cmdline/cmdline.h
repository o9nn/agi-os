#ifndef __DC_CMDLINE_H__
#define __DC_CMDLINE_H__
#ifdef __cplusplus
extern "C" {
#endif
char*           dc_cmdline           (dc_context_t*, const char* cmd);
void            dc_cmdline_skip_auth ();
#ifdef __cplusplus
}
#endif
#endif