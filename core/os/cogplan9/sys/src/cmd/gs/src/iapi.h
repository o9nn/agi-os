#ifndef iapi_INCLUDED
# define iapi_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
#if defined(_WINDOWS_) || defined(__WINDOWS__)
# ifndef _Windows
# define _Windows
# endif
#endif
#ifdef _Windows
# ifndef GSDLLEXPORT
# define GSDLLEXPORT __declspec(dllexport)
# endif
# ifndef GSDLLAPI
# define GSDLLAPI __stdcall
# endif
# ifndef GSDLLCALL
# define GSDLLCALL __stdcall
# endif
#endif
#if defined(OS2) && defined(__IBMC__)
# ifndef GSDLLAPI
# define GSDLLAPI _System
# endif
# ifndef GSDLLCALL
# define GSDLLCALL _System
# endif
#endif
#ifdef __MACOS__
# pragma export on
#endif
#ifndef GSDLLEXPORT
# define GSDLLEXPORT
#endif
#ifndef GSDLLAPI
# define GSDLLAPI
#endif
#ifndef GSDLLCALL
# define GSDLLCALL
#endif
#if defined(__IBMC__)
# define GSDLLAPIPTR * GSDLLAPI
# define GSDLLCALLPTR * GSDLLCALL
#else
# define GSDLLAPIPTR GSDLLAPI *
# define GSDLLCALLPTR GSDLLCALL *
#endif
#ifndef display_callback_DEFINED
# define display_callback_DEFINED
typedef struct display_callback_s display_callback;
#endif
typedef struct gsapi_revision_s {
const char *product;
const char *copyright;
long revision;
long revisiondate;
} gsapi_revision_t;
GSDLLEXPORT int GSDLLAPI
gsapi_revision(gsapi_revision_t *pr, int len);
GSDLLEXPORT int GSDLLAPI
gsapi_new_instance(void **pinstance, void *caller_handle);
GSDLLEXPORT void GSDLLAPI
gsapi_delete_instance(void *instance);
GSDLLEXPORT int GSDLLAPI
gsapi_set_stdio(void *instance,
int (GSDLLCALLPTR stdin_fn)(void *caller_handle, char *buf, int len),
int (GSDLLCALLPTR stdout_fn)(void *caller_handle, const char *str, int len),
int (GSDLLCALLPTR stderr_fn)(void *caller_handle, const char *str, int len));
GSDLLEXPORT int GSDLLAPI gsapi_set_poll(void *instance,
int (GSDLLCALLPTR poll_fn)(void *caller_handle));
GSDLLEXPORT int GSDLLAPI gsapi_set_display_callback(
void *instance, display_callback *callback);
GSDLLEXPORT int GSDLLAPI gsapi_init_with_args(void *instance,
int argc, char **argv);
GSDLLEXPORT int GSDLLAPI
gsapi_run_string_begin(void *instance,
int user_errors, int *pexit_code);
GSDLLEXPORT int GSDLLAPI
gsapi_run_string_continue(void *instance,
const char *str, unsigned int length, int user_errors, int *pexit_code);
GSDLLEXPORT int GSDLLAPI
gsapi_run_string_end(void *instance,
int user_errors, int *pexit_code);
GSDLLEXPORT int GSDLLAPI
gsapi_run_string_with_length(void *instance,
const char *str, unsigned int length, int user_errors, int *pexit_code);
GSDLLEXPORT int GSDLLAPI
gsapi_run_string(void *instance,
const char *str, int user_errors, int *pexit_code);
GSDLLEXPORT int GSDLLAPI
gsapi_run_file(void *instance,
const char *file_name, int user_errors, int *pexit_code);
GSDLLEXPORT int GSDLLAPI
gsapi_exit(void *instance);
struct vd_trace_interface_s;
GSDLLEXPORT void GSDLLAPI
gsapi_set_visual_tracer(struct vd_trace_interface_s *I);
typedef int (GSDLLAPIPTR PFN_gsapi_revision)(
gsapi_revision_t *pr, int len);
typedef int (GSDLLAPIPTR PFN_gsapi_new_instance)(
void **pinstance, void *caller_handle);
typedef void (GSDLLAPIPTR PFN_gsapi_delete_instance)(
void *instance);
typedef int (GSDLLAPIPTR PFN_gsapi_set_stdio)(void *instance,
int (GSDLLCALLPTR stdin_fn)(void *caller_handle, char *buf, int len),
int (GSDLLCALLPTR stdout_fn)(void *caller_handle, const char *str, int len),
int (GSDLLCALLPTR stderr_fn)(void *caller_handle, const char *str, int len));
typedef int (GSDLLAPIPTR PFN_gsapi_set_poll)(void *instance,
int(GSDLLCALLPTR poll_fn)(void *caller_handle));
typedef int (GSDLLAPIPTR PFN_gsapi_set_display_callback)(
void *instance, display_callback *callback);
typedef int (GSDLLAPIPTR PFN_gsapi_init_with_args)(
void *instance, int argc, char **argv);
typedef int (GSDLLAPIPTR PFN_gsapi_run_string_begin)(
void *instance, int user_errors, int *pexit_code);
typedef int (GSDLLAPIPTR PFN_gsapi_run_string_continue)(
void *instance, const char *str, unsigned int length,
int user_errors, int *pexit_code);
typedef int (GSDLLAPIPTR PFN_gsapi_run_string_end)(
void *instance, int user_errors, int *pexit_code);
typedef int (GSDLLAPIPTR PFN_gsapi_run_string_with_length)(
void *instance, const char *str, unsigned int length,
int user_errors, int *pexit_code);
typedef int (GSDLLAPIPTR PFN_gsapi_run_string)(
void *instance, const char *str,
int user_errors, int *pexit_code);
typedef int (GSDLLAPIPTR PFN_gsapi_run_file)(void *instance,
const char *file_name, int user_errors, int *pexit_code);
typedef int (GSDLLAPIPTR PFN_gsapi_exit)(void *instance);
typedef void (GSDLLAPIPTR PFN_gsapi_set_visual_tracer)
(struct vd_trace_interface_s *I);
#ifdef __MACOS__
#pragma export off
#endif
#ifdef __cplusplus
}
#endif
#endif