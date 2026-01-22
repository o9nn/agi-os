#ifndef gserror_INCLUDED
# define gserror_INCLUDED
int gs_log_error(int, const char *, int);
#ifndef DEBUG
# define gs_log_error(err, file, line) (err)
#endif
#define gs_note_error(err) gs_log_error(err, __FILE__, __LINE__)
#define return_error(err) return gs_note_error(err)
#endif