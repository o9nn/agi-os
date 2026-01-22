#if HAVE_EDITLINE
char *lg_readline(const char *mb_prompt);
#endif
#if HAVE_WIDECHAR_EDITLINE
void find_history_filepath(const char *, const char *, const char *);
#else
#define find_history_filepath(a, b, c)
#endif