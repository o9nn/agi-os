#if defined __STDC__ || has_prototypes
# define __MAKETIME_P(x) x
#else
# define __MAKETIME_P(x) ()
#endif
struct tm *time2tm __MAKETIME_P ((time_t, int));
time_t difftm __MAKETIME_P ((struct tm const *, struct tm const *));
time_t str2time __MAKETIME_P ((char const *, time_t, long));
time_t tm2time __MAKETIME_P ((struct tm *, int));
void adjzone __MAKETIME_P ((struct tm *, long));