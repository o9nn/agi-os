#define TM_UNDEFINED (-1)
#define TM_DEFINED(x) (0 <= (x))
#define TM_LOCAL_ZONE LONG_MIN
#define TM_UNDEFINED_ZONE (LONG_MIN + 1)
struct partime
{
struct tm tm;
int ymodulus;
int yweek;
long zone;
};
#if defined __STDC__ || has_prototypes
# define __PARTIME_P(x) x
#else
# define __PARTIME_P(x) ()
#endif
char *partime __PARTIME_P ((char const *, struct partime *));
char *parzone __PARTIME_P ((char const *, long *));