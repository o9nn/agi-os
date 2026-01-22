#ifndef	_FNMATCH_H
#define	_FNMATCH_H	1
#undef FNM_PATHNAME
#define	FNM_PATHNAME	(1 << 0)
#undef FNM_NOESCAPE
#define	FNM_NOESCAPE	(1 << 1)
#undef FNM_PERIOD
#define	FNM_PERIOD	(1 << 2)
#undef __FNM_FLAGS
#define	__FNM_FLAGS	(FNM_PATHNAME|FNM_NOESCAPE|FNM_PERIOD)
#undef FNM_NOMATCH
#define	FNM_NOMATCH	1
#if __STDC__
extern int fnmatch (const char *pattern, const char *string, int flags);
#else
extern int fnmatch ();
#endif
#endif