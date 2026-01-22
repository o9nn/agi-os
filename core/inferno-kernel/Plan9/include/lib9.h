#include <u.h>
typedef usize size_t;
#define	Rendez	xRendez
#include <libc.h>
#undef Rendez
#define	setbinmode()
#define	USE_FPdbleword