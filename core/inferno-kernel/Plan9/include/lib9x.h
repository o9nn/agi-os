#include <u.h>
typedef usize size_t;
#define	Runeerror xRuneerror
#define	Rendez	xRendez
#include <libc.h>
#undef Runeerror
#undef Rendez
enum
{
Runeerror	= 0x80,
};
#define	setbinmode()
#define	USE_FPdbleword