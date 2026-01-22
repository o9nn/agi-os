#ifdef SYSV
#include <termio.h>
#ifdef DKSTREAMS
#include <sys/stream.h>
#include <sys/stropts.h>
#endif
#endif
#ifdef V9
#include <sys/filio.h>
#include <sys/ttyio.h>
extern int	tty_ld;
#endif
#ifdef BSD4_2
#include <sgtty.h>
#include <sys/time.h>
#include <errno.h>
#define FD_ZERO(s) (s) = 0
#define FD_SET(n,s) (s) |= 1 << (n)
extern int	errno;
#endif
#ifdef DKHOST
#include <dk.h>
#include <sysexits.h>
extern char	*dtnamer();
extern int	dkminor();
#endif
extern char	*line;
extern int	ttyi;
extern int	ttyo;
extern FILE	*fp_log;
extern char	mesg[];
extern char	*endmesg;
extern int	next;
extern short	baudrate;
extern int	stopbits;
extern int	interactive;
extern int	whatami;
extern int	canread;
extern int	canwrite;