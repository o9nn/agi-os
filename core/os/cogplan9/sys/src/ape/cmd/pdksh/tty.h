#ifdef EXTERN
# define I__(i) = i
#else
# define I__(i)
# define EXTERN extern
# define EXTERN_DEFINED
#endif
#ifdef SYS_IOCTL_WITH_TERMIOS
# define SYS_IOCTL_WITH_TERMIOS
#endif
#ifdef SYS_IOCTL_WITH_TERMIO
# define SYS_IOCTL_WITH_TERMIO
#endif
#ifdef	HAVE_TERMIOS_H
# include <termios.h>
# ifdef SYS_IOCTL_WITH_TERMIOS
#  if !(defined(sun) && !defined(__svr4__))
#   include <sys/ioctl.h>
#  endif
# endif
typedef struct termios TTY_state;
#else
# ifdef HAVE_TERMIO_H
#  include <termio.h>
#  ifdef SYS_IOCTL_WITH_TERMIO
#   include <sys/ioctl.h>
#  endif
#  if _BSD_SYSV
#   ifndef NTTYDISC
#    define	TIOCGETD	_IOR( 't', 0, int )
#    define	TIOCSETD	_IOW( 't', 1, int )
#    define	NTTYDISC	2
#   endif
#   ifndef TIOCSTI
#    define	TIOCSTI		_IOW( 't', 114, char )
#   endif
#   ifndef TIOCSPGRP
#    define	TIOCSPGRP	_IOW( 't', 118, int )
#   endif
#  endif
typedef struct termio TTY_state;
# else
#  ifdef _MINIX
#   include <sgtty.h>
#   define TIOCSETN	TIOCSETP
#  else
#   include <sys/ioctl.h>
#  endif
typedef struct {
struct sgttyb	sgttyb;
#  ifdef TIOCGATC
struct lchars	lchars;
#  else
struct tchars	tchars;
#   ifdef TIOCGLTC
struct ltchars	ltchars;
#   endif
#  endif
} TTY_state;
# endif
#endif
#define TF_NONE		0x00
#define TF_WAIT		0x01
#define TF_MIPSKLUDGE	0x02
EXTERN int		tty_fd I__(-1);
EXTERN int		tty_devtty;
EXTERN TTY_state	tty_state;
extern int	get_tty ARGS((int fd, TTY_state *ts));
extern int	set_tty ARGS((int fd, TTY_state *ts, int flags));
extern void	tty_init ARGS((int init_ttystate));
extern void	tty_close ARGS((void));
#ifdef EXTERN_DEFINED
# undef EXTERN_DEFINED
# undef EXTERN
#endif
#undef I__