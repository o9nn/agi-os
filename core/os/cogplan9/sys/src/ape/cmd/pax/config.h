#ifndef _PAX_CONFIG_H
#define _PAX_CONFIG_H
#define USG
#define DEF_AR_FILE	"-"
#define	TTY	"/dev/cons"
#define DIRENT
#define OFFSET	off_t
#ifndef __STDC__
#endif
#define SIG_T	void
#ifdef BSD
#ifdef USG
#include "You must first edit config.h and Makefile to configure pax."
#endif
#endif
#ifdef PAXDIR
#  ifndef DIRENT
#    define DIRENT
#  endif
#endif
#ifdef XENIX_286
#  define USG
#endif
#endif
#ifndef __STDC__
#define __STDC__
#endif