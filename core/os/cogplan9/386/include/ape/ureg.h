#ifndef __UREG_H
#define __UREG_H
#if !defined(_PLAN9_SOURCE)
This header file is an extension to ANSI/POSIX
#endif
struct Ureg
{
unsigned long	di;
unsigned long	si;
unsigned long	bp;
unsigned long	nsp;
unsigned long	bx;
unsigned long	dx;
unsigned long	cx;
unsigned long	ax;
unsigned long	gs;
unsigned long	fs;
unsigned long	es;
unsigned long	ds;
unsigned long	trap;
unsigned long	ecode;
unsigned long	pc;
unsigned long	cs;
unsigned long	flags;
union {
unsigned long	usp;
unsigned long	sp;
};
unsigned long	ss;
};
#endif