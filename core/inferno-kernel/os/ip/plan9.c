#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include	"ip.h"
char*
commonuser(void)
{
return up->env->user;
}
Chan*
commonfdtochan(int fd, int mode, int a, int b)
{
return fdtochan(up->env->fgrp, fd, mode, a, b);
}
char*
commonerror(void)
{
return up->env->errstr;
}
int
postnote(Proc *p, int, char *, int)
{
swiproc(p, 0);
return 0;
}