#ifndef TRIVFS_PRIV_H_INCLUDED
#define TRIVFS_PRIV_H_INCLUDED
#include <mach.h>
#include <hurd.h>
#include <hurd/ports.h>
#include <idvec.h>
#include <unistd.h>
#include "trivfs.h"
static inline int
_is_privileged (struct idvec *uids)
{
return idvec_contains (uids, 0) || idvec_contains (uids, getuid ());
}
#endif