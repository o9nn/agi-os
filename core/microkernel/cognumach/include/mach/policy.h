#ifndef	_MACH_POLICY_H_
#define _MACH_POLICY_H_
#define	POLICY_TIMESHARE	1
#define POLICY_FIXEDPRI		2
#define POLICY_LAST		2
#define invalid_policy(policy)	(((policy) <= 0) || ((policy) > POLICY_LAST))
#endif