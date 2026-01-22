#ifndef XEN_GRANT_H
#define XEN_GRANT_H
#include <sys/types.h>
#include <machine/xen.h>
#include <xen/public/xen.h>
#include <xen/public/grant_table.h>
void hyp_grant_init(void);
grant_ref_t hyp_grant_give(domid_t domid, unsigned long frame_nr, int readonly);
void hyp_grant_takeback(grant_ref_t grant);
grant_ref_t hyp_grant_accept_transfer(domid_t domid, unsigned long frame_nr);
unsigned long hyp_grant_finish_transfer(grant_ref_t grant);
void *hyp_grant_address(grant_ref_t grant);
#endif