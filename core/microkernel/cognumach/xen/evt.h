#ifndef XEN_EVT_H
#define XEN_EVT_H
#include <machine/spl.h>
void hyp_intrinit(void);
void form_int_mask(void);
void hyp_evt_handler(evtchn_port_t port, interrupt_handler_fn handler, int unit, spl_t spl);
void hyp_c_callback(void *ret_addr, void *regs);
#endif