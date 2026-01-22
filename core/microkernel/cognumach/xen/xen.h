#ifndef XEN_XEN_H
#define XEN_XEN_H
void hyp_init(void);
void hyp_dev_init(void);
void hyp_idle(void);
void hyp_p2m_init(void);
struct i386_interrupt_state;
void hypclock_machine_intr(int old_ipl, void *ret_addr, struct i386_interrupt_state *regs, uint64_t delta);
struct failsafe_callback_regs {
unsigned int ds;
unsigned int es;
unsigned int fs;
unsigned int gs;
unsigned int ip;
unsigned int cs_and_mask;
unsigned int flags;
};
void hyp_failsafe_c_callback(struct failsafe_callback_regs *regs);
#endif