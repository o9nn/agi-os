#ifndef	_MACH_AARCH64_THREAD_STATUS_H_
#define _MACH_AARCH64_THREAD_STATUS_H_
#define AARCH64_THREAD_STATE	1
#define AARCH64_FLOAT_STATE	2
struct aarch64_thread_state {
uint64_t x[31];
uint64_t sp;
uint64_t pc;
uint64_t tpidr_el0;
uint64_t cpsr;
};
#define AARCH64_THREAD_STATE_COUNT	(sizeof(struct aarch64_thread_state) / sizeof(unsigned int))
struct aarch64_float_state {
__int128 v[32];
uint64_t fpsr;
uint64_t fpcr;
uint64_t fpmr;
uint64_t fp_reserved;
};
#define AARCH64_FLOAT_STATE_COUNT	(sizeof(struct aarch64_float_state) / sizeof(unsigned int))
#endif