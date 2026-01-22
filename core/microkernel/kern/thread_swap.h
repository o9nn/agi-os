#ifndef	_KERN_THREAD_SWAP_H_
#define _KERN_THREAD_SWAP_H_
extern void	swapper_init(void);
extern void	thread_swapin(thread_t thread);
extern kern_return_t	thread_doswapin(thread_t thread);
extern void	swapin_thread(void) __attribute__((noreturn));
#endif