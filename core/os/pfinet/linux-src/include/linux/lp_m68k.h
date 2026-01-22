#ifndef _LINUX_LP_H
#define _LINUX_LP_H
#define MAX_LP 5
#define LP_EXIST 0x0001
#define LP_BUSY	 0x0004
#define LP_ABORT 0x0040
#define LP_CAREFUL 0x0080
#define LP_ABORTOPEN 0x0100
#define LP_INIT_CHAR 1000
#define LP_INIT_WAIT 0
#define LP_INIT_TIME 40
#define LPCHAR   0x0601
#define LPTIME   0x0602
#define LPABORT  0x0604
#define LPSETIRQ 0x0605
#define LPGETIRQ 0x0606
#define LPWAIT   0x0608
#define LPCAREFUL   0x0609
#define LPABORTOPEN 0x060a
#define LPGETSTATUS 0x060b
#define LPRESET     0x060c
#define LP_TIMEOUT_INTERRUPT	(60 * HZ)
#define LP_TIMEOUT_POLLED	(10 * HZ)
#define LP_BUFFER_SIZE 1024
enum lp_type  {
LP_UNKNOWN = 0,
LP_AMIGA = 1,
LP_ATARI = 2,
LP_MFC = 3,
LP_IOEXT = 4,
LP_MVME167 = 5,
LP_BVME6000 = 6
};
struct lp_struct {
char *name;
unsigned int irq;
void (*lp_out)(int,int);
int (*lp_is_busy)(int);
int (*lp_has_pout)(int);
int (*lp_is_online)(int);
int (*lp_dummy)(int);
int (*lp_ioctl)(int, unsigned int, unsigned long);
int (*lp_open)(int);
void (*lp_release)(int);
int flags;
unsigned int chars;
unsigned int time;
unsigned int wait;
struct wait_queue *lp_wait_q;
void *base;
enum lp_type type;
char lp_buffer[LP_BUFFER_SIZE];
int do_print;
unsigned long copy_size,bytes_written;
};
extern struct lp_struct *lp_table[MAX_LP];
extern unsigned int lp_irq;
void lp_interrupt(int dev);
int lp_m68k_init(void);
int register_parallel(struct lp_struct *, int);
void unregister_parallel(int);
#endif