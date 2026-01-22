#ifndef _LINUX_TIMER_H
#define _LINUX_TIMER_H
#define BLANK_TIMER	0
#define BEEP_TIMER	1
#define RS_TIMER	2
#define SWAP_TIMER	3
#define HD_TIMER	16
#define FLOPPY_TIMER	17
#define SCSI_TIMER 	18
#define NET_TIMER	19
#define SOUND_TIMER	20
#define COPRO_TIMER	21
#define QIC02_TAPE_TIMER	22
#define MCD_TIMER	23
#define HD_TIMER2	24
#define GSCD_TIMER	25
#define DIGI_TIMER	29
struct timer_struct {
unsigned long expires;
void (*fn)(void);
};
extern unsigned long timer_active;
extern struct timer_struct timer_table[32];
struct timer_list {
struct timer_list *next;
struct timer_list *prev;
unsigned long expires;
unsigned long data;
void (*function)(unsigned long);
};
extern void add_timer(struct timer_list * timer);
extern int  del_timer(struct timer_list * timer);
extern void it_real_fn(unsigned long);
extern inline void init_timer(struct timer_list * timer)
{
timer->next = NULL;
timer->prev = NULL;
}
#endif