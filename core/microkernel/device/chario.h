#ifndef _DEVICE_CHARIO_H_
#define _DEVICE_CHARIO_H_
#include <device/tty.h>
extern void chario_init(void);
void queue_delayed_reply(
queue_t qh,
io_req_t ior,
boolean_t (*io_done)(io_req_t));
void tty_output(struct tty *tp);
boolean_t char_open_done(io_req_t);
boolean_t char_read_done(io_req_t);
boolean_t char_write_done(io_req_t);
#endif