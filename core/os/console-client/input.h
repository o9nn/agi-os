#ifndef _INPUT_H_
#define _INPUT_H_ 1
#include <errno.h>
#include <stddef.h>
#include <hurd/cons.h>
struct input_ops;
typedef struct input_ops *input_ops_t;
error_t driver_add_input (input_ops_t ops, void *handle);
error_t driver_remove_input (input_ops_t ops, void *handle);
error_t console_input (char *buf, size_t size);
int console_scrollback (cons_scroll_t type, float value);
error_t console_current_id (int *cur);
error_t console_switch (int id, int delta);
void console_error (const wchar_t *const err_msg);
void console_exit (void) __attribute__ ((noreturn));
void console_switch_away (void);
void console_switch_back (void);
error_t console_move_mouse (mouse_event_t ev);
#if QUAERENDO_INVENIETIS
void console_deprecated (int key);
#endif
struct input_ops
{
error_t (*set_scroll_lock_status) (void *handle, int onoff);
void (*deprecated) (void *handle, int key);
};
#endif