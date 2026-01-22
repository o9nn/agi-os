#ifndef _CONS_PRIV_H
#define _CONS_PRIV_H
#include "cons.h"
typedef enum
{
BELL_OFF,
BELL_VISUAL,
BELL_AUDIBLE
} bell_type_t;
#define CONS_EVT_MOUSE_MOVE	(1 << 1)
#define CONS_EVT_MOUSE_BUTTON	(1 << 2)
#define CONS_EVT_KEYPRESS	(1 << 4)
#define CONS_EVT_OUTPUT		(1 << 8)
extern int _cons_slack;
extern int _cons_jump_down_on_input;
extern int _cons_jump_down_on_output;
extern bell_type_t _cons_visual_bell;
extern bell_type_t _cons_audible_bell;
extern int _cons_show_mouse;
extern int _cons_hide_mouse;
extern float _cons_mouse_sens;
int _cons_vcons_scrollback (vcons_t vcons, cons_scroll_t type, float value);
error_t _cons_vcons_input (vcons_t vcons, char *buf, size_t size);
void _cons_vcons_console_event (vcons_t vcons, int event);
static inline cons_notify_t
begin_using_notify_port (fs_notify_t port)
{
return ports_lookup_port (cons_port_bucket, port, cons_port_class);
}
static inline cons_notify_t
begin_using_notify_payload (uintptr_t payload)
{
return ports_lookup_payload (cons_port_bucket, payload, cons_port_class);
}
static inline void
end_using_notify_port (cons_notify_t cred)
{
if (cred)
ports_port_deref (cred);
}
#endif