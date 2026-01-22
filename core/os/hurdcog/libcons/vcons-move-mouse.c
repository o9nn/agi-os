#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include "cons.h"
#include "priv.h"
static float mousepos_x;
static float mousepos_y;
error_t
cons_vcons_move_mouse (vcons_t vcons, mouse_event_t ev)
{
char event[CONS_MOUSE_EVENT_LENGTH];
uint32_t report_events;
pthread_mutex_lock (&vcons->lock);
report_events = vcons->display->flags & CONS_FLAGS_TRACK_MOUSE;
switch (ev->mouse_movement)
{
case CONS_VCONS_MOUSE_MOVE_REL:
mousepos_x += ((float) ev->x / _cons_mouse_sens);
mousepos_y += ((float) ev->y / _cons_mouse_sens);
break;
case CONS_VCONS_MOUSE_MOVE_ABS_PERCENT:
mousepos_x = vcons->state.screen.width * ev->x / 100;
mousepos_y = vcons->state.screen.height * ev->y / 100;
break;
case CONS_VCONS_MOUSE_MOVE_ABS:
mousepos_x = ev->x;
mousepos_y = ev->y;
break;
}
if (mousepos_x < 0)
mousepos_x = 0;
if (mousepos_y < 0)
mousepos_y = 0;
if (mousepos_x >= (float) vcons->state.screen.width)
mousepos_x = vcons->state.screen.width - 1;
if (mousepos_y >= (float) vcons->state.screen.height)
mousepos_y = vcons->state.screen.height - 1;
cons_vcons_set_mousecursor_pos (vcons, (float) mousepos_x, (float) mousepos_y);
if (ev->x || ev->y)
_cons_vcons_console_event (vcons, CONS_EVT_MOUSE_MOVE);
if (ev->mouse_button != CONS_VCONS_MOUSE_BUTTON_NO_OP)
_cons_vcons_console_event (vcons, CONS_EVT_MOUSE_BUTTON);
if (report_events)
{
switch (ev->mouse_button)
{
case CONS_VCONS_MOUSE_BUTTON_NO_OP:
break;
case CONS_VCONS_MOUSE_BUTTON_PRESSED:
if (CONS_MOUSE_EVENT (event, ev->button, (int) mousepos_x + 1, (int) mousepos_y + 1))
_cons_vcons_input (vcons, event, CONS_MOUSE_EVENT_LENGTH);
break;
case CONS_VCONS_MOUSE_BUTTON_RELEASED:
if (CONS_MOUSE_EVENT (event, CONS_MOUSE_RELEASE, (int) mousepos_x + 1, (int) mousepos_y + 1))
_cons_vcons_input (vcons, event, CONS_MOUSE_EVENT_LENGTH);
break;
}
}
pthread_mutex_unlock (&vcons->lock);
return 0;
}