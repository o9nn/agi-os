#include <stdint.h>
#include <pthread.h>
#include "cons.h"
#include "priv.h"
int
_cons_vcons_scrollback (vcons_t vcons, cons_scroll_t type, float value)
{
int scrolling;
uint32_t new_scr;
switch (type)
{
case CONS_SCROLL_DELTA_LINES:
scrolling = vcons->scrolling + ((uint32_t) value);
break;
case CONS_SCROLL_DELTA_SCREENS:
scrolling = vcons->scrolling
+ ((uint32_t) (value * vcons->state.screen.height));
break;
case CONS_SCROLL_ABSOLUTE_LINE:
scrolling = (uint32_t) value;
break;
case CONS_SCROLL_ABSOLUTE_PERCENTAGE:
scrolling = (uint32_t) (value * vcons->state.screen.scr_lines);
break;
default:
return 0;
}
if (scrolling < 0)
new_scr = 0;
else if (scrolling > vcons->state.screen.scr_lines)
new_scr = vcons->state.screen.scr_lines;
else
new_scr = scrolling;
if (new_scr == vcons->scrolling)
return 0;
scrolling = vcons->scrolling - new_scr;
{
uint32_t new_cur_line;
off_t size = vcons->state.screen.width
* vcons->state.screen.lines;
off_t vis_start;
off_t start;
off_t end;
if (vcons->state.screen.cur_line >= new_scr)
new_cur_line = vcons->state.screen.cur_line - new_scr;
else
new_cur_line = (UINT32_MAX - (new_scr - vcons->state.screen.cur_line)) + 1;
if (scrolling > 0 && (uint32_t) scrolling > vcons->state.screen.height)
scrolling = vcons->state.screen.height;
else if (scrolling < 0
&& (uint32_t) (-scrolling) > vcons->state.screen.height)
scrolling = -vcons->state.screen.height;
if ((scrolling > 0 && scrolling < vcons->state.screen.height)
|| (scrolling < 0
&& (uint32_t) (-scrolling) < vcons->state.screen.height))
cons_vcons_scroll (vcons, scrolling);
else if ((scrolling > 0 && scrolling == vcons->state.screen.height)
|| (scrolling < 0
&& (uint32_t) (-scrolling) == vcons->state.screen.height))
cons_vcons_clear (vcons, vcons->state.screen.width
* vcons->state.screen.height, 0, 0);
vis_start = vcons->state.screen.width
* (new_cur_line % vcons->state.screen.lines);
if (scrolling > 0)
start = (((new_cur_line % vcons->state.screen.lines)
+ vcons->state.screen.height - scrolling)
* vcons->state.screen.width) % size;
else
start = vis_start;
end = start + abs (scrolling) * vcons->state.screen.width - 1;
cons_vcons_write (vcons,
vcons->state.screen.matrix + start,
end < size
? end - start + 1
: size - start,
0, (scrolling > 0)
? vcons->state.screen.height - scrolling : 0);
if (end >= size)
cons_vcons_write (vcons,
vcons->state.screen.matrix,
end - size + 1,
0, (size - vis_start)
/ vcons->state.screen.width);
}
{
uint32_t row = vcons->state.cursor.row;
uint32_t height = vcons->state.screen.height;
if (row + new_scr < height)
{
cons_vcons_set_cursor_pos (vcons, vcons->state.cursor.col,
row + new_scr);
if (row + vcons->scrolling >= height)
cons_vcons_set_cursor_status (vcons, vcons->state.cursor.status);
}
else if (row + vcons->scrolling < height)
cons_vcons_set_cursor_status (vcons, CONS_CURSOR_INVISIBLE);
}
vcons->scrolling -= scrolling;
return -scrolling;
}
int
cons_vcons_scrollback (vcons_t vcons, cons_scroll_t type, float value)
{
int ret;
pthread_mutex_lock (&vcons->lock);
ret = _cons_vcons_scrollback (vcons, type, value);
_cons_vcons_console_event (vcons, CONS_EVT_OUTPUT);
cons_vcons_update (vcons);
pthread_mutex_unlock (&vcons->lock);
return ret;
}