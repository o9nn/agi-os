#include <errno.h>
#include <assert-backtrace.h>
#include "cons.h"
#include "priv.h"
void
cons_vcons_refresh (vcons_t vcons)
{
uint32_t start;
vcons->state.screen.cur_line = vcons->display->screen.cur_line;
vcons->state.screen.scr_lines = vcons->display->screen.scr_lines;
vcons->state.cursor.col = vcons->display->cursor.col;
vcons->state.cursor.row = vcons->display->cursor.row;
vcons->state.cursor.status = vcons->display->cursor.status;
vcons->state.bell.audible = vcons->display->bell.audible;
vcons->state.bell.visible = vcons->display->bell.visible;
vcons->state.flags = vcons->display->flags;
vcons->state.changes.written = vcons->display->changes.written;
if (vcons->state.screen.scr_lines < vcons->scrolling)
vcons->scrolling = vcons->scrolling;
cons_vcons_set_dimension (vcons, vcons->state.screen.width,
vcons->state.screen.height);
if (vcons->state.screen.cur_line >= vcons->scrolling)
start = vcons->state.screen.cur_line - vcons->scrolling;
else
start = (UINT32_MAX
- (vcons->scrolling - vcons->state.screen.cur_line)) + 1;
start %= vcons->state.screen.lines;
cons_vcons_write (vcons, vcons->state.screen.matrix
+ start * vcons->state.screen.width,
((vcons->state.screen.lines - start
< vcons->state.screen.height)
? vcons->state.screen.lines - start
: vcons->state.screen.height)
* vcons->state.screen.width, 0, 0);
if (vcons->state.screen.lines - start < vcons->state.screen.height)
cons_vcons_write (vcons, vcons->state.screen.matrix,
vcons->state.screen.height * vcons->state.screen.width
- (vcons->state.screen.lines - start)
* vcons->state.screen.width, 0,
vcons->state.screen.lines - start);
cons_vcons_set_cursor_pos (vcons, vcons->state.cursor.col,
vcons->state.cursor.row);
cons_vcons_set_cursor_status (vcons, vcons->state.cursor.status);
cons_vcons_set_scroll_lock (vcons, vcons->state.flags
& CONS_FLAGS_SCROLL_LOCK);
_cons_vcons_console_event (vcons, CONS_EVT_OUTPUT);
cons_vcons_update (vcons);
}