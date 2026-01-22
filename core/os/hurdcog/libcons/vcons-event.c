#include "cons.h"
#include "priv.h"
void
_cons_vcons_console_event (vcons_t vcons, int event)
{
if (_cons_show_mouse & event)
cons_vcons_set_mousecursor_status (vcons, 1);
else if (_cons_hide_mouse & event)
cons_vcons_set_mousecursor_status (vcons, 0);
}