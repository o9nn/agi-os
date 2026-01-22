#include <assert-backtrace.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <wchar.h>
#include <curses.h>
#include <pthread.h>
#include <hurd/console.h>
#include "driver.h"
static pthread_mutex_t ncurses_lock;
static unsigned int current_width;
static unsigned int current_height;
static WINDOW *conspad;
static unsigned int padx;
static unsigned int pady;
static int autoscroll;
static struct display_ops ncursesw_display_ops;
static struct input_ops ncursesw_input_ops;
static struct bell_ops ncursesw_bell_ops;
struct curses_kc_to_cons_kc
{
int curses;
char *cons;
};
static struct curses_kc_to_cons_kc keycodes[] =
{
{ KEY_BREAK, NULL },
{ KEY_DOWN, CONS_KEY_DOWN },
{ KEY_UP, CONS_KEY_UP },
{ KEY_RIGHT, CONS_KEY_RIGHT },
{ KEY_LEFT, CONS_KEY_LEFT },
{ KEY_HOME, CONS_KEY_HOME },
{ KEY_BACKSPACE, CONS_KEY_BACKSPACE },
{ KEY_F(1), CONS_KEY_F1 },
{ KEY_F(2), CONS_KEY_F2 },
{ KEY_F(3), CONS_KEY_F3 },
{ KEY_F(4), CONS_KEY_F4 },
{ KEY_F(5), CONS_KEY_F5 },
{ KEY_F(6), CONS_KEY_F6 },
{ KEY_F(7), CONS_KEY_F7 },
{ KEY_F(8), CONS_KEY_F8 },
{ KEY_F(9), CONS_KEY_F9 },
{ KEY_F(10), CONS_KEY_F10 },
{ KEY_DL, NULL },
{ KEY_IL, NULL },
{ KEY_DC, CONS_KEY_DC },
{ KEY_IC, CONS_KEY_IC },
{ KEY_EIC, NULL },
{ KEY_CLEAR, NULL },
{ KEY_EOS, NULL },
{ KEY_EOL, NULL },
{ KEY_SF, NULL },
{ KEY_SR, NULL },
{ KEY_NPAGE, CONS_KEY_NPAGE },
{ KEY_PPAGE, CONS_KEY_PPAGE },
{ KEY_STAB, NULL },
{ KEY_CTAB, NULL },
{ KEY_CATAB, NULL },
{ KEY_ENTER, NULL },
{ KEY_SRESET, NULL },
{ KEY_RESET, NULL },
{ KEY_PRINT, NULL },
{ KEY_LL, NULL },
{ KEY_A1, NULL },
{ KEY_A3, NULL },
{ KEY_B2, NULL },
{ KEY_C1, NULL },
{ KEY_C3, NULL },
{ KEY_BTAB, CONS_KEY_BTAB },
{ KEY_BEG, NULL },
{ KEY_CANCEL, NULL },
{ KEY_CLOSE, NULL },
{ KEY_COMMAND, NULL },
{ KEY_COPY, NULL },
{ KEY_CREATE, NULL },
{ KEY_END, CONS_KEY_END },
{ KEY_EXIT, NULL },
{ KEY_FIND, NULL },
{ KEY_HELP, NULL },
{ KEY_MARK, NULL },
{ KEY_MESSAGE, NULL },
{ KEY_MOUSE, NULL },
{ KEY_MOVE, NULL },
{ KEY_NEXT, NULL },
{ KEY_OPEN, NULL },
{ KEY_OPTIONS, NULL },
{ KEY_PREVIOUS, NULL },
{ KEY_REDO, NULL },
{ KEY_REFERENCE, NULL },
{ KEY_REFRESH, NULL },
{ KEY_REPLACE, NULL },
{ KEY_RESIZE, NULL },
{ KEY_RESTART, NULL },
{ KEY_RESUME, NULL },
{ KEY_SAVE, NULL },
{ KEY_SBEG, NULL },
{ KEY_SCANCEL, NULL },
{ KEY_SCOMMAND, NULL },
{ KEY_SCOPY, NULL },
{ KEY_SCREATE, NULL },
{ KEY_SDC, NULL },
{ KEY_SDL, NULL },
{ KEY_SELECT, NULL },
{ KEY_SEND, NULL },
{ KEY_SEOL, NULL },
{ KEY_SEXIT, NULL },
{ KEY_SFIND, NULL },
{ KEY_SHELP, NULL },
{ KEY_SHOME, NULL },
{ KEY_SIC, NULL },
{ KEY_SLEFT, NULL },
{ KEY_SMESSAGE, NULL },
{ KEY_SMOVE, NULL },
{ KEY_SNEXT, NULL },
{ KEY_SOPTIONS, NULL },
{ KEY_SPREVIOUS, NULL },
{ KEY_SPRINT, NULL },
{ KEY_SREDO, NULL },
{ KEY_SREPLACE, NULL },
{ KEY_SRIGHT, NULL },
{ KEY_SRSUME, NULL },
{ KEY_SSAVE, NULL },
{ KEY_SSUSPEND, NULL },
{ KEY_SUNDO, NULL },
{ KEY_SUSPEND, NULL },
{ KEY_UNDO, NULL }
};
static int
ucs4_to_altchar (wchar_t chr, chtype *achr)
{
switch (chr)
{
case CONS_CHAR_RARROW:
*achr = ACS_RARROW;
break;
case CONS_CHAR_LARROW:
*achr = ACS_LARROW;
break;
case CONS_CHAR_UARROW:
*achr = ACS_UARROW;
break;
case CONS_CHAR_DARROW:
*achr = ACS_DARROW;
break;
case CONS_CHAR_BLOCK:
*achr = ACS_BLOCK;
break;
case CONS_CHAR_LANTERN:
*achr = ACS_LANTERN;
break;
case CONS_CHAR_DIAMOND:
*achr = ACS_DIAMOND;
break;
case CONS_CHAR_CKBOARD:
*achr = ACS_CKBOARD;
break;
case CONS_CHAR_DEGREE:
*achr = ACS_DEGREE;
break;
case CONS_CHAR_PLMINUS:
*achr = ACS_PLMINUS;
break;
case CONS_CHAR_BOARD:
*achr = ACS_BOARD;
break;
case CONS_CHAR_LRCORNER:
*achr = ACS_LRCORNER;
break;
case CONS_CHAR_URCORNER:
*achr = ACS_URCORNER;
break;
case CONS_CHAR_ULCORNER:
*achr = ACS_ULCORNER;
break;
case CONS_CHAR_LLCORNER:
*achr = ACS_LLCORNER;
break;
case CONS_CHAR_PLUS:
*achr = ACS_PLUS;
break;
case CONS_CHAR_S1:
*achr = ACS_S1;
break;
case CONS_CHAR_S3:
*achr = ACS_S3;
break;
case CONS_CHAR_HLINE:
*achr = ACS_HLINE;
break;
case CONS_CHAR_S7:
*achr = ACS_S7;
break;
case CONS_CHAR_S9:
*achr = ACS_S9;
break;
case CONS_CHAR_LTEE:
*achr = ACS_LTEE;
break;
case CONS_CHAR_RTEE:
*achr = ACS_RTEE;
break;
case CONS_CHAR_BTEE:
*achr = ACS_BTEE;
break;
case CONS_CHAR_TTEE:
*achr = ACS_TTEE;
break;
case CONS_CHAR_VLINE:
*achr = ACS_VLINE;
break;
case CONS_CHAR_LEQUAL:
*achr = ACS_LEQUAL;
break;
case CONS_CHAR_GEQUAL:
*achr = ACS_GEQUAL;
break;
case CONS_CHAR_PI:
*achr = ACS_PI;
break;
case CONS_CHAR_NEQUAL:
*achr = ACS_NEQUAL;
break;
case CONS_CHAR_STERLING:
*achr = ACS_STERLING;
break;
case CONS_CHAR_BULLET:
*achr = ACS_BULLET;
break;
default:
return 0;
}
return 1;
}
static error_t
refresh_screen (void)
{
if (!current_width && !current_height)
return 0;
return prefresh (conspad, pady, padx, 0, 0,
(current_height <= (unsigned int) LINES
? current_height : (unsigned int) LINES) - 1,
(current_width <= (unsigned int) COLS
? current_width : (unsigned int) COLS) - 1);
}
static void *
input_loop (void *unused)
{
int fd = 0;
fd_set rfds;
int w_escaped = 0;
pthread_setname_np (pthread_self (), "input");
FD_ZERO (&rfds);
FD_SET (fd, &rfds);
while (1)
{
int ret;
FD_SET (fd, &rfds);
ret = select (fd + 1, &rfds, 0, 0, 0);
if (ret == 1)
{
char buffer[100];
char *buf = buffer;
size_t size = 0;
pthread_mutex_lock (&ncurses_lock);
while ((ret = wgetch (conspad)) != ERR)
{
unsigned int i;
int found;
if (w_escaped)
{
switch (ret)
{
case 'x':
pthread_mutex_unlock (&ncurses_lock);
console_exit ();
break;
case 23:
assert_backtrace (size < 100);
buf[size++] = ret;
break;
case '1':
case '2':
case '3':
case '4':
case '5':
case '6':
case '7':
case '8':
case '9':
pthread_mutex_unlock (&ncurses_lock);
console_switch (1 + (ret - '1'), 0);
pthread_mutex_lock (&ncurses_lock);
break;
case 'j':
if (padx > 0)
{
padx--;
refresh_screen ();
}
break;
case 'k':
if (pady < current_height - LINES)
{
pady++;
refresh_screen ();
}
break;
case 'l':
if (padx < current_width - COLS)
{
padx++;
refresh_screen ();
}
break;
case 'i':
if (pady > 0)
{
pady--;
refresh_screen ();
}
break;
case 'a':
autoscroll = !autoscroll;
break;
default:
break;
}
w_escaped = 0;
}
else
switch (ret)
{
case 23:
w_escaped = 1;
break;
default:
found = 0;
for (i = 0; i < sizeof (keycodes) / sizeof (keycodes[0]);
i++)
{
if (keycodes[i].curses == ret)
{
if (keycodes[i].cons)
{
assert_backtrace (size
< 101 - strlen (keycodes[i].cons));
strcpy (&buf[size], keycodes[i].cons);
size += strlen (keycodes[i].cons);
}
found = 1;
break;
}
}
if (!found)
{
assert_backtrace (size < 100);
buf[size++] = ret;
}
break;
}
}
pthread_mutex_unlock (&ncurses_lock);
if (size)
console_input (buf, size);
}
}
}
static inline attr_t
conchar_attr_to_attr (conchar_attr_t attr)
{
return ((attr.intensity == CONS_ATTR_INTENSITY_BOLD
? A_BOLD : (attr.intensity == CONS_ATTR_INTENSITY_DIM
? A_DIM : A_NORMAL))
| (attr.underlined ? A_UNDERLINE : 0)
| (attr.reversed ? A_REVERSE : 0)
| (attr.blinking ? A_BLINK: 0)
| (attr.concealed ? A_INVIS : 0));
}
static inline short
conchar_attr_to_color_pair (conchar_attr_t attr)
{
return attr.bgcol << 3 | attr.fgcol;
}
static void
mvwputsn (conchar_t *str, size_t len, off_t x, off_t y)
{
cchar_t chr;
wchar_t wch[2] = { L'\0', L'\0' };
uint32_t last_attr = * (uint32_t *) &str->attr;
attr_t attr = conchar_attr_to_attr (str->attr);
short color_pair = conchar_attr_to_color_pair (str->attr);
wmove (conspad, y, x);
while (len)
{
int ret;
chtype ac;
if (last_attr != *(uint32_t *) &str->attr)
{
last_attr = * (uint32_t *) &str->attr;
attr = conchar_attr_to_attr (str->attr);
color_pair = conchar_attr_to_color_pair (str->attr);
}
if (ucs4_to_altchar (str->chr, &ac))
waddch (conspad, ac | attr | color_pair);
else
{
wch[0] = str->chr;
ret = setcchar (&chr, wch, attr, color_pair, NULL);
#if 0
if (ret == ERR)
{
printf ("setcchar failed: %s\n", strerror (errno));
printf ("[%lc]\n", wch[0]);
assert_backtrace (!"Do something if setcchar fails.");
}
#endif
ret = wadd_wch (conspad, &chr);
#if 0
if (ret == ERR)
{
printf ("add_wch failed: %i, %s\n", ret, strerror (errno));
printf ("[%lc]\n", wch[0]);
assert_backtrace (!"Do something if add_wchr fails.");
}
#endif
}
len--;
str++;
}
}
static error_t
ncursesw_update (void *handle)
{
pthread_mutex_lock (&ncurses_lock);
refresh_screen ();
pthread_mutex_unlock (&ncurses_lock);
return 0;
}
static error_t
ncursesw_set_cursor_pos (void *handle, uint32_t col, uint32_t row)
{
pthread_mutex_lock (&ncurses_lock);
assert_backtrace (current_width && current_height);
if (autoscroll)
{
if (col > COLS + padx)
{
padx += COLS / 2;
if (padx > COLS + current_width)
padx = current_width - COLS;
refresh_screen ();
}
else if (col < padx)
{
padx -= COLS / 2;
if (padx < 0)
padx = 0;
refresh_screen ();
}
if (row > LINES + pady)
{
pady += LINES / 2;
if (pady > LINES + current_height)
pady = current_height - LINES;
refresh_screen ();
}
else if (row < pady)
{
pady -= LINES / 2;
if (pady < 0)
pady = 0;
refresh_screen ();
}
}
wmove (conspad, row, col);
pthread_mutex_unlock (&ncurses_lock);
return 0;
}
static error_t
ncursesw_set_cursor_status (void *handle, uint32_t status)
{
pthread_mutex_lock (&ncurses_lock);
if (curs_set (status) == -1 && status)
curs_set (status == 1 ? 2 : 1);
pthread_mutex_unlock (&ncurses_lock);
return 0;
}
static error_t
ncursesw_scroll (void *handle, int delta)
{
assert_backtrace (delta >= 0);
pthread_mutex_lock (&ncurses_lock);
idlok (conspad, TRUE);
scrollok (conspad, TRUE);
wscrl (conspad, delta);
idlok (conspad, FALSE);
scrollok (conspad, FALSE);
pthread_mutex_unlock (&ncurses_lock);
return 0;
}
static error_t
ncursesw_write (void *handle, conchar_t *str, size_t length,
uint32_t col, uint32_t row)
{
int x;
int y;
pthread_mutex_lock (&ncurses_lock);
getyx (conspad, y, x);
mvwputsn (str, length, col, row);
wmove (conspad, y, x);
pthread_mutex_unlock (&ncurses_lock);
return 0;
}
static error_t
ncursesw_flash (void *handle)
{
pthread_mutex_lock (&ncurses_lock);
flash ();
pthread_mutex_unlock (&ncurses_lock);
return 0;
}
error_t
ncursesw_beep (void *handle)
{
pthread_mutex_lock (&ncurses_lock);
beep ();
pthread_mutex_unlock (&ncurses_lock);
return 0;
}
static error_t
ncursesw_driver_init (void **handle, int no_exit,
int argc, char *argv[], int *next)
{
pthread_mutex_init (&ncurses_lock, NULL);
return 0;
}
static error_t
ncursesw_driver_start (void *handle)
{
pthread_t thread;
error_t err;
int i;
initscr ();
start_color ();
for (i = 0; i < 64; i++)
init_pair (i, i & 7, i >> 3);
raw ();
noecho ();
nonl ();
conspad = newpad (1, 1);
if (!conspad)
return errno;
intrflush (conspad, FALSE);
nodelay (conspad, TRUE);
wtimeout (conspad, 1);
keypad (conspad, TRUE);
err = driver_add_display (&ncursesw_display_ops, NULL);
if (err)
{
endwin ();
return err;
}
err = driver_add_input (&ncursesw_input_ops, NULL);
if (err)
{
err = driver_remove_display (&ncursesw_display_ops, NULL);
endwin ();
return err;
}
err = driver_add_bell (&ncursesw_bell_ops, NULL);
if (err)
{
err = driver_remove_input (&ncursesw_input_ops, NULL);
err = driver_remove_display (&ncursesw_display_ops, NULL);
endwin ();
return err;
}
err = pthread_create (&thread, NULL, input_loop, NULL);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
return 0;
}
static error_t
ncursesw_driver_fini (void *handle, int force)
{
pthread_mutex_lock (&ncurses_lock);
driver_remove_display (&ncursesw_display_ops, NULL);
driver_remove_input (&ncursesw_input_ops, NULL);
driver_remove_bell (&ncursesw_bell_ops, NULL);
pthread_mutex_unlock (&ncurses_lock);
endwin ();
return 0;
}
static error_t
ncursesw_set_dimension (void *handle, unsigned int width, unsigned int height)
{
pthread_mutex_lock (&ncurses_lock);
if (width != current_width || height != current_height)
{
wresize (conspad, height, width);
padx = 0;
pady = 0;
}
current_width = width;
current_height = height;
pthread_mutex_unlock(&ncurses_lock);
return 0;
}
struct driver_ops driver_ncursesw_ops =
{
ncursesw_driver_init,
ncursesw_driver_start,
ncursesw_driver_fini,
};
static struct display_ops ncursesw_display_ops =
{
ncursesw_set_cursor_pos,
ncursesw_set_cursor_status,
ncursesw_scroll,
NULL,
ncursesw_write,
ncursesw_update,
ncursesw_flash,
NULL,
ncursesw_set_dimension
};
static struct input_ops ncursesw_input_ops =
{
NULL,
NULL
};
static struct bell_ops ncursesw_bell_ops =
{
ncursesw_beep,
NULL
};