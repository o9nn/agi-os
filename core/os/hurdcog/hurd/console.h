#ifndef _HURD_CONSOLE_H
#define _HURD_CONSOLE_H
#include <stdint.h>
#include <string.h>
#include <wchar.h>
typedef enum
{
CONS_COLOR_BLACK = 0, CONS_COLOR_RED, CONS_COLOR_GREEN, CONS_COLOR_YELLOW,
CONS_COLOR_BLUE, CONS_COLOR_MAGENTA, CONS_COLOR_CYAN, CONS_COLOR_WHITE
} cons_color_t;
#define CONS_COLOR_MAX (CONS_COLOR_WHITE)
typedef struct
{
#define CONS_ATTR_INTENSITY_NORMAL 000000000000
#define CONS_ATTR_INTENSITY_BOLD 000000000001
#define CONS_ATTR_INTENSITY_DIM 000000000002
uint32_t intensity : 2;
uint32_t underlined : 1;
uint32_t blinking : 1;
uint32_t reversed : 1;
uint32_t concealed : 1;
uint32_t bgcol : 3;
uint32_t fgcol : 3;
uint32_t italic : 1;
uint32_t bold : 1;
} conchar_attr_t;
static inline int
conchar_attr_equal (conchar_attr_t *c1, conchar_attr_t *c2)
{
return !memcmp (c1, c2, sizeof (conchar_attr_t));
}
#define CONS_WCHAR_MASK ((wchar_t) 0x401fffff)
#define CONS_WCHAR_CONTINUED ((wchar_t) 0x40000000)
typedef struct
{
wchar_t chr;
conchar_attr_t attr;
} conchar_t;
typedef union
{
struct
{
uint32_t start;
uint32_t end;
} matrix;
struct
{
uint32_t cursor_pos : 1;
uint32_t cursor_status : 1;
uint32_t screen_cur_line : 1;
uint32_t screen_scr_lines : 1;
uint32_t bell_audible : 1;
uint32_t bell_visible : 1;
uint32_t flags : 1;
uint32_t _unused : 24;
uint32_t not_matrix : 1;
} what;
} cons_change_t;
struct cons_display
{
#define CONS_MAGIC 0x48555244
uint32_t magic;
#define CONS_VERSION_MAJ 0x0
#define CONS_VERSION_MAJ_SHIFT 16
#define CONS_VERSION_AGE 0x0
uint32_t version;
#define CONS_FLAGS_SCROLL_LOCK 0x00000001
#define CONS_FLAGS_TRACK_MOUSE 0x00000002
uint32_t flags;
struct
{
uint32_t width;
uint32_t lines;
uint32_t cur_line;
uint32_t scr_lines;
uint32_t height;
uint32_t matrix;
} screen;
struct
{
uint32_t col;
uint32_t row;
#define CONS_CURSOR_INVISIBLE 0
#define CONS_CURSOR_NORMAL 1
#define CONS_CURSOR_VERY_VISIBLE 2
uint32_t status;
} cursor;
struct
{
uint32_t audible;
uint32_t visible;
} bell;
struct
{
uint32_t buffer;
uint32_t length;
uint32_t written;
#define _CONS_CHANGES_LENGTH 512
cons_change_t _buffer[_CONS_CHANGES_LENGTH];
} changes;
conchar_t _matrix[0];
};
#define CONS_CHAR_BLOCK ((wchar_t) 0x2588)
#define CONS_CHAR_DIAMOND ((wchar_t) 0x25c6)
#define CONS_CHAR_CKBOARD ((wchar_t) 0x2592)
#define CONS_CHAR_BOARD ((wchar_t) 0x2591)
#define CONS_CHAR_BULLET ((wchar_t) 0x2022)
#define CONS_CHAR_STERLING ((wchar_t) 0x00a3)
#define CONS_CHAR_DEGREE ((wchar_t) 0x00b0)
#define CONS_CHAR_PLMINUS ((wchar_t) 0x00b1)
#define CONS_CHAR_PI ((wchar_t) 0x03c0)
#define CONS_CHAR_LANTERN ((wchar_t) 0x29d7)
#define CONS_CHAR_RARROW ((wchar_t) 0x2192)
#define CONS_CHAR_LARROW ((wchar_t) 0x2190)
#define CONS_CHAR_UARROW ((wchar_t) 0x2191)
#define CONS_CHAR_DARROW ((wchar_t) 0x2193)
#define CONS_CHAR_LRCORNER ((wchar_t) 0x2518)
#define CONS_CHAR_URCORNER ((wchar_t) 0x2510)
#define CONS_CHAR_ULCORNER ((wchar_t) 0x250c)
#define CONS_CHAR_LLCORNER ((wchar_t) 0x2514)
#define CONS_CHAR_PLUS ((wchar_t) 0x253c)
#define CONS_CHAR_HLINE ((wchar_t) 0x2500)
#define CONS_CHAR_LTEE ((wchar_t) 0x251c)
#define CONS_CHAR_RTEE ((wchar_t) 0x2524)
#define CONS_CHAR_BTEE ((wchar_t) 0x2534)
#define CONS_CHAR_TTEE ((wchar_t) 0x252c)
#define CONS_CHAR_VLINE ((wchar_t) 0x2502)
#define CONS_CHAR_S1 ((wchar_t) 0x23ba)
#define CONS_CHAR_S3 ((wchar_t) 0x23bb)
#define CONS_CHAR_S7 ((wchar_t) 0x23bc)
#define CONS_CHAR_S9 ((wchar_t) 0x23bd)
#define CONS_CHAR_NEQUAL ((wchar_t) 0x2260)
#define CONS_CHAR_LEQUAL ((wchar_t) 0x2264)
#define CONS_CHAR_GEQUAL ((wchar_t) 0x2265)
#define CONS_KEY_UP "\eOA"
#define CONS_KEY_DOWN "\eOB"
#define CONS_KEY_RIGHT "\eOC"
#define CONS_KEY_LEFT "\eOD"
#define CONS_KEY_BACKSPACE "\177"
#define CONS_KEY_F1 "\eOP"
#define CONS_KEY_F2 "\eOQ"
#define CONS_KEY_F3 "\eOR"
#define CONS_KEY_F4 "\eOS"
#define CONS_KEY_F5 "\e[15~"
#define CONS_KEY_F6 "\e[17~"
#define CONS_KEY_F7 "\e[18~"
#define CONS_KEY_F8 "\e[19~"
#define CONS_KEY_F9 "\e[20~"
#define CONS_KEY_F10 "\e[21~"
#define CONS_KEY_F11 "\e[23~"
#define CONS_KEY_F12 "\e[24~"
#define CONS_KEY_F13 "\e[25~"
#define CONS_KEY_F14 "\e[26~"
#define CONS_KEY_F15 "\e[28~"
#define CONS_KEY_F16 "\e[29~"
#define CONS_KEY_F17 "\e[31~"
#define CONS_KEY_F18 "\e[32~"
#define CONS_KEY_F19 "\e[33~"
#define CONS_KEY_F20 "\e[34~"
#define CONS_KEY_HOME "\e[1~"
#define CONS_KEY_IC "\e[2~"
#define CONS_KEY_DC "\e[3~"
#define CONS_KEY_END "\e[4~"
#define CONS_KEY_PPAGE "\e[5~"
#define CONS_KEY_NPAGE "\e[6~"
#define CONS_KEY_BTAB "\e[Z"
#define CONS_KEY_B2 "\e[G"
#define CONS_MOUSE_BUTTON_MASK 0x03
#define CONS_MOUSE_BUTTON1 0x00
#define CONS_MOUSE_BUTTON2 0x01
#define CONS_MOUSE_BUTTON3 0x02
#define CONS_MOUSE_RELEASE 0x03
#define CONS_MOUSE_MOD_MASK 0x1c
#define CONS_MOUSE_MOD_SHIFT 0x04
#define CONS_MOUSE_MOD_META 0x08
#define CONS_MOUSE_MOD_CTRL 0x10
#define CONS_MOUSE_OFFSET_BASE 0x20
#define CONS_MOUSE_EVENT_LENGTH 6
#define CONS_MOUSE_EVENT_PREFIX "\e[M"
#define CONS_MOUSE_EVENT(str,event,x,y) \
(((int)(x) < 0 || (int)(x) + CONS_MOUSE_OFFSET_BASE > 255 \
|| (int)(y) < 0 || (int)(y) + CONS_MOUSE_OFFSET_BASE > 255) ? 0 \
: ((*(str) = CONS_MOUSE_EVENT_PREFIX[0]), \
(*((str) + 1) = CONS_MOUSE_EVENT_PREFIX[1]), \
(*((str) + 2) = CONS_MOUSE_EVENT_PREFIX[2]), \
(*((str) + 3) = (char)((int)(event) + CONS_MOUSE_OFFSET_BASE)), \
(*((str) + 4) = (char)((int)(x) + CONS_MOUSE_OFFSET_BASE)), \
(*((str) + 5) = (char)((int)(y) + CONS_MOUSE_OFFSET_BASE), 1)))
#endif