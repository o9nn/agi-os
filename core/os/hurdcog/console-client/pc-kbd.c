#include <errno.h>
#include <assert-backtrace.h>
#include <string.h>
#include <iconv.h>
#include <sys/mman.h>
#include <argp.h>
#include <device/device.h>
#include <pthread.h>
#include <hurd/console.h>
#include <hurd/cons.h>
#include "driver.h"
#include "mach-inputdev.h"
#ifdef XKB_SUPPORT
#include "xkb/xkb.h"
#endif
#define DEFAULT_REPEATER_NODE "kbd"
static device_t kbd_dev;
static iconv_t cd;
struct {
int scroll_lock : 1;
int num_lock : 1;
int caps_lock : 1;
} led_state;
int gnumach_v1_compat;
static struct input_ops pc_kbd_ops;
static char *repeater_node;
static consnode_t cnode;
#ifdef XKB_SUPPORT
static int ctrlaltbs;
static int xkb_repeat_delay;
static int xkb_repeat_interval;
#endif
enum scancode
{
SC_F9 = 0x01,
SC_F5 = 0x03,
SC_F3 = 0x04,
SC_F1 = 0x05,
SC_F2 = 0x06,
SC_F12 = 0x07,
SC_F10 = 0x09,
SC_F8 = 0x0A,
SC_F6 = 0x0B,
SC_F4 = 0x0C,
SC_TAB = 0x0D,
SC_BACKQUOTE = 0x0E,
SC_LEFT_ALT = 0x11,
SC_LEFT_SHIFT = 0x12,
SC_LEFT_CTRL = 0x14,
SC_Q = 0x15,
SC_1 = 0x16,
SC_Z = 0x1A,
SC_S = 0x1B,
SC_A = 0x1C,
SC_W = 0x1D,
SC_2 = 0x1E,
SC_C = 0x21,
SC_X = 0x22,
SC_D = 0x23,
SC_E = 0x24,
SC_4 = 0x25,
SC_3 = 0x26,
SC_SPACE = 0x29,
SC_V = 0x2A,
SC_F = 0x2B,
SC_T = 0x2C,
SC_R = 0x2D,
SC_5 = 0x2E,
SC_N = 0x31,
SC_B = 0x32,
SC_H = 0x33,
SC_G = 0x34,
SC_Y = 0x35,
SC_6 = 0x36,
SC_M = 0x3A,
SC_J = 0x3B,
SC_U = 0x3C,
SC_7 = 0x3D,
SC_8 = 0x3E,
SC_COMMA = 0x41,
SC_K = 0x42,
SC_I = 0x43,
SC_O = 0x44,
SC_0 = 0x45,
SC_9 = 0x46,
SC_PERIOD = 0x49,
SC_SLASH = 0x4A,
SC_L = 0x4B,
SC_SEMICOLON = 0x4C,
SC_P = 0x4D,
SC_MINUS = 0x4E,
SC_APOSTROPHE = 0x52,
SC_LEFT_BRACKET = 0x54,
SC_EQUAL = 0x55,
SC_CAPSLOCK = 0x58,
SC_RIGHT_SHIFT = 0x59,
SC_ENTER = 0x5A,
SC_RIGHT_BRACKET = 0x5B,
SC_BACKSLASH = 0x5D,
SC_BACKSPACE = 0x66,
SC_PAD_1 = 0x69,
SC_PAD_4 = 0x6B,
SC_PAD_7 = 0x6C,
SC_PAD_0 = 0x70,
SC_PAD_DECIMAL = 0x71,
SC_PAD_2 = 0x72,
SC_PAD_5 = 0x73,
SC_PAD_6 = 0x74,
SC_PAD_8 = 0x75,
SC_ESC = 0x76,
SC_NUMLOCK = 0x77,
SC_F11 = 0x78,
SC_PAD_PLUS = 0x79,
SC_PAD_3 = 0x7A,
SC_PAD_MINUS = 0x7B,
SC_PAD_ASTERISK = 0x7C,
SC_PAD_9 = 0x7D,
SC_SCROLLLOCK = 0x7E,
SC_F7 = 0x83,
SC_EXTENDED1 = 0xE0,
SC_EXTENDED2 = 0xE1,
SC_ERROR = 0xFF,
SC_FLAG_UP = 0xF000
};
#define IS_FUNC_KEY(c) ((sc >= SC_F9 && sc <= SC_F4) || \
sc == SC_F7 || sc == SC_F11)
enum scancode_x1
{
SC_X1_RIGHT_ALT = 0x11,
SC_X1_PRTSC = 0x12,
SC_X1_RIGHT_CTRL = 0x14,
SC_X1_LEFT_GUI = 0x1F,
SC_X1_RIGHT_GUI = 0x27,
SC_X1_APPS = 0x2F,
SC_X1_POWER = 0x37,
SC_X1_SLEEP = 0x3F,
SC_X1_PAD_SLASH = 0x4A,
SC_X1_PAD_ENTER = 0x5A,
SC_X1_WAKEUP = 0x5E,
SC_X1_END = 0x69,
SC_X1_LEFT = 0x6B,
SC_X1_HOME = 0x6C,
SC_X1_INS = 0x70,
SC_X1_DEL = 0x71,
SC_X1_DOWN = 0x72,
SC_X1_RIGHT = 0x74,
SC_X1_UP = 0x75,
SC_X1_PGDN = 0x7A,
SC_X1_PGUP = 0x7D
};
enum scancode_x2
{
SC_X2_BREAK = 0x1477,
};
char *sc_to_kc[][7] =
{
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_F9, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_F5, CONS_KEY_F17, 0, 0, 0, 0, 0 },
{ CONS_KEY_F3, CONS_KEY_F15, 0, 0, 0, 0, 0 },
{ CONS_KEY_F1, CONS_KEY_F13, 0, 0, 0, 0, 0 },
{ CONS_KEY_F2, CONS_KEY_F14, 0, 0, 0, 0, 0 },
{ CONS_KEY_F12, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_F10, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_F8, CONS_KEY_F20, 0, 0, 0, 0, 0 },
{ CONS_KEY_F6, CONS_KEY_F18, 0, 0, 0, 0, 0 },
{ CONS_KEY_F4, CONS_KEY_F16, 0, 0, 0, 0, 0 },
{ "\t", "\t", "\t", "\e\t", "\e\t", "\e\t", "\t" },
{ "`", "~", 0, "\e`", "\e~", 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "q", "Q", "\x11", "\eq", "\eQ", "\e\x11", "q" },
{ "1", "!", 0, "\e1", "\e!", 0, "1" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "z", "Z", "\x1a", "\ez", "\eZ", "\e\x1a", "z" },
{ "s", "S", "\x13", "\es", "\eS", "\e\x13", "s" },
{ "a", "A", "\x01", "\ea", "\eA", "\e\x01", "a" },
{ "w", "W", "\x17", "\ew", "\eW", "\e\x17", "w" },
{ "2", "@", "", "\e2", "\e@", 0, "2" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "c", "C", "\x03", "\ec", "\eC", "\e\x03", "\xc2\xa2" },
{ "x", "X", "\x18", "\ex", "\eX", "\e\x18", "x" },
{ "d", "D", "\x04", "\ed", "\eD", "\e\x04", "d" },
{ "e", "E", "\x05", "\ee", "\eE", "\e\x05","\xe2\x82\xac" },
{ "4", "$", "\x1c", "\e4", "\e$", "\e\x1c", "4" },
{ "3", "#", "\e", "\e3", "\e#", 0, "3" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ " ", " ", "", "\e ", "\e ", 0, " " },
{ "v", "V", "\x16", "\ev", "\eV", "\e\x16", "v" },
{ "f", "F", "\x06", "\ef", "\eF", "\e\x06", "f" },
{ "t", "T", "\x14", "\et", "\eT", "\e\x14", "t" },
{ "r", "R", "\x12", "\er", "\eR", "\e\x12", "r" },
{ "5", "%", "\x1d", "\e5", "\e%", 0, "5" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "n", "N", "\x0e", "\en", "\eN", "\e\x0e", "n" },
{ "b", "B", "\x02", "\eb", "\eB", "\e\x02", "b" },
{ "h", "H", "\x08", "\eh", "\eH", "\e\x08", "h" },
{ "g", "G", "\x07", "\eg", "\eG", "\e\x07", "g" },
{ "y", "Y", "\x19", "\ey", "\eY", "\e\x19", "y" },
{ "6", "^", "\x1e", "\e6", "\e^", 0, "6" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "m", "M", "\x0d", "\em", "\eM", "\e\x0d", "m" },
{ "j", "J", "\x0a", "\ej", "\eJ", "\e\x0a", "j" },
{ "u", "U", "\x15", "\eu", "\eU", "\e\x15", "u" },
{ "7", "&", "\x1f", "\e7", "\e&", "\e\x1f", "7" },
{ "8", "*", "\x7f", "\e8", "\e*", 0, "8" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ ",", "<", 0, "\e,", "\e<", 0, 0 },
{ "k", "K", "\x0b", "\ek", "\eK", "\e\x0b", "k" },
{ "i", "I", "\x09", "\ei", "\eI", "\e\x09", "i" },
{ "o", "O", "\x0f", "\eo", "\eO", "\e\x0f", "o" },
{ "0", ")", 0, "\e0", "\e)", 0, "0" },
{ "9", "(", 0, "\e9", "\e(", 0, "9" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ ".", ">", 0, "\e.", "\e>", 0, 0 },
{ "/", "?", "\x7f", "\e/", "\e?", 0, 0 },
{ "l", "L", "\x0c", "\el", "\eL", "\e\x0c", "l" },
{ ";", ":", 0, "\e;", "\e:", 0, 0 },
{ "p", "P", "\x10", "\ep", "\eP", "\e\x10", "p" },
{ "-", "_", "\x1f", "\e-", "\e_", "\e\x1f", "-" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "'", "\"", "\x07", "\e'", "\e\"", 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "[", "{", "\e", "\e[", "\e{", 0, 0 },
{ "=", "+", 0, "\e=", "\e+", 0, "=" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{"\x0d","\x0d", "\x0d","\e\x0d","\e\x0d","\e\x0d","\x0d" },
{ "]", "}", "\x1d", "\e]", "\e}", "\e\x1d", "~" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "\\",  "|",  "\x1c", "\e\\", "\e|", 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_BACKSPACE, CONS_KEY_BACKSPACE, CONS_KEY_BACKSPACE,
"\e" CONS_KEY_BACKSPACE, "\e" CONS_KEY_BACKSPACE,
"\e" CONS_KEY_BACKSPACE, CONS_KEY_BACKSPACE },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_END, CONS_KEY_END, CONS_KEY_END, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_LEFT, CONS_KEY_LEFT, CONS_KEY_LEFT, 0, 0, 0, 0 },
{ CONS_KEY_HOME, CONS_KEY_HOME, CONS_KEY_HOME, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_IC, CONS_KEY_IC, CONS_KEY_IC, 0, 0, 0, 0 },
{ CONS_KEY_DC, CONS_KEY_DC, CONS_KEY_DC, 0, 0, 0, 0 },
{ CONS_KEY_DOWN, CONS_KEY_DOWN, CONS_KEY_DOWN, 0, 0, 0, 0 },
{ "\e[G", "\e[G", "\e[G", 0, 0, 0, 0 },
{ CONS_KEY_RIGHT, CONS_KEY_RIGHT, CONS_KEY_RIGHT,0, 0, 0, 0 },
{ CONS_KEY_UP, CONS_KEY_UP, CONS_KEY_UP, 0, 0, 0, 0 },
{ "\e", "\e", "\e", "\e\e", "\e\e", "\e\e", "\e" },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_F11, 0, 0, 0, 0, 0, 0 },
{ "+", "+", "+", "+", "+", "+", "+" },
{ CONS_KEY_NPAGE, CONS_KEY_NPAGE, CONS_KEY_NPAGE,0, 0, 0, 0 },
{ "-", "-", "-", "-", "-", "-", "-" },
{ "*", "*", "*", "*", "*", "*", "*" },
{ CONS_KEY_PPAGE, CONS_KEY_PPAGE, CONS_KEY_PPAGE,0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_F7, CONS_KEY_F19, 0, 0, 0, 0, 0 }
};
char *sc_x1_to_kc[][7] =
{
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ "/", "/", "/", "/", "/", "/", 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ "\n", "\n", "\n", "\n", "\n", "\n", 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_END, CONS_KEY_END, CONS_KEY_END, CONS_KEY_END,
CONS_KEY_END, CONS_KEY_END, CONS_KEY_END },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_LEFT, CONS_KEY_LEFT, CONS_KEY_LEFT, CONS_KEY_LEFT,
CONS_KEY_LEFT, CONS_KEY_LEFT, CONS_KEY_LEFT },
{ CONS_KEY_HOME, CONS_KEY_HOME, CONS_KEY_HOME, CONS_KEY_HOME,
CONS_KEY_HOME, CONS_KEY_HOME, CONS_KEY_HOME },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_IC, CONS_KEY_IC, CONS_KEY_IC, CONS_KEY_IC,
CONS_KEY_IC, CONS_KEY_IC, CONS_KEY_IC },
{ CONS_KEY_DC, CONS_KEY_DC, CONS_KEY_DC, CONS_KEY_DC,
CONS_KEY_DC, CONS_KEY_DC, CONS_KEY_DC },
{ CONS_KEY_DOWN, CONS_KEY_DOWN, CONS_KEY_DOWN, CONS_KEY_DOWN,
CONS_KEY_DOWN, CONS_KEY_DOWN, CONS_KEY_DOWN },
{ 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_RIGHT, CONS_KEY_RIGHT, CONS_KEY_RIGHT, CONS_KEY_RIGHT,
CONS_KEY_RIGHT, CONS_KEY_RIGHT, CONS_KEY_RIGHT },
{ CONS_KEY_UP, CONS_KEY_UP, CONS_KEY_UP, CONS_KEY_UP,
CONS_KEY_UP, CONS_KEY_UP, CONS_KEY_UP },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_NPAGE, CONS_KEY_NPAGE, CONS_KEY_NPAGE, CONS_KEY_NPAGE,
CONS_KEY_NPAGE, CONS_KEY_NPAGE, CONS_KEY_NPAGE },
{ 0, 0, 0, 0, 0, 0, 0 }, { 0, 0, 0, 0, 0, 0, 0 },
{ CONS_KEY_PPAGE, CONS_KEY_PPAGE, CONS_KEY_PPAGE, CONS_KEY_PPAGE,
CONS_KEY_PPAGE, CONS_KEY_PPAGE, CONS_KEY_PPAGE }
};
char *sc_x2_to_kc[][7] =
{
{ "\e[P", "\e[P", "\e[P", "\e[P", "\e[P", "\e[P","\e[P" },
};
enum scancode sc_set1_to_set2[] =
{
0x00,
SC_ESC,
SC_1,
SC_2,
SC_3,
SC_4,
SC_5,
SC_6,
SC_7,
SC_8,
SC_9,
SC_0,
SC_MINUS,
SC_EQUAL,
SC_BACKSPACE,
SC_TAB,
SC_Q,
SC_W,
SC_E,
SC_R,
SC_T,
SC_Y,
SC_U,
SC_I,
SC_O,
SC_P,
SC_LEFT_BRACKET,
SC_RIGHT_BRACKET,
SC_ENTER,
SC_LEFT_CTRL,
SC_A,
SC_S,
SC_D,
SC_F,
SC_G,
SC_H,
SC_J,
SC_K,
SC_L,
SC_SEMICOLON,
SC_APOSTROPHE,
SC_BACKQUOTE,
SC_LEFT_SHIFT,
SC_BACKSLASH,
SC_Z,
SC_X,
SC_C,
SC_V,
SC_B,
SC_N,
SC_M,
SC_COMMA,
SC_PERIOD,
SC_SLASH,
SC_RIGHT_SHIFT,
SC_PAD_ASTERISK,
SC_LEFT_ALT,
SC_SPACE,
SC_CAPSLOCK,
SC_F1,
SC_F2,
SC_F3,
SC_F4,
SC_F5,
SC_F6,
SC_F7,
SC_F8,
SC_F9,
SC_F10,
SC_NUMLOCK,
SC_SCROLLLOCK,
SC_PAD_7,
SC_PAD_8,
SC_PAD_9,
SC_PAD_MINUS,
SC_PAD_4,
SC_PAD_5,
SC_PAD_6,
SC_PAD_PLUS,
SC_PAD_1,
SC_PAD_2,
SC_PAD_3,
SC_PAD_0,
SC_PAD_DECIMAL,
0x00,
0x00,
0x00,
SC_F11,
SC_F12,
};
enum scancode sc_set1_to_set2_x1[] =
{
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00,
SC_X1_PAD_ENTER,
SC_X1_RIGHT_CTRL,
0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00,
SC_X1_PAD_SLASH,
0x00,
SC_X1_PRTSC,
SC_X1_RIGHT_ALT,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00,
SC_X1_HOME,
SC_X1_UP,
SC_X1_PGUP,
0x00,
SC_X1_LEFT,
0x00,
SC_X1_RIGHT,
0x00,
SC_X1_END,
SC_X1_DOWN,
SC_X1_PGDN,
SC_X1_INS,
SC_X1_DEL
};
static enum scancode
gnumach_v1_input_next (void)
{
kd_event data_buf;
#ifndef XKB_SUPPORT
int up;
enum scancode sc;
#endif
do
{
mach_msg_type_number_t data_cnt = sizeof (data_buf);
error_t err = device_read_inband (kbd_dev, 0, -1, sizeof (kd_event),
(void *) &data_buf, &data_cnt);
if (err)
return 0;
if (kbd_repeater_opened && data_buf.type == KEYBD_EVENT)
{
kbd_repeat_key (&data_buf);
data_buf.type = 0;
continue;
}
}
while (data_buf.type != KEYBD_EVENT);
#ifdef XKB_SUPPORT
return data_buf.value.sc;
#else
if (data_buf.value.sc == SC_EXTENDED1
|| data_buf.value.sc == SC_EXTENDED2
|| data_buf.value.sc == SC_ERROR)
return data_buf.value.sc;
#define SC_SET1_FLAG_UP 0x80
up = data_buf.value.sc & SC_SET1_FLAG_UP;
sc = sc_set1_to_set2[data_buf.value.sc &~ SC_SET1_FLAG_UP];
return sc | (up ? SC_FLAG_UP : 0);
#endif
}
static void
update_leds (void)
{
error_t err;
if (gnumach_v1_compat)
{
int led = (led_state.scroll_lock ? 1 : 0)
| (led_state.num_lock ? 2 : 0)
| (led_state.caps_lock ? 4 : 0);
err = device_set_status (kbd_dev, KDSETLEDS, &led, 1);
}
else
{
char leds[2];
int data_cnt = 2;
leds[0] = '\xed';
leds[1] = (led_state.scroll_lock ? 1 : 0)
| (led_state.num_lock ? 2 : 0)
| (led_state.caps_lock ? 4 : 0);
err = device_write_inband (kbd_dev, 0, -1, (void *) leds, 2, &data_cnt);
if (!err && data_cnt == 1)
err = device_write_inband (kbd_dev, 0, -1, (void *) &leds[1], 1,
&data_cnt);
}
}
static enum scancode
input_next (void)
{
enum scancode sc = 0;
unsigned char next;
if (gnumach_v1_compat)
return gnumach_v1_input_next ();
do
{
mach_msg_type_number_t data_cnt = 1;
error_t err = device_read_inband (kbd_dev, 0, -1, 1,
(void *) &next, &data_cnt);
if (err)
return 0;
if (next == 0xF0)
sc |= SC_FLAG_UP;
}
while (next == 0xF0);
sc |= next;
return sc;
}
#ifdef XKB_SUPPORT
keycode_t
read_keycode (void)
{
scancode_t sc = input_next ();
if (sc == SC_EXTENDED1)
{
sc = input_next ();
int release = sc & 0x80;
sc &= ~0x80;
switch (sc)
{
case 0x1D:
sc = 101;
break;
case 0x38:
sc = 105;
break;
case 0x5B:
sc = 107;
break;
case 0x5C:
sc = 108;
break;
case 0x5D:
sc = 109;
break;
case 0x52:
sc = 98;
break;
case 0x47:
sc = 89;
break;
case 0x49:
sc = 91;
break;
case 0x53:
sc = 99;
break;
case 0x4F:
sc = 95;
break;
case 0x51:
sc = 97;
break;
case 0x48:
sc = 90;
break;
case 0x50:
sc = 96;
break;
case 0x4b:
sc = 92;
break;
case 0x4d:
sc = 94;
break;
case 0x35:
sc = 104;
break;
case 0x1C:
sc = 100;
break;
default:
sc += 0x78;
}
sc |= release;
}
return sc;
}
#endif
static void *
input_loop (void *unused)
{
pthread_setname_np (pthread_self (), "kbd_input");
#ifdef XKB_SUPPORT
if (gnumach_v1_compat)
{
keycode_t prevkey = 0;
while (1)
{
keypress_t key;
keycode_t raw_keycode = read_keycode () + get_min_keycode();
key.keycode = raw_keycode & ~0x80;
key.rel = raw_keycode & 0x80;
if (raw_keycode == prevkey)
continue;
process_input(key);
prevkey = raw_keycode;
}
return 0;
}
#endif
while (1)
{
enum scancode fsc = input_next ();
enum scancode sc = fsc & ~SC_FLAG_UP;
int down = !(fsc & SC_FLAG_UP);
char buf[100];
size_t size = 0;
int modifier = -1;
static struct {
wchar_t direct;
unsigned int extended : 2;
unsigned int left_shift : 1;
unsigned int right_shift : 1;
unsigned int caps_lock : 1;
unsigned int caps_lock_pressed : 1;
unsigned int left_ctrl : 1;
unsigned int right_ctrl : 1;
unsigned int left_alt : 1;
unsigned int right_alt : 1;
unsigned int num_lock : 1;
unsigned int num_lock_pressed : 1;
} state;
if (!state.left_alt && !state.right_alt)
{
if (state.left_ctrl || state.right_ctrl)
modifier = 2;
else if (state.left_shift || state.right_shift)
modifier = 1;
else
modifier = 0;
}
else if (state.left_alt)
{
if (state.left_ctrl || state.right_ctrl)
modifier = 5;
if (state.left_shift || state.right_shift)
modifier = 4;
else
modifier = 3;
}
else if (state.right_alt)
{
if (!state.left_ctrl && !state.right_ctrl
&& !state.left_shift && !state.right_shift)
modifier = 6;
}
if (!state.extended)
{
if (fsc == SC_EXTENDED1)
state.extended = 1;
else if (fsc == SC_EXTENDED2)
state.extended = 2;
else if (sc == SC_LEFT_SHIFT)
state.left_shift = down;
else if (sc == SC_RIGHT_SHIFT)
state.right_shift = down;
else if (sc == SC_CAPSLOCK)
{
if (down && !state.caps_lock_pressed)
{
state.caps_lock = !state.caps_lock;
state.caps_lock_pressed = 1;
led_state.caps_lock = state.caps_lock;
update_leds ();
}
else if (!down)
state.caps_lock_pressed = 0;
}
else if (sc == SC_LEFT_CTRL)
state.left_ctrl = down;
else if (sc == SC_LEFT_ALT)
state.left_alt = down;
else if (state.left_alt && down && IS_FUNC_KEY (sc))
{
int vc = 0;
switch (sc)
{
case SC_F1:
vc = 1;
break;
case SC_F2:
vc = 2;
break;
case SC_F3:
vc = 3;
break;
case SC_F4:
vc = 4;
break;
case SC_F5:
vc = 5;
break;
case SC_F6:
vc = 6;
break;
case SC_F7:
vc = 7;
break;
case SC_F8:
vc = 8;
break;
case SC_F9:
vc = 9;
break;
case SC_F10:
vc = 10;
break;
case SC_F11:
vc = 11;
break;
case SC_F12:
vc = 12;
break;
default:
vc = 0;
}
if (vc)
console_switch (vc, 0);
}
else if (state.left_alt && state.left_ctrl && down && sc == SC_BACKSPACE)
console_exit ();
else if (state.right_alt && down && sc == SC_PAD_0)
state.direct = (state.direct << 4) | 0x0;
else if (state.right_alt && down && sc == SC_PAD_1)
state.direct = (state.direct << 4) | 0x1;
else if (state.right_alt && down && sc == SC_PAD_2)
state.direct = (state.direct << 4) | 0x2;
else if (state.right_alt && down && sc == SC_PAD_3)
state.direct = (state.direct << 4) | 0x3;
else if (state.right_alt && down && sc == SC_PAD_4)
state.direct = (state.direct << 4) | 0x4;
else if (state.right_alt && down && sc == SC_PAD_5)
state.direct = (state.direct << 4) | 0x5;
else if (state.right_alt && down && sc == SC_PAD_6)
state.direct = (state.direct << 4) | 0x6;
else if (state.right_alt && down && sc == SC_PAD_7)
state.direct = (state.direct << 4) | 0x7;
else if (state.right_alt && down && sc == SC_PAD_8)
state.direct = (state.direct << 4) | 0x8;
else if (state.right_alt && down && sc == SC_PAD_9)
state.direct = (state.direct << 4) | 0x9;
else if (state.right_alt && down && sc == SC_NUMLOCK)
state.direct = (state.direct << 4) | 0xa;
else if (state.right_alt && down && sc == SC_PAD_ASTERISK)
state.direct = (state.direct << 4) | 0xc;
else if (state.right_alt && down && sc == SC_PAD_MINUS)
state.direct = (state.direct << 4) | 0xd;
else if (state.right_alt && down && sc == SC_PAD_PLUS)
state.direct = (state.direct << 4) | 0xe;
else if (sc == SC_NUMLOCK)
{
if (down && !state.num_lock_pressed)
{
state.num_lock = !state.num_lock;
state.num_lock_pressed = 1;
led_state.num_lock = state.num_lock;
update_leds ();
}
else if (!down)
state.num_lock_pressed = 0;
}
else if (down && sc < sizeof (sc_to_kc)/sizeof (sc_to_kc[0]))
{
#if QUAERENDO_INVENIETIS
if (state.left_alt && state.right_alt
&& sc_to_kc[sc][0][0] >= '0' && sc_to_kc[sc][0][0] <= '9'
&& sc_to_kc[sc][0][1] == '\0')
console_deprecated (sc_to_kc[sc][0][0] - '0');
else
#endif
{
if (modifier == 0 && state.caps_lock
&& sc_to_kc[sc][modifier]
&& sc_to_kc[sc][modifier][0] >= 'a'
&& sc_to_kc[sc][modifier][0] <= 'z'
&& sc_to_kc[sc][modifier][1] == '\0')
modifier = 1;
else if (state.num_lock && sc == SC_PAD_0)
{
modifier = 0;
sc = SC_0;
}
else if (state.num_lock && sc == SC_PAD_1)
{
modifier = 0;
sc = SC_1;
}
else if (state.num_lock && sc == SC_PAD_2)
{
modifier = 0;
sc = SC_2;
}
else if (state.num_lock && sc == SC_PAD_3)
{
modifier = 0;
sc = SC_3;
}
else if (state.num_lock && sc == SC_PAD_4)
{
modifier = 0;
sc = SC_4;
}
else if (state.num_lock && sc == SC_PAD_5)
{
modifier = 0;
sc = SC_5;
}
else if (state.num_lock && sc == SC_PAD_6)
{
modifier = 0;
sc = SC_6;
}
else if (state.num_lock && sc == SC_PAD_7)
{
modifier = 0;
sc = SC_7;
}
else if (state.num_lock && sc == SC_PAD_8)
{
modifier = 0;
sc = SC_8;
}
else if (state.num_lock && sc == SC_PAD_9)
{
modifier = 0;
sc = SC_9;
}
else if (state.num_lock && sc == SC_PAD_DECIMAL)
{
modifier = 0;
sc = SC_PERIOD;
}
if (modifier >= 0 && sc_to_kc[sc][modifier])
{
if (!sc_to_kc[sc][modifier][0])
{
assert_backtrace (size < 100);
buf[size++] = '\0';
}
else
{
assert_backtrace (size
< 101 - strlen(sc_to_kc[sc][modifier]));
strcpy (&buf[size], sc_to_kc[sc][modifier]);
size += strlen (sc_to_kc[sc][modifier]);
}
}
}
}
}
else if (state.extended == 1)
{
const enum scancode_x1 scx1 = (enum scancode_x1) sc;
state.extended = 0;
if (scx1 == SC_X1_RIGHT_CTRL)
state.right_ctrl = down;
else if (scx1 == SC_X1_RIGHT_ALT)
{
state.right_alt = down;
if (down)
state.direct = (wchar_t) 0;
else
{
if (state.direct != (wchar_t) 0)
{
char *buffer = &buf[size];
size_t left = sizeof (buf) - size;
char *inbuf = (char *) &state.direct;
size_t inbufsize = sizeof (wchar_t);
size_t nr;
nr = iconv (cd, &inbuf, &inbufsize, &buffer, &left);
if (nr == (size_t) -1)
{
if (errno == E2BIG)
console_error (L"Input buffer overflow");
else if (errno == EILSEQ)
console_error
(L"Input contained invalid byte sequence");
else if (errno == EINVAL)
console_error
(L"Input contained incomplete byte sequence");
else
console_error
(L"Input caused unexpected error");
}
size = sizeof (buf) - left;
}
}
}
else if (state.right_alt && down && scx1 == SC_X1_PAD_SLASH)
state.direct = (state.direct << 4) | 0xb;
else if (state.right_alt && down && scx1 == SC_X1_PAD_ENTER)
state.direct = (state.direct << 4) | 0xf;
else if (state.left_alt && down && scx1 == SC_X1_RIGHT)
console_switch (0, 1);
else if (state.left_alt && down && scx1 == SC_X1_LEFT)
console_switch (0, -1);
else if (state.left_alt && down && scx1 == SC_X1_UP)
console_scrollback (CONS_SCROLL_DELTA_LINES, 1);
else if (state.left_alt && down && scx1 == SC_X1_DOWN)
console_scrollback (CONS_SCROLL_DELTA_LINES, -1);
else if ((state.right_shift || state.left_shift)
&& down && scx1 == SC_X1_PGUP)
console_scrollback (CONS_SCROLL_DELTA_SCREENS, 0.5);
else if ((state.right_shift || state.left_shift)
&& down && scx1 == SC_X1_PGDN)
console_scrollback (CONS_SCROLL_DELTA_SCREENS, -0.5);
else if (down && sc < sizeof (sc_x1_to_kc)/sizeof (sc_x1_to_kc[0]))
{
if (modifier >= 0 && sc_x1_to_kc[sc][modifier])
{
assert_backtrace (size < 101 - strlen(sc_x1_to_kc[sc][modifier]));
strcpy (&buf[size], sc_x1_to_kc[sc][modifier]);
size += strlen (sc_x1_to_kc[sc][modifier]);
}
}
}
else if (state.extended == 2)
state.extended = 3;
else if (state.extended == 3)
state.extended = 0;
if (size)
console_input (buf, size);
}
return 0;
}
static const char doc[] = "PC Keyboard Driver";
struct arguments
{
int pos;
#ifdef XKB_SUPPORT
char *model;
char *layout;
char *variant;
char *options;
char *composefile;
int ctrlaltbs;
int repeat_delay;
int repeat_interval;
#endif
};
static const struct argp_option options[] =
{
#ifdef XKB_SUPPORT
#define REPEAT_DELAY_ID 25425
#define REPEAT_INTERVAL_ID 5322
{"model", 'm', "XKB_MODEL", 0,
"the keyboard model for xkb" },
{"layout", 'l', "XKB_LAYOUT", 0,
"The layout of the keyboard" },
{"variant", 'v', "XKB_VARIANT" , 0,
"The variant to use"},
{"options", 'p', "XKB_OPTIONS" , 0,
"The xkb options"},
{"compose", 'o', "COMPOSEFILE", 0,
"Compose file to load (default none)"},
{"ctrlaltbs", 'c', 0 , 0,
"CTRL + Alt + Backspace will exit the console client (default)."},
{"no-ctrlaltbs", 'n', 0 , 0,
"CTRL + Alt + Backspace will not exit the console client."},
{"repeat-delay", REPEAT_DELAY_ID, "DELAY", 0,
"Delay before pressed key starts repeating (measured in jiffies)"},
{"repeat-interval", REPEAT_INTERVAL_ID, "INTERVAL", 0,
"Time elapsed between repeated keys (measured in jiffies)"},
#endif
{"repeat", 'r', "NODE", OPTION_ARG_OPTIONAL,
"Set a repeater translator on NODE (default: " DEFAULT_REPEATER_NODE ")"},
{ 0 }
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
struct arguments *arguments = state->input;
switch (key)
{
#ifdef XKB_SUPPORT
case 'm':
arguments->model = arg;
break;
case 'l':
arguments->layout = arg;
break;
case 'v':
arguments->variant = arg;
break;
case 'p':
arguments->options = arg;
break;
case 'o':
arguments->composefile = arg;
break;
case 'c':
arguments->ctrlaltbs = 1;
break;
case 'n':
arguments->ctrlaltbs = 0;
break;
case REPEAT_DELAY_ID:
arguments->repeat_delay = atoi(arg);
break;
case REPEAT_INTERVAL_ID:
arguments->repeat_interval = atoi(arg);
break;
#endif
case 'r':
repeater_node = arg ? arg: DEFAULT_REPEATER_NODE;
break;
case ARGP_KEY_END:
break;
default:
return ARGP_ERR_UNKNOWN;
}
arguments->pos = state->next;
return 0;
}
static struct argp argp = {options, parse_opt, 0, doc};
static error_t
pc_kbd_init (void **handle, int no_exit, int argc, char *argv[], int *next)
{
error_t err;
struct arguments arguments =
{
pos: 1
#ifdef XKB_SUPPORT
, model: 0
, layout: 0
, variant: 0
, options: 0
, composefile: 0
, ctrlaltbs: 1
, repeat_delay: -1
, repeat_interval: -1
#endif
};
err = argp_parse (&argp, argc, argv, ARGP_IN_ORDER | ARGP_NO_EXIT
| ARGP_SILENT, 0 , &arguments);
*next += arguments.pos - 1;
if (err && err != EINVAL)
return err;
#ifdef XKB_SUPPORT
if (arguments.repeat_delay <= 0)
{
arguments.repeat_delay = 50;
}
if (arguments.repeat_interval <= 0)
{
arguments.repeat_interval = 10;
}
ctrlaltbs = arguments.ctrlaltbs;
xkb_repeat_delay = arguments.repeat_delay;
xkb_repeat_interval = arguments.repeat_interval;
err = xkb_context_init ("base", arguments.model, arguments.layout, arguments.variant, arguments.options, arguments.composefile);
if (err)
return err;
#endif
return 0;
}
static error_t
pc_kbd_start (void *handle)
{
error_t err;
pthread_t thread;
device_t device_master;
cd = iconv_open ("UTF-8", "WCHAR_T");
if (cd == (iconv_t) -1)
return errno;
err = get_privileged_ports (0, &device_master);
if (err)
{
iconv_close (cd);
return err;
}
err = device_open (device_master, D_READ | D_WRITE, "@>=kbd", &kbd_dev);
if (err == D_NO_SUCH_DEVICE)
{
gnumach_v1_compat = 1;
err = device_open (device_master, D_READ, "kbd", &kbd_dev);
}
mach_port_deallocate (mach_task_self (), device_master);
if (err)
{
iconv_close (cd);
return err;
}
if (gnumach_v1_compat)
{
int data = KB_EVENT;
err = device_set_status (kbd_dev, KDSKBDMODE, &data, 1);
if (err)
{
device_close (kbd_dev);
mach_port_deallocate (mach_task_self (), kbd_dev);
iconv_close (cd);
return err;
}
#ifdef XKB_SUPPORT
xkb_init_repeat (xkb_repeat_delay, xkb_repeat_interval);
#endif
}
update_leds ();
err = driver_add_input (&pc_kbd_ops, NULL);
if (err)
{
if (gnumach_v1_compat)
{
int data = KB_ASCII;
device_set_status (kbd_dev, KDSKBDMODE, &data, 1);
}
device_close (kbd_dev);
mach_port_deallocate (mach_task_self (), kbd_dev);
iconv_close (cd);
return err;
}
if (repeater_node)
kbd_setrepeater (repeater_node, &cnode);
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
pc_kbd_fini (void *handle, int force)
{
driver_remove_input (&pc_kbd_ops, NULL);
if (gnumach_v1_compat)
{
int data = KB_ASCII;
device_set_status (kbd_dev, KDSKBDMODE, &data, 1);
}
device_close (kbd_dev);
mach_port_deallocate (mach_task_self (), kbd_dev);
iconv_close (cd);
console_unregister_consnode (cnode);
console_destroy_consnode (cnode);
#ifdef XKB_SUPPORT
xkb_context_cleanup ();
#endif
return 0;
}
static error_t
pc_kbd_set_scroll_lock_status (void *handle, int onoff)
{
led_state.scroll_lock = onoff;
update_leds ();
return 0;
}
struct driver_ops driver_pc_kbd_ops =
{
pc_kbd_init,
pc_kbd_start,
pc_kbd_fini
};
static struct input_ops pc_kbd_ops =
{
pc_kbd_set_scroll_lock_status,
NULL
};