#include <errno.h>
#include <argp.h>
#include <xkbcommon/xkbcommon.h>
extern struct xkb_context *ctx;
typedef int keycode_t;
typedef unsigned int scancode_t;
typedef struct keypress
{
keycode_t keycode;
unsigned short rel;
} keypress_t;
error_t xkb_context_init (const char *rules, const char *model, const char *layout, const char *variant, const char* options, const char *composefile);
void xkb_context_cleanup (void);
void process_input (keypress_t key);
void process_keypress_event (keycode_t keycode);
void xkb_timer_notify_input (keypress_t key);
int get_min_keycode (void);
error_t xkb_init_repeat (int delay, int repeat);
int debug_printf (const char *f, ...);