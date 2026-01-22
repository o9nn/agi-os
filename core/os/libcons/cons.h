#ifndef _HURD_CONS_H
#define _HURD_CONS_H
#include <dirent.h>
#include <hurd/ports.h>
#include <mach.h>
#include <hurd/console.h>
typedef struct cons *cons_t;
typedef struct vcons_list *vcons_list_t;
typedef struct vcons *vcons_t;
typedef struct cons_notify *cons_notify_t;
struct vcons_list
{
cons_t cons;
vcons_list_t next;
vcons_list_t prev;
int id;
vcons_t vcons;
};
struct cons_notify
{
struct port_info pi;
cons_t cons;
};
struct vcons
{
struct cons_notify notify;
cons_t cons;
vcons_list_t vcons_entry;
int id;
pthread_mutex_t lock;
int input;
struct cons_display *display;
size_t display_size;
struct
{
uint32_t flags;
struct
{
uint32_t col;
uint32_t row;
uint32_t status;
} cursor;
struct
{
uint32_t width;
uint32_t height;
uint32_t lines;
uint32_t cur_line;
uint32_t scr_lines;
conchar_t *matrix;
} screen;
struct
{
uint32_t audible;
uint32_t visible;
} bell;
struct
{
uint32_t written;
uint32_t length;
cons_change_t *buffer;
} changes;
} state;
uint32_t scrolling;
};
struct cons
{
pthread_mutex_t lock;
vcons_list_t vcons_list;
vcons_list_t vcons_last;
struct port_class *port_class;
struct port_bucket *port_bucket;
DIR *dir;
io_t dirport;
int slack;
};
enum mouse_movement
{
CONS_VCONS_MOUSE_MOVE_REL,
CONS_VCONS_MOUSE_MOVE_ABS,
CONS_VCONS_MOUSE_MOVE_ABS_PERCENT
};
enum mouse_button
{
CONS_VCONS_MOUSE_BUTTON_NO_OP,
CONS_VCONS_MOUSE_BUTTON_PRESSED,
CONS_VCONS_MOUSE_BUTTON_RELEASED
};
typedef struct mouse_event
{
enum mouse_movement mouse_movement;
float x;
float y;
enum mouse_button mouse_button;
int button;
} *mouse_event_t;
extern const char *cons_client_name;
extern const char *cons_client_version;
extern char *cons_extra_version;
void cons_vcons_clear (vcons_t vcons, size_t length,
uint32_t col, uint32_t row);
void cons_vcons_write (vcons_t vcons, conchar_t *str, size_t length,
uint32_t col, uint32_t row);
void cons_vcons_set_cursor_pos (vcons_t vcons, uint32_t col, uint32_t row);
void cons_vcons_set_cursor_status (vcons_t vcons, uint32_t status);
void cons_vcons_scroll (vcons_t vcons, int delta);
void cons_vcons_update (vcons_t vcons);
void cons_vcons_beep (vcons_t vcons);
void cons_vcons_flash (vcons_t vcons);
void cons_vcons_set_scroll_lock (vcons_t vcons, int onoff);
error_t cons_vcons_activate (vcons_t vcons);
void cons_vcons_add (cons_t cons, vcons_list_t vcons_entry);
void cons_vcons_remove (cons_t cons, vcons_list_t vcons_entry);
error_t cons_switch (vcons_t vcons, int id, int delta, vcons_t *r_vcons);
error_t cons_vcons_input (vcons_t vcons, char *buf, size_t size);
error_t cons_vcons_set_dimension (vcons_t vcons,
uint32_t col, uint32_t row);
typedef enum
{
CONS_SCROLL_DELTA_LINES, CONS_SCROLL_DELTA_SCREENS,
CONS_SCROLL_ABSOLUTE_LINE, CONS_SCROLL_ABSOLUTE_PERCENTAGE
} cons_scroll_t;
int cons_vcons_scrollback (vcons_t vcons, cons_scroll_t type, float value);
error_t cons_vcons_set_mousecursor_pos (vcons_t vcons, float x, float y);
error_t cons_vcons_set_mousecursor_status (vcons_t vcons, int status);
extern const struct argp cons_startup_argp;
extern struct port_bucket *cons_port_bucket;
extern struct port_class *cons_port_class;
extern char *cons_file;
error_t cons_init (void);
void cons_server_loop (void);
int cons_demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp);
error_t cons_lookup (cons_t cons, int id, int create, vcons_list_t *r_vcons);
error_t cons_vcons_open (cons_t cons, vcons_list_t vcons_entry,
vcons_t *r_vcons);
void cons_vcons_close (vcons_t vcons);
void cons_vcons_destroy (void *port);
void cons_vcons_refresh (vcons_t vcons);
error_t cons_vcons_move_mouse (vcons_t vcons, mouse_event_t ev);
#endif