#ifndef DISPLAY_H
#define DISPLAY_H
#include <sys/ioctl.h>
struct display;
typedef struct display *display_t;
void display_init (void);
error_t
display_create (display_t *r_display, const char *encoding,
conchar_attr_t def_attr, unsigned int lines,
unsigned int width, unsigned int height);
void display_destroy (display_t display);
off_t display_get_size (display_t display);
void display_getsize (display_t display, struct winsize *winsize);
error_t display_set_owner (display_t display, pid_t pid);
error_t display_get_owner (display_t display, pid_t *pid);
ssize_t display_output (display_t display, int nonblock, const char *data,
size_t datalen);
mach_port_t display_get_filemap (display_t display, vm_prot_t prot);
ssize_t display_read (display_t display, int nonblock, off_t off,
char *data, size_t len);
error_t display_notice_changes (display_t display, mach_port_t notify);
void display_start_output (display_t display);
void display_stop_output (display_t display);
size_t display_pending_output (display_t display);
void display_discard_output (display_t display);
#endif