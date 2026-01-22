#ifndef _DISPLAY_H_
#define _DISPLAY_H_ 1
#include <errno.h>
#include <stdint.h>
#include <hurd/console.h>
struct display_ops;
typedef struct display_ops *display_ops_t;
error_t driver_add_display (display_ops_t ops, void *handle);
error_t driver_remove_display (display_ops_t ops, void *handle);
struct display_ops
{
error_t (*set_cursor_pos) (void *handle, uint32_t col, uint32_t row);
error_t (*set_cursor_status) (void *handle, uint32_t state);
error_t (*scroll) (void *handle, int delta);
error_t (*clear) (void *handle, size_t length, uint32_t col, uint32_t row);
error_t (*write) (void *handle, conchar_t *str, size_t length,
uint32_t col, uint32_t row);
error_t (*update) (void *handle);
error_t (*flash) (void *handle);
void (*deprecated) (void *handle, int key);
error_t (*set_dimension) (void *handle, unsigned int width,
unsigned int height);
error_t (*set_mousecursor_pos) (void *handle, float x, float y);
error_t (*set_mousecursor_status) (void *handle, int status);
};
#endif