#ifndef _VGA_SUPPORT_H_
#define _VGA_SUPPORT_H_ 1
#include <errno.h>
#include <sys/types.h>
extern char *vga_videomem;
error_t vga_init (void);
void vga_fini (void);
void vga_memset (void *__restrict s, int c, size_t n);
void vga_memcpy (void *__restrict dest, const void *__restrict src, size_t n);
void vga_memmove (void *dest, const void *src, size_t n);
void vga_write_font_buffer (int buffer, int index,
unsigned char *data, size_t datalen);
void vga_read_font_buffer (int buffer, int index,
unsigned char *data, size_t datalen);
void vga_select_font_buffer (int font_buffer, int font_buffer_supp);
void vga_set_font_height (int height);
int vga_get_font_width (void);
void vga_set_font_width (int width);
void vga_display_cursor (int on);
void vga_set_cursor_size (int start, int end);
void vga_set_cursor_pos (unsigned int pos);
void vga_read_palette (unsigned char index, unsigned char *data, int nr);
void vga_write_palette (unsigned char index,
const unsigned char *data, int nr);
void vga_exchange_palette_attributes (unsigned char index,
unsigned char *saved_palette_attr,
int nr);
#endif