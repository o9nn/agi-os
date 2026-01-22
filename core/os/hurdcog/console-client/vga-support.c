#include <errno.h>
#include <assert-backtrace.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/io.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include "vga-hw.h"
#include "vga-support.h"
char *vga_videomem;
struct vga_state
{
unsigned char seq_clock_mode;
unsigned char seq_map;
unsigned char seq_font;
unsigned char seq_mode;
unsigned char gfx_map;
unsigned char gfx_mode;
unsigned char gfx_misc;
unsigned char crt_max_scan_line;
unsigned char crt_cursor_start;
unsigned char crt_cursor_end;
unsigned char crt_cursor_high;
unsigned char crt_cursor_low;
unsigned char attr_mode;
char videomem[2 * 80 * 25]
__attribute__ ((aligned (__BIGGEST_ALIGNMENT__)));
unsigned char fontmem[2 * VGA_FONT_SIZE * VGA_FONT_HEIGHT]
__attribute__ ((aligned (__BIGGEST_ALIGNMENT__)));
};
static struct vga_state *vga_state;
error_t
vga_init (void)
{
error_t err;
int fd;
if (ioperm (VGA_MIN_REG, VGA_MAX_REG - VGA_MIN_REG + 1, 1) < 0)
{
if (errno != EMIG_BAD_ID && errno != ENOSYS)
{
free (vga_state);
return errno;
}
}
fd = open ("/dev/mem", O_RDWR);
if (fd >= 0)
{
vga_videomem = mmap (0, VGA_VIDEO_MEM_LENGTH, PROT_READ | PROT_WRITE,
MAP_SHARED, fd, VGA_VIDEO_MEM_BASE_ADDR);
err = errno;
close (fd);
if (vga_videomem == (void *) -1)
return err;
}
else
return errno;
vga_state = malloc (sizeof (*vga_state));
if (!vga_state)
return errno;
outb (VGA_SEQ_CLOCK_MODE_ADDR, VGA_SEQ_ADDR_REG);
vga_state->seq_clock_mode = inb (VGA_SEQ_DATA_REG);
outb (VGA_SEQ_MAP_ADDR, VGA_SEQ_ADDR_REG);
vga_state->seq_map = inb (VGA_SEQ_DATA_REG);
outb (VGA_SEQ_FONT_ADDR, VGA_SEQ_ADDR_REG);
vga_state->seq_font = inb (VGA_SEQ_DATA_REG);
outb (VGA_SEQ_MODE_ADDR, VGA_SEQ_ADDR_REG);
vga_state->seq_mode = inb (VGA_SEQ_DATA_REG);
outb (VGA_GFX_MAP_ADDR, VGA_GFX_ADDR_REG);
vga_state->gfx_map = inb (VGA_GFX_DATA_REG);
outb (VGA_GFX_MODE_ADDR, VGA_GFX_ADDR_REG);
vga_state->gfx_mode = inb (VGA_GFX_DATA_REG);
outb (VGA_GFX_MISC_ADDR, VGA_GFX_ADDR_REG);
vga_state->gfx_misc = inb (VGA_GFX_DATA_REG);
outb (VGA_CRT_MAX_SCAN_LINE, VGA_CRT_ADDR_REG);
vga_state->crt_max_scan_line = inb (VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_START, VGA_CRT_ADDR_REG);
vga_state->crt_cursor_start = inb (VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_END, VGA_CRT_ADDR_REG);
vga_state->crt_cursor_end = inb (VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_HIGH, VGA_CRT_ADDR_REG);
vga_state->crt_cursor_high = inb (VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_LOW, VGA_CRT_ADDR_REG);
vga_state->crt_cursor_low = inb (VGA_CRT_DATA_REG);
inb (VGA_INPUT_STATUS_1_REG);
outb (VGA_ATTR_MODE_ADDR, VGA_ATTR_ADDR_DATA_REG);
vga_state->attr_mode = inb (VGA_ATTR_DATA_READ_REG);
inb (VGA_INPUT_STATUS_1_REG);
outb (VGA_ATTR_ENABLE_ADDR, VGA_ATTR_ADDR_DATA_REG);
outb (0x00, VGA_ATTR_ADDR_DATA_REG);
outb (VGA_GFX_MISC_ADDR, VGA_GFX_ADDR_REG);
outb (VGA_GFX_MISC_CHAINOE | VGA_GFX_MISC_B8TOBF, VGA_GFX_DATA_REG);
outb (VGA_GFX_MODE_ADDR, VGA_GFX_ADDR_REG);
outb (VGA_GFX_MODE_HOSTOE, VGA_GFX_DATA_REG);
vga_memcpy (vga_state->videomem, vga_videomem, 2 * 80 * 25);
vga_read_font_buffer (0, 0, vga_state->fontmem,
2 * VGA_FONT_SIZE * VGA_FONT_HEIGHT);
vga_memset (vga_videomem, 0, 80 * 25 * 2 * 2);
return 0;
}
void
vga_fini (void)
{
vga_write_font_buffer (0, 0, vga_state->fontmem,
2 * VGA_FONT_SIZE * VGA_FONT_HEIGHT);
vga_memcpy (vga_videomem, vga_state->videomem, 2 * 80 * 25);
outb (VGA_SEQ_CLOCK_MODE_ADDR, VGA_SEQ_ADDR_REG);
outb (vga_state->seq_clock_mode, VGA_SEQ_DATA_REG);
outb (VGA_SEQ_MAP_ADDR, VGA_SEQ_ADDR_REG);
outb (vga_state->seq_map, VGA_SEQ_DATA_REG);
outb (VGA_SEQ_FONT_ADDR, VGA_SEQ_ADDR_REG);
outb (vga_state->seq_font, VGA_SEQ_DATA_REG);
outb (VGA_SEQ_MODE_ADDR, VGA_SEQ_ADDR_REG);
outb (vga_state->seq_mode, VGA_SEQ_DATA_REG);
outb (VGA_GFX_MAP_ADDR, VGA_GFX_ADDR_REG);
outb (vga_state->gfx_map, VGA_GFX_DATA_REG);
outb (VGA_GFX_MODE_ADDR, VGA_GFX_ADDR_REG);
outb (vga_state->gfx_mode, VGA_GFX_DATA_REG);
outb (VGA_GFX_MISC_ADDR, VGA_GFX_ADDR_REG);
outb (vga_state->gfx_misc, VGA_GFX_DATA_REG);
outb (VGA_CRT_MAX_SCAN_LINE, VGA_CRT_ADDR_REG);
outb (vga_state->crt_max_scan_line, VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_START, VGA_CRT_ADDR_REG);
outb (vga_state->crt_cursor_start, VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_END, VGA_CRT_ADDR_REG);
outb (vga_state->crt_cursor_end, VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_HIGH, VGA_CRT_ADDR_REG);
outb (vga_state->crt_cursor_high, VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_LOW, VGA_CRT_ADDR_REG);
outb (vga_state->crt_cursor_low, VGA_CRT_DATA_REG);
inb (VGA_INPUT_STATUS_1_REG);
outb (VGA_ATTR_MODE_ADDR, VGA_ATTR_ADDR_DATA_REG);
outb (vga_state->attr_mode, VGA_ATTR_DATA_READ_REG);
inb (VGA_INPUT_STATUS_1_REG);
outb (VGA_ATTR_ENABLE_ADDR, VGA_ATTR_ADDR_DATA_REG);
outb (0x00, VGA_ATTR_ADDR_DATA_REG);
ioperm (VGA_MIN_REG, VGA_MAX_REG - VGA_MIN_REG + 1, 0);
munmap (vga_videomem, VGA_VIDEO_MEM_LENGTH);
}
void vga_memset (void *__restrict _s, int c, size_t n)
{
uint8_t * __restrict s = _s;
size_t i;
for (i = 0; i < n; i++)
s[i] = c;
}
void vga_memcpy (void *__restrict _dest, const void *__restrict _src, size_t n)
{
uint8_t * __restrict dest = _dest;
const uint8_t * __restrict src = _src;
size_t i;
for (i = 0; i < n; i++)
dest[i] = src[i];
}
void vga_memmove (void *_dest, const void *_src, size_t n)
{
uint8_t * dest = _dest;
const uint8_t * src = _src;
size_t i;
if (dest <= src)
for (i = 0; i < n; i++)
dest[i] = src[i];
else
for (i = 0; i < n; i++)
dest[n-1-i] = src[n-1-i];
}
static void
vga_read_write_font_buffer (int write, int buffer, int index,
unsigned char *data, size_t datalen)
{
char saved_seq_map;
char saved_seq_mode;
char saved_gfx_map;
char saved_gfx_mode;
char saved_gfx_misc;
int offset = buffer * VGA_FONT_SIZE + index * VGA_FONT_HEIGHT;
assert_backtrace (offset >= 0 && offset + datalen <= VGA_VIDEO_MEM_LENGTH);
outb (VGA_SEQ_MAP_ADDR, VGA_SEQ_ADDR_REG);
saved_seq_map = inb (VGA_SEQ_DATA_REG);
outb (VGA_SEQ_MAP_PLANE2, VGA_SEQ_DATA_REG);
outb (VGA_SEQ_MODE_ADDR, VGA_SEQ_ADDR_REG);
saved_seq_mode = inb (VGA_SEQ_DATA_REG);
outb (VGA_SEQ_MODE_SEQUENTIAL | VGA_SEQ_MODE_EXT, VGA_SEQ_DATA_REG);
outb (VGA_GFX_MAP_ADDR, VGA_GFX_ADDR_REG);
saved_gfx_map = inb (VGA_GFX_DATA_REG);
outb (VGA_GFX_MAP_PLANE2, VGA_GFX_DATA_REG);
outb (VGA_GFX_MODE_ADDR, VGA_GFX_ADDR_REG);
saved_gfx_mode = inb (VGA_GFX_DATA_REG);
outb (VGA_GFX_MODE_READ0, VGA_GFX_DATA_REG);
outb (VGA_GFX_MISC_ADDR, VGA_GFX_ADDR_REG);
saved_gfx_misc = inb (VGA_GFX_DATA_REG);
outb (VGA_GFX_MISC_B8TOBF, VGA_GFX_DATA_REG);
if (write)
vga_memcpy (vga_videomem + offset, data, datalen);
else
vga_memcpy (data, vga_videomem + offset, datalen);
outb (VGA_SEQ_MAP_ADDR, VGA_SEQ_ADDR_REG);
outb (saved_seq_map, VGA_SEQ_DATA_REG);
outb (VGA_SEQ_MODE_ADDR, VGA_SEQ_ADDR_REG);
outb (saved_seq_mode, VGA_SEQ_DATA_REG);
outb (VGA_GFX_MAP_ADDR, VGA_GFX_ADDR_REG);
outb (saved_gfx_map, VGA_GFX_DATA_REG);
outb (VGA_GFX_MODE_ADDR, VGA_GFX_ADDR_REG);
outb (saved_gfx_mode, VGA_GFX_DATA_REG);
outb (VGA_GFX_MISC_ADDR, VGA_GFX_ADDR_REG);
outb (saved_gfx_misc, VGA_GFX_DATA_REG);
}
void
vga_write_font_buffer (int buffer, int index, unsigned char *data,
size_t datalen)
{
vga_read_write_font_buffer (1, buffer, index, data, datalen);
}
void
vga_read_font_buffer (int buffer, int index, unsigned char *data,
size_t datalen)
{
vga_read_write_font_buffer (0, buffer, index, data, datalen);
}
void
vga_select_font_buffer (int font_buffer, int font_buffer_supp)
{
char font = ((font_buffer & 6) >> 1)  | ((font_buffer & 1) << 4)
| ((font_buffer_supp & 6) << 1) | ((font_buffer_supp & 1) << 5);
outb (VGA_SEQ_FONT_ADDR, VGA_SEQ_ADDR_REG);
outb (font, VGA_SEQ_DATA_REG);
}
void
vga_set_font_height (int height)
{
char saved;
outb (VGA_CRT_MAX_SCAN_LINE, VGA_CRT_ADDR_REG);
saved = inb (VGA_CRT_DATA_REG);
saved &= ~31;
saved |= (height - 1) & 31;
outb (saved, VGA_CRT_DATA_REG);
}
int
vga_get_font_width (void)
{
outb (VGA_SEQ_CLOCK_MODE_ADDR, VGA_SEQ_ADDR_REG);
return (inb (VGA_SEQ_DATA_REG) & VGA_SEQ_CLOCK_MODE_8) ? 8 : 9;
}
void
vga_set_font_width (int width)
{
char saved;
if (width != 8 && width != 9)
return;
outb (VGA_SEQ_CLOCK_MODE_ADDR, VGA_SEQ_ADDR_REG);
saved = inb (VGA_SEQ_DATA_REG);
if (width == 8)
saved |= VGA_SEQ_CLOCK_MODE_8;
else
saved &= ~VGA_SEQ_CLOCK_MODE_8;
outb (saved, VGA_SEQ_DATA_REG);
inb (VGA_INPUT_STATUS_1_REG);
outb (VGA_ATTR_MODE_ADDR, VGA_ATTR_ADDR_DATA_REG);
saved = inb (VGA_ATTR_DATA_READ_REG);
if (width == 8)
saved &= ~VGA_ATTR_MODE_LGE;
else
saved |= VGA_ATTR_MODE_LGE;
outb (saved, VGA_ATTR_ADDR_DATA_REG);
inb (VGA_INPUT_STATUS_1_REG);
outb (VGA_ATTR_ENABLE_ADDR, VGA_ATTR_ADDR_DATA_REG);
outb (0x00, VGA_ATTR_ADDR_DATA_REG);
}
void
vga_display_cursor (int on)
{
char crs_start;
outb (VGA_CRT_CURSOR_START, VGA_CRT_ADDR_REG);
crs_start = inb (VGA_CRT_DATA_REG);
if (on)
crs_start &= ~VGA_CRT_CURSOR_DISABLE;
else
crs_start |= VGA_CRT_CURSOR_DISABLE;
outb (crs_start, VGA_CRT_DATA_REG);
}
void
vga_set_cursor_size (int start, int end)
{
char saved;
if (start >= 0)
{
outb (VGA_CRT_CURSOR_START, VGA_CRT_ADDR_REG);
saved = inb (VGA_CRT_DATA_REG);
saved &= ~31;
saved |= start & 31;
outb (saved, VGA_CRT_DATA_REG);
}
if (end >= 0)
{
outb (VGA_CRT_CURSOR_END, VGA_CRT_ADDR_REG);
saved = inb (VGA_CRT_DATA_REG);
saved &= ~31;
saved |= end & 31;
outb (saved, VGA_CRT_DATA_REG);
}
}
void
vga_set_cursor_pos (unsigned int pos)
{
outb (VGA_CRT_CURSOR_HIGH, VGA_CRT_ADDR_REG);
outb (pos >> 8, VGA_CRT_DATA_REG);
outb (VGA_CRT_CURSOR_LOW, VGA_CRT_ADDR_REG);
outb (pos & 0xff, VGA_CRT_DATA_REG);
}
void
vga_read_palette (unsigned char index, unsigned char *data, int nr)
{
nr *= 3;
outb (index, VGA_DAC_READ_ADDR_REG);
while (nr--)
*(data++) = inb (VGA_DAC_DATA_REG);
}
void
vga_write_palette (unsigned char index, const unsigned char *data, int nr)
{
nr *= 3;
outb (index, VGA_DAC_WRITE_ADDR_REG);
while (nr--)
outb (*(data++), VGA_DAC_DATA_REG);
}
void
vga_exchange_palette_attributes (unsigned char index,
unsigned char *palette_attr,
int nr)
{
if (!nr)
return;
while (nr--)
{
unsigned char attr;
inb (VGA_INPUT_STATUS_1_REG);
outb (index++, VGA_ATTR_ADDR_DATA_REG);
attr = inb (VGA_ATTR_DATA_READ_REG);
outb ((attr & ~077) | (*palette_attr & 077), VGA_ATTR_ADDR_DATA_REG);
*(palette_attr++) = attr & 077;
}
inb (VGA_INPUT_STATUS_1_REG);
outb (VGA_ATTR_ENABLE_ADDR, VGA_ATTR_ADDR_DATA_REG);
outb (0x00, VGA_ATTR_ADDR_DATA_REG);
}