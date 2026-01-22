#ifndef gdevpcfb_INCLUDED
# define gdevpcfb_INCLUDED
#ifdef __MSDOS__
# include "dos_.h"
typedef union REGS registers;
#endif
#define ega_bits_of_color 2
#define rgb_max ega_bits_of_color
#define no_color ((int)gx_no_color_index)
dev_proc_open_device(ega_open);
dev_proc_close_device(ega_close);
dev_proc_fill_rectangle(ega_fill_rectangle);
dev_proc_tile_rectangle(ega_tile_rectangle);
dev_proc_copy_mono(ega_copy_mono);
dev_proc_copy_color(ega_copy_color);
dev_proc_get_bits(ega_get_bits);
typedef struct pcfb_bios_state_s {
int display_mode;
byte text_page;
uint text_cursor_mode;
uint text_font;
byte text_attribute;
byte border_color;
} pcfb_bios_state;
void pcfb_set_signals(gx_device *);
void pcfb_get_state(pcfb_bios_state *);
void pcfb_set_mode(int);
void pcfb_set_state(const pcfb_bios_state *);
typedef byte *fb_ptr;
typedef volatile byte *volatile_fb_ptr;
#ifdef A4
# define PAGE_HEIGHT_INCHES 11.69
#else
# define PAGE_HEIGHT_INCHES 11.0
#endif
typedef struct gx_device_ega_s gx_device_ega;
struct gx_device_ega_s {
gx_device_common;
int raster;
int fb_seg_mult;
int fb_byte_mult;
#define mk_fb_ptr(x, y)\
(fb_dev->fb_byte_mult == 0 ?\
(fb_ptr)MK_PTR(regen + (y) * (fb_dev->fb_seg_mult), (x) >> 3) :\
(fb_ptr)MK_PTR(regen + ((y) >> 4) * (fb_dev->fb_seg_mult),\
(((y) & 15) * fb_dev->fb_byte_mult) + ((x) >> 3)))
int video_mode;
};
#define ega_device(dev_name, procs, fb_raster, screen_height, aspect_ratio, video_mode)\
{ std_device_dci_body(gx_device_ega, &procs, dev_name,\
fb_raster * 8, screen_height,\
(screen_height * (aspect_ratio)) / PAGE_HEIGHT_INCHES, \
screen_height / PAGE_HEIGHT_INCHES, \
(rgb_max ? 3 : 1), \
4, \
(rgb_max ? rgb_max : 1), \
rgb_max,\
(rgb_max ? rgb_max + 1 : 2), \
(rgb_max ? rgb_max + 1 : 0) \
),\
{ 0 }, \
fb_raster,\
(fb_raster & 15 ? fb_raster : fb_raster >> 4),\
(fb_raster & 15 ? fb_raster : 0),\
video_mode\
}
#define seq_addr 0x3c4
#define s_map 2
#define set_s_map(mask) outport2(seq_addr, s_map, mask)
#define graph_addr 0x3ce
#define g_const 0
#define set_g_const(color) outport2(graph_addr, g_const, color)
#define g_const_map 1
#define set_g_const_map(map) outport2(graph_addr, g_const_map, map)
#define g_function 3
# define gf_WRITE 0
# define gf_AND 8
# define gf_OR 0x10
# define gf_XOR 0x18
#define set_g_function(func) outport2(graph_addr, g_function, func)
#define g_read_plane 4
#define set_g_read_plane(plane) outport2(graph_addr, g_read_plane, plane)
#define g_mode 5
# define gm_DATA 0
# define gm_FILL 2
#define set_g_mode(mode) outport2(graph_addr, g_mode, mode)
#define g_mask 8
#define set_g_mask(mask) outport2(graph_addr, g_mask, mask)
#define select_g_mask() outportb(graph_addr, g_mask)
#define out_g_mask(mask) outportb(graph_addr+1, mask)
#define regen 0xa000
#if defined(M_UNIX) || defined(M_XENIX) || defined(UNIX) || defined(SYSV) || defined(__linux__)
#undef outportb
#if defined(__GNUC__)
static inline void
outportb(int port, int data)
{
__asm__ volatile ("outb %0,%1"::
"a" ((unsigned char)data),
"d" ((unsigned short)port));
}
static inline void
outport2(int port, int index, int data)
{
__asm__ volatile ("movb %0,%%ah; movb %1,%%al; outw %%ax,%2"::
"qmi" ((unsigned char)data),
"qmi" ((unsigned char)index),
"d" ((unsigned short)port):
"eax");
}
#else
void outportb(uint, byte);
void outport2(uint, byte, byte);
#endif
#undef mk_fb_ptr
extern fb_ptr fb_addr;
#define mk_fb_ptr(x, y) (fb_addr + (y) * (fb_dev->raster) + ((x) >> 3))
#else
#define outport2(port, index, data)\
(outportb(port, index), outportb((port)+1, data))
#endif
static unsigned char byte_discard_;
#define byte_discard(expr) byte_discard_ = (expr)
#endif