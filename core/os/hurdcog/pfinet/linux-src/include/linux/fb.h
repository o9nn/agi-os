#ifndef _LINUX_FB_H
#define _LINUX_FB_H
#include <asm/types.h>
#define FB_MAJOR 29
#define FB_MODES_SHIFT 5
#define FB_NUM_MINORS 256
#define FB_MAX (FB_NUM_MINORS / (1 << FB_MODES_SHIFT))
#define GET_FB_IDX(node) (MINOR(node) >> FB_MODES_SHIFT)
#define FBIOGET_VSCREENINFO 0x4600
#define FBIOPUT_VSCREENINFO 0x4601
#define FBIOGET_FSCREENINFO 0x4602
#define FBIOGETCMAP 0x4604
#define FBIOPUTCMAP 0x4605
#define FBIOPAN_DISPLAY 0x4606
#define FBIOGET_CON2FBMAP 0x460F
#define FBIOPUT_CON2FBMAP 0x4610
#define FBIOBLANK 0x4611
#define FB_TYPE_PACKED_PIXELS 0
#define FB_TYPE_PLANES 1
#define FB_TYPE_INTERLEAVED_PLANES 2
#define FB_TYPE_TEXT 3
#define FB_TYPE_VGA_PLANES 4
#define FB_AUX_TEXT_MDA 0
#define FB_AUX_TEXT_CGA 1
#define FB_AUX_TEXT_S3_MMIO 2
#define FB_AUX_TEXT_MGA_STEP16 3
#define FB_AUX_TEXT_MGA_STEP8 4
#define FB_VISUAL_MONO01 0
#define FB_VISUAL_MONO10 1
#define FB_VISUAL_TRUECOLOR 2
#define FB_VISUAL_PSEUDOCOLOR 3
#define FB_VISUAL_DIRECTCOLOR 4
#define FB_VISUAL_STATIC_PSEUDOCOLOR 5
#define FB_ACCEL_NONE 0
#define FB_ACCEL_ATARIBLITT 1
#define FB_ACCEL_AMIGABLITT 2
#define FB_ACCEL_S3_TRIO64 3
#define FB_ACCEL_NCR_77C32BLT 4
#define FB_ACCEL_S3_VIRGE 5
#define FB_ACCEL_ATI_MACH64GX 6
#define FB_ACCEL_DEC_TGA 7
#define FB_ACCEL_ATI_MACH64CT 8
#define FB_ACCEL_ATI_MACH64VT 9
#define FB_ACCEL_ATI_MACH64GT 10
#define FB_ACCEL_SUN_CREATOR 11
#define FB_ACCEL_SUN_CGSIX 12
#define FB_ACCEL_SUN_LEO 13
#define FB_ACCEL_IMS_TWINTURBO 14
#define FB_ACCEL_3DLABS_PERMEDIA2 15
#define FB_ACCEL_MATROX_MGA2064W 16
#define FB_ACCEL_MATROX_MGA1064SG 17
#define FB_ACCEL_MATROX_MGA2164W 18
#define FB_ACCEL_MATROX_MGA2164W_AGP 19
#define FB_ACCEL_MATROX_MGAG100 20
#define FB_ACCEL_MATROX_MGAG200 21
#define FB_ACCEL_SUN_CG14 22
#define FB_ACCEL_SUN_BWTWO 23
#define FB_ACCEL_SUN_CGTHREE 24
#define FB_ACCEL_SUN_TCX 25
#define FB_ACCEL_MATROX_MGAG400 26
struct fb_fix_screeninfo {
char id[16];
char *smem_start;
__u32 smem_len;
__u32 type;
__u32 type_aux;
__u32 visual;
__u16 xpanstep;
__u16 ypanstep;
__u16 ywrapstep;
__u32 line_length;
char *mmio_start;
__u32 mmio_len;
__u32 accel;
__u16 reserved[3];
};
struct fb_bitfield {
__u32 offset;
__u32 length;
__u32 msb_right;
};
#define FB_NONSTD_HAM 1
#define FB_ACTIVATE_NOW 0
#define FB_ACTIVATE_NXTOPEN 1
#define FB_ACTIVATE_TEST 2
#define FB_ACTIVATE_MASK 15
#define FB_ACTIVATE_VBL 16
#define FB_CHANGE_CMAP_VBL 32
#define FB_ACTIVATE_ALL 64
#define FB_ACCELF_TEXT 1
#define FB_SYNC_HOR_HIGH_ACT 1
#define FB_SYNC_VERT_HIGH_ACT 2
#define FB_SYNC_EXT 4
#define FB_SYNC_COMP_HIGH_ACT 8
#define FB_SYNC_BROADCAST 16
#define FB_SYNC_ON_GREEN 32
#define FB_VMODE_NONINTERLACED 0
#define FB_VMODE_INTERLACED 1
#define FB_VMODE_DOUBLE 2
#define FB_VMODE_MASK 255
#define FB_VMODE_YWRAP 256
#define FB_VMODE_SMOOTH_XPAN 512
#define FB_VMODE_CONUPDATE 512
struct fb_var_screeninfo {
__u32 xres;
__u32 yres;
__u32 xres_virtual;
__u32 yres_virtual;
__u32 xoffset;
__u32 yoffset;
__u32 bits_per_pixel;
__u32 grayscale;
struct fb_bitfield red;
struct fb_bitfield green;
struct fb_bitfield blue;
struct fb_bitfield transp;
__u32 nonstd;
__u32 activate;
__u32 height;
__u32 width;
__u32 accel_flags;
__u32 pixclock;
__u32 left_margin;
__u32 right_margin;
__u32 upper_margin;
__u32 lower_margin;
__u32 hsync_len;
__u32 vsync_len;
__u32 sync;
__u32 vmode;
__u32 reserved[6];
};
struct fb_cmap {
__u32 start;
__u32 len;
__u16 *red;
__u16 *green;
__u16 *blue;
__u16 *transp;
};
struct fb_con2fbmap {
__u32 console;
__u32 framebuffer;
};
struct fb_monspecs {
__u32 hfmin;
__u32 hfmax;
__u16 vfmin;
__u16 vfmax;
unsigned dpms : 1;
};
#ifdef __KERNEL__
#include <linux/fs.h>
struct fb_info;
struct fb_info_gen;
struct vm_area_struct;
struct file;
struct fb_ops {
int (*fb_open)(struct fb_info *info, int user);
int (*fb_release)(struct fb_info *info, int user);
int (*fb_get_fix)(struct fb_fix_screeninfo *fix, int con,
struct fb_info *info);
int (*fb_get_var)(struct fb_var_screeninfo *var, int con,
struct fb_info *info);
int (*fb_set_var)(struct fb_var_screeninfo *var, int con,
struct fb_info *info);
int (*fb_get_cmap)(struct fb_cmap *cmap, int kspc, int con,
struct fb_info *info);
int (*fb_set_cmap)(struct fb_cmap *cmap, int kspc, int con,
struct fb_info *info);
int (*fb_pan_display)(struct fb_var_screeninfo *var, int con,
struct fb_info *info);
int (*fb_ioctl)(struct inode *inode, struct file *file, unsigned int cmd,
unsigned long arg, int con, struct fb_info *info);
int (*fb_mmap)(struct fb_info *info, struct file *file, struct vm_area_struct *vma);
int (*fb_rasterimg)(struct fb_info *info, int start);
};
struct display {
struct fb_var_screeninfo var;
struct fb_cmap cmap;
char *screen_base;
int visual;
int type;
int type_aux;
u_short ypanstep;
u_short ywrapstep;
u_long line_length;
u_short can_soft_blank;
u_short inverse;
struct display_switch *dispsw;
void *dispsw_data;
#if 0
struct fb_fix_cursorinfo fcrsr;
struct fb_var_cursorinfo *vcrsr;
struct fb_cursorstate crsrstate;
#endif
struct vc_data *conp;
struct fb_info *fb_info;
int vrows;
unsigned short cursor_x;
unsigned short cursor_y;
int fgcol;
int bgcol;
u_long next_line;
u_long next_plane;
u_char *fontdata;
unsigned short _fontheightlog;
unsigned short _fontwidthlog;
unsigned short _fontheight;
unsigned short _fontwidth;
int userfont;
u_short scrollmode;
short yscroll;
unsigned char fgshift, bgshift;
unsigned short charmask;
};
struct fb_info {
char modename[40];
kdev_t node;
int flags;
#define FBINFO_FLAG_MODULE 1
struct fb_ops *fbops;
struct fb_monspecs monspecs;
struct display *disp;
struct vc_data *display_fg;
char fontname[40];
int (*changevar)(int);
int (*switch_con)(int, struct fb_info*);
int (*updatevar)(int, struct fb_info*);
void (*blank)(int, struct fb_info*);
};
#ifdef MODULE
#define FBINFO_FLAG_DEFAULT FBINFO_FLAG_MODULE
#else
#define FBINFO_FLAG_DEFAULT 0
#endif
struct fbgen_hwswitch {
void (*detect)(void);
int (*encode_fix)(struct fb_fix_screeninfo *fix, const void *par,
struct fb_info_gen *info);
int (*decode_var)(const struct fb_var_screeninfo *var, void *par,
struct fb_info_gen *info);
int (*encode_var)(struct fb_var_screeninfo *var, const void *par,
struct fb_info_gen *info);
void (*get_par)(void *par, struct fb_info_gen *info);
void (*set_par)(const void *par, struct fb_info_gen *info);
int (*getcolreg)(unsigned regno, unsigned *red, unsigned *green,
unsigned *blue, unsigned *transp, struct fb_info *info);
int (*setcolreg)(unsigned regno, unsigned red, unsigned green,
unsigned blue, unsigned transp, struct fb_info *info);
int (*pan_display)(const struct fb_var_screeninfo *var,
struct fb_info_gen *info);
int (*blank)(int blank_mode, struct fb_info_gen *info);
void (*set_disp)(const void *par, struct display *disp,
struct fb_info_gen *info);
};
struct fb_info_gen {
struct fb_info info;
u_int parsize;
struct fbgen_hwswitch *fbhw;
};
extern int fbgen_get_fix(struct fb_fix_screeninfo *fix, int con,
struct fb_info *info);
extern int fbgen_get_var(struct fb_var_screeninfo *var, int con,
struct fb_info *info);
extern int fbgen_set_var(struct fb_var_screeninfo *var, int con,
struct fb_info *info);
extern int fbgen_get_cmap(struct fb_cmap *cmap, int kspc, int con,
struct fb_info *info);
extern int fbgen_set_cmap(struct fb_cmap *cmap, int kspc, int con,
struct fb_info *info);
extern int fbgen_pan_display(struct fb_var_screeninfo *var, int con,
struct fb_info *info);
extern int fbgen_ioctl(struct inode *inode, struct file *file,
unsigned int cmd, unsigned long arg, int con,
struct fb_info *info);
extern int fbgen_do_set_var(struct fb_var_screeninfo *var, int isactive,
struct fb_info_gen *info);
extern void fbgen_set_disp(int con, struct fb_info_gen *info);
extern void fbgen_install_cmap(int con, struct fb_info_gen *info);
extern int fbgen_update_var(int con, struct fb_info *info);
extern int fbgen_switch(int con, struct fb_info *info);
extern void fbgen_blank(int blank, struct fb_info *info);
struct fb_videomode {
const char *name;
struct fb_var_screeninfo var;
};
extern int register_framebuffer(struct fb_info *fb_info);
extern int unregister_framebuffer(const struct fb_info *fb_info);
extern int fbmon_valid_timings(u_int pixclock, u_int htotal, u_int vtotal,
const struct fb_info *fb_info);
extern int fbmon_dpms(const struct fb_info *fb_info);
extern int num_registered_fb;
extern struct fb_info *registered_fb[FB_MAX];
extern char con2fb_map[MAX_NR_CONSOLES];
extern struct display fb_display[MAX_NR_CONSOLES];
extern int fb_alloc_cmap(struct fb_cmap *cmap, int len, int transp);
extern void fb_copy_cmap(struct fb_cmap *from, struct fb_cmap *to,
int fsfromto);
extern int fb_get_cmap(struct fb_cmap *cmap, int kspc,
int (*getcolreg)(u_int, u_int *, u_int *, u_int *,
u_int *, struct fb_info *),
struct fb_info *fb_info);
extern int fb_set_cmap(struct fb_cmap *cmap, int kspc,
int (*setcolreg)(u_int, u_int, u_int, u_int, u_int,
struct fb_info *),
struct fb_info *fb_info);
extern struct fb_cmap *fb_default_cmap(int len);
extern void fb_invert_cmaps(void);
#define VESA_NO_BLANKING 0
#define VESA_VSYNC_SUSPEND 1
#define VESA_HSYNC_SUSPEND 2
#define VESA_POWERDOWN 3
#endif
#if 1
#define FBCMD_GET_CURRENTPAR 0xDEAD0005
#define FBCMD_SET_CURRENTPAR 0xDEAD8005
#endif
#if 1
#define FBIOGET_FCURSORINFO 0x4607
#define FBIOGET_VCURSORINFO 0x4608
#define FBIOPUT_VCURSORINFO 0x4609
#define FBIOGET_CURSORSTATE 0x460A
#define FBIOPUT_CURSORSTATE 0x460B
struct fb_fix_cursorinfo {
__u16 crsr_width;
__u16 crsr_height;
__u16 crsr_xsize;
__u16 crsr_ysize;
__u16 crsr_color1;
__u16 crsr_color2;
};
struct fb_var_cursorinfo {
__u16 width;
__u16 height;
__u16 xspot;
__u16 yspot;
__u8 data[1];
};
struct fb_cursorstate {
__s16 xoffset;
__s16 yoffset;
__u16 mode;
};
#define FB_CURSOR_OFF 0
#define FB_CURSOR_ON 1
#define FB_CURSOR_FLASH 2
#endif
#endif