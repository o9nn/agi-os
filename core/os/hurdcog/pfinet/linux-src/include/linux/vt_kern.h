#ifndef _VT_KERN_H
#define _VT_KERN_H
#include <linux/config.h>
#include <linux/vt.h>
#ifdef CONFIG_VGA_CONSOLE
#define BROKEN_GRAPHICS_PROGRAMS 1
#endif
extern struct vt_struct {
int vc_num;
unsigned char	vc_mode;
struct vt_mode	vt_mode;
int		vt_pid;
int		vt_newvt;
struct wait_queue *paste_wait;
} *vt_cons[MAX_NR_CONSOLES];
void (*kd_mksound)(unsigned int hz, unsigned int ticks);
struct console_font_op;
struct consw;
int vc_allocate(unsigned int console);
int vc_cons_allocated(unsigned int console);
int vc_resize(unsigned int lines, unsigned int cols,
unsigned int first, unsigned int last);
#define vc_resize_all(l, c) vc_resize(l, c, 0, MAX_NR_CONSOLES-1)
#define vc_resize_con(l, c, x) vc_resize(l, c, x, x)
void vc_disallocate(unsigned int console);
void reset_palette(int currcons);
void set_palette(int currcons);
void do_blank_screen(int gfx_mode);
void unblank_screen(void);
void poke_blanked_console(void);
int con_font_op(int currcons, struct console_font_op *op);
int con_set_cmap(unsigned char *cmap);
int con_get_cmap(unsigned char *cmap);
void scrollback(int);
void scrollfront(int);
void update_region(int currcons, unsigned long start, int count);
void redraw_screen(int new_console, int is_switch);
#define update_screen(x) redraw_screen(x, 0)
#define switch_screen(x) redraw_screen(x, 1)
struct tty_struct;
int tioclinux(struct tty_struct *tty, unsigned long arg);
struct unimapinit;
struct unipair;
int con_set_trans_old(unsigned char * table);
int con_get_trans_old(unsigned char * table);
int con_set_trans_new(unsigned short * table);
int con_get_trans_new(unsigned short * table);
int con_clear_unimap(int currcons, struct unimapinit *ui);
int con_set_unimap(int currcons, ushort ct, struct unipair *list);
int con_get_unimap(int currcons, ushort ct, ushort *uct, struct unipair *list);
int con_set_default_unimap(int currcons);
void con_free_unimap(int currcons);
void con_protect_unimap(int currcons, int rdonly);
int con_copy_unimap(int dstcons, int srccons);
extern unsigned int video_font_height;
extern unsigned int default_font_height;
extern unsigned int video_scan_lines;
void complete_change_console(unsigned int new_console);
int vt_waitactive(int vt);
void change_console(unsigned int);
void reset_vc(unsigned int new_console);
int vt_waitactive(int vt);
#endif