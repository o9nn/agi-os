#ifndef _LINUX_CONSOLE_H_
#define _LINUX_CONSOLE_H_ 1
struct vc_data;
struct console_font_op;
#define VT100ID "\033[?1;2c"
#define VT102ID "\033[?6c"
struct consw {
const char *(*con_startup)(void);
void	(*con_init)(struct vc_data *, int);
void	(*con_deinit)(struct vc_data *);
void	(*con_clear)(struct vc_data *, int, int, int, int);
void	(*con_putc)(struct vc_data *, int, int, int);
void	(*con_putcs)(struct vc_data *, const unsigned short *, int, int, int);
void	(*con_cursor)(struct vc_data *, int);
int	(*con_scroll)(struct vc_data *, int, int, int, int);
void	(*con_bmove)(struct vc_data *, int, int, int, int, int, int);
int	(*con_switch)(struct vc_data *);
int	(*con_blank)(struct vc_data *, int);
int	(*con_font_op)(struct vc_data *, struct console_font_op *);
int	(*con_set_palette)(struct vc_data *, unsigned char *);
int	(*con_scrolldelta)(struct vc_data *, int);
int	(*con_set_origin)(struct vc_data *);
void	(*con_save_screen)(struct vc_data *);
u8	(*con_build_attr)(struct vc_data *, u8, u8, u8, u8, u8);
void	(*con_invert_region)(struct vc_data *, u16 *, int);
u16    *(*con_screen_pos)(struct vc_data *, int);
unsigned long (*con_getxy)(struct vc_data *, unsigned long, int *, int *);
};
extern struct consw *conswitchp;
extern struct consw dummy_con;
extern struct consw fb_con;
extern struct consw vga_con;
extern struct consw newport_con;
extern struct consw prom_con;
void take_over_console(struct consw *sw, int first, int last, int deflt);
void give_up_console(struct consw *sw);
#define SM_UP       (1)
#define SM_DOWN     (2)
#define CM_DRAW     (1)
#define CM_ERASE    (2)
#define CM_MOVE     (3)
struct console_cmdline
{
char	name[8];
int	index;
char	*options;
};
#define MAX_CMDLINECONSOLES 8
extern struct console_cmdline console_list[MAX_CMDLINECONSOLES];
#define CON_PRINTBUFFER	(1)
#define CON_CONSDEV	(2)
#define CON_ENABLED	(4)
struct console
{
char	name[8];
void	(*write)(struct console *, const char *, unsigned);
int	(*read)(struct console *, const char *, unsigned);
kdev_t	(*device)(struct console *);
int	(*wait_key)(struct console *);
void	(*unblank)(void);
int	(*setup)(struct console *, char *);
short	flags;
short	index;
int	cflag;
struct	 console *next;
};
extern void register_console(struct console *);
extern int unregister_console(struct console *);
extern struct console *console_drivers;
#define VESA_NO_BLANKING        0
#define VESA_VSYNC_SUSPEND      1
#define VESA_HSYNC_SUSPEND      2
#define VESA_POWERDOWN          3
#endif