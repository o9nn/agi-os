#include <sys/types.h>
#include <kern/debug.h>
#include <kern/mach_clock.h>
#include <kern/printf.h>
#include <device/conf.h>
#include <device/tty.h>
#include <device/io_req.h>
#include <device/buf.h>
#include <vm/vm_kern.h>
#include <i386/db_interface.h>
#include <i386/irq.h>
#include <i386/locore.h>
#include <i386/loose_ends.h>
#include <i386/vm_param.h>
#include <i386/spl.h>
#include <i386/pio.h>
#include <i386at/cram.h>
#include <i386at/kd.h>
#include <i386at/kd_event.h>
#include <i386at/kd_mouse.h>
#include <i386at/kdsoft.h>
#include <device/cons.h>
#include <util/atoi.h>
#define DEBUG	1
#if 0
#define BROKEN_KEYBOARD_RESET
#endif
struct tty       kd_tty;
extern boolean_t rebootflag;
static void charput(csrpos_t pos, char ch, char chattr);
static void charmvup(csrpos_t from, csrpos_t to, int count);
static void charmvdown(csrpos_t from, csrpos_t to, int count);
static void charclear(csrpos_t to, int count, char chattr);
static void charsetcursor(csrpos_t newpos);
static void kd_noopreset(void);
void	(*kd_dput)(csrpos_t, char, char) = charput;
void	(*kd_dmvup)(csrpos_t, csrpos_t, int)	= charmvup;
void	(*kd_dmvdown)(csrpos_t, csrpos_t, int)	= charmvdown;
void	(*kd_dclear)(csrpos_t, int, char)	= charclear;
void	(*kd_dsetcursor)(csrpos_t) = charsetcursor;
void	(*kd_dreset)(void) = kd_noopreset;
vm_offset_t kd_bitmap_start = (vm_offset_t)0xa0000;
u_char 	*vid_start	= (u_char *)EGA_START;
csrpos_t kd_curpos	= 0;
short	kd_lines	= 25;
short	kd_cols		= 80;
char	kd_attr		= KA_NORMAL;
char	kd_color	= KA_NORMAL;
char	kd_attrflags	= 0;
int  	kd_state	= KS_NORMAL;
int	kb_mode		= KB_ASCII;
int kd_kbd_mouse = 0;
int kd_kbd_magic_scale = 6;
int kd_kbd_magic_button  = 0;
enum why_ack {NOT_WAITING, SET_LEDS, DATA_ACK};
enum why_ack	kd_ack	= NOT_WAITING;
u_char last_sent = 0;
u_char	kd_nextled	= 0;
boolean_t kd_initialized 	= FALSE;
boolean_t kd_extended	= FALSE;
#define	K_MAXESC	32
u_char	esc_seq[K_MAXESC];
u_char	*esc_spt	= (u_char *)0;
unsigned char	key_map[NUMKEYS][WIDTH_KMAP] = {
{NC,NC,NC,      NC,NC,NC,       NC,NC,NC,      NC,NC,NC,      NC,NC,NC},
{K_ESC,NC,NC,   K_ESC,NC,NC,    K_ESC,NC,NC,   0x1b,K_ESC,NC, K_ESC,NC,NC},
{K_ONE,NC,NC,   K_BANG,NC,NC,   K_ONE,NC,NC,   0x1b,K_ONE,NC,  0x1b,0x4e,K_BANG},
{K_TWO,NC,NC,   K_ATSN,NC,NC,   K_NUL,NC,NC,   0x1b,K_TWO,NC,  0x1b,0x4e,K_ATSN},
{K_THREE,NC,NC, K_POUND,NC,NC,  K_THREE,NC,NC, 0x1b,K_THREE,NC,  0x1b,0x4e,K_POUND},
{K_FOUR,NC,NC,  K_DOLLAR,NC,NC, K_FOUR,NC,NC,  0x1b,K_FOUR,NC,  0x1b,0x4e,K_DOLLAR},
{K_FIVE,NC,NC,  K_PERC,NC,NC,   K_FIVE,NC,NC,  0x1b,K_FIVE,NC,  0x1b,0x4e,K_PERC},
{K_SIX,NC,NC,   K_CARET,NC,NC,  K_RS,NC,NC,    0x1b,K_SIX,NC,  0x1b,0x4e,K_CARET},
{K_SEVEN,NC,NC, K_AMPER,NC,NC,  K_SEVEN,NC,NC, 0x1b,K_SEVEN,NC,  0x1b,0x4e,K_AMPER},
{K_EIGHT,NC,NC, K_ASTER,NC,NC,  K_EIGHT,NC,NC, 0x1b,K_EIGHT,NC,  0x1b,0x4e,K_ASTER},
{K_NINE,NC,NC,  K_LPAREN,NC,NC, K_NINE,NC,NC,  0x1b,K_NINE,NC,  0x1b,0x4e,K_LPAREN},
{K_ZERO,NC,NC,  K_RPAREN,NC,NC, K_ZERO,NC,NC,  0x1b,K_ZERO,NC,  0x1b,0x4e,K_RPAREN},
{K_MINUS,NC,NC, K_UNDSC,NC,NC,  K_US,NC,NC,    0x1b,K_MINUS,NC,  0x1b,0x4e,K_UNDSC},
{K_EQL,NC,NC,   K_PLUS,NC,NC,   K_EQL,NC,NC,   0x1b,K_EQL,NC,  0x1b,0x4e,K_PLUS},
{K_DEL,NC,NC,   K_DEL,NC,NC,    K_DEL,NC,NC,   0x1b,K_DEL,NC, K_DEL,NC,NC},
{K_HT,NC,NC,    K_GS,NC,NC,     K_HT,NC,NC,    0x1b,K_HT,NC,  K_GS,NC,NC},
{K_q,NC,NC,     K_Q,NC,NC,      K_DC1,NC,NC,   0x1b,K_q,NC,  0x1b,0x4e,K_Q},
{K_w,NC,NC,     K_W,NC,NC,      K_ETB,NC,NC,   0x1b,K_w,NC,  0x1b,0x4e,K_W},
{K_e,NC,NC,     K_E,NC,NC,      K_ENQ,NC,NC,   0x1b,K_e,NC,  0x1b,0x4e,K_E},
{K_r,NC,NC,     K_R,NC,NC,      K_DC2,NC,NC,   0x1b,K_r,NC,  0x1b,0x4e,K_R},
{K_t,NC,NC,     K_T,NC,NC,      K_DC4,NC,NC,   0x1b,K_t,NC,  0x1b,0x4e,K_T},
{K_y,NC,NC,     K_Y,NC,NC,      K_EM,NC,NC,    0x1b,K_y,NC,  0x1b,0x4e,K_Y},
{K_u,NC,NC,     K_U,NC,NC,      K_NAK,NC,NC,   0x1b,K_u,NC,  0x1b,0x4e,K_U},
{K_i,NC,NC,     K_I,NC,NC,      K_HT,NC,NC,    0x1b,K_i,NC,  0x1b,0x4e,K_I},
{K_o,NC,NC,     K_O,NC,NC,      K_SI,NC,NC,    0x1b,K_o,NC,  0x1b,0x4e,K_O},
{K_p,NC,NC,     K_P,NC,NC,      K_DLE,NC,NC,   0x1b,K_p,NC,  0x1b,0x4e,K_P},
{K_LBRKT,NC,NC, K_LBRACE,NC,NC, K_ESC,NC,NC,   0x1b,K_LBRKT,NC,  0x1b,0x4e,K_LBRACE},
{K_RBRKT,NC,NC, K_RBRACE,NC,NC, K_GS,NC,NC,    0x1b,K_RBRKT,NC,  0x1b,0x4e,K_RBRACE},
{K_CR,NC,NC,    K_CR,NC,NC,     K_CR,NC,NC,    0x1b,K_CR,NC,  K_CR,NC,NC},
{K_SCAN,K_CTLSC,NC, K_SCAN,K_CTLSC,NC, K_SCAN,K_CTLSC,NC, K_SCAN,K_CTLSC,NC, K_SCAN,K_CTLSC,NC},
{K_a,NC,NC,     K_A,NC,NC,      K_SOH,NC,NC,   0x1b,K_a,NC,  0x1b,0x4e,K_A},
{K_s,NC,NC,     K_S,NC,NC,      K_DC3,NC,NC,   0x1b,K_s,NC,  0x1b,0x4e,K_S},
{K_d,NC,NC,     K_D,NC,NC,      K_EOT,NC,NC,   0x1b,K_d,NC,  0x1b,0x4e,K_D},
{K_f,NC,NC,     K_F,NC,NC,      K_ACK,NC,NC,   0x1b,K_f,NC,  0x1b,0x4e,K_F},
{K_g,NC,NC,     K_G,NC,NC,      K_BEL,NC,NC,   0x1b,K_g,NC,  0x1b,0x4e,K_G},
{K_h,NC,NC,     K_H,NC,NC,      K_BS,NC,NC,    0x1b,K_h,NC,  0x1b,0x4e,K_H},
{K_j,NC,NC,     K_J,NC,NC,      K_LF,NC,NC,    0x1b,K_j,NC,  0x1b,0x4e,K_J},
{K_k,NC,NC,     K_K,NC,NC,      K_VT,NC,NC,    0x1b,K_k,NC,  0x1b,0x4e,K_K},
{K_l,NC,NC,     K_L,NC,NC,      K_FF,NC,NC,    0x1b,K_l,NC,  0x1b,0x4e,K_L},
{K_SEMI,NC,NC,  K_COLON,NC,NC,  K_SEMI,NC,NC,  0x1b,K_SEMI,NC,  0x1b,0x4e,K_COLON},
{K_SQUOTE,NC,NC,K_DQUOTE,NC,NC, K_SQUOTE,NC,NC,0x1b,K_SQUOTE,NC,  0x1b,0x4e,K_DQUOTE},
{K_GRAV,NC,NC,  K_TILDE,NC,NC,  K_RS,NC,NC,    0x1b,K_GRAV,NC,  0x1b,0x4e,K_TILDE},
{K_SCAN,K_LSHSC,NC, K_SCAN,K_LSHSC,NC, K_SCAN,K_LSHSC,NC, K_SCAN,K_LSHSC,NC, K_SCAN,K_LSHSC,NC},
{K_BSLSH,NC,NC, K_PIPE,NC,NC,   K_FS,NC,NC,    0x1b,K_BSLSH,NC,  0x1b,0x4e,K_PIPE},
{K_z,NC,NC,     K_Z,NC,NC,      K_SUB,NC,NC,   0x1b,K_z,NC,  0x1b,0x4e,K_Z},
{K_x,NC,NC,     K_X,NC,NC,      K_CAN,NC,NC,   0x1b,K_x,NC,  0x1b,0x4e,K_X},
{K_c,NC,NC,     K_C,NC,NC,      K_ETX,NC,NC,   0x1b,K_c,NC,  0x1b,0x4e,K_C},
{K_v,NC,NC,     K_V,NC,NC,      K_SYN,NC,NC,   0x1b,K_v,NC,  0x1b,0x4e,K_V},
{K_b,NC,NC,     K_B,NC,NC,      K_STX,NC,NC,   0x1b,K_b,NC,  0x1b,0x4e,K_B},
{K_n,NC,NC,     K_N,NC,NC,      K_SO,NC,NC,    0x1b,K_n,NC,  0x1b,0x4e,K_N},
{K_m,NC,NC,     K_M,NC,NC,      K_CR,NC,NC,    0x1b,K_m,NC,  0x1b,0x4e,K_M},
{K_COMMA,NC,NC, K_LTHN,NC,NC,   K_COMMA,NC,NC, 0x1b,K_COMMA,NC,  0x1b,0x4e,K_LTHN},
{K_PERIOD,NC,NC,K_GTHN,NC,NC,   K_PERIOD,NC,NC,0x1b,K_PERIOD,NC,  0x1b,0x4e,K_GTHN},
{K_SLASH,NC,NC, K_QUES,NC,NC,   K_SLASH,NC,NC, 0x1b,K_SLASH,NC,  0x1b,0x4e,K_QUES},
{K_SCAN,K_RSHSC,NC, K_SCAN,K_RSHSC,NC, K_SCAN,K_RSHSC,NC, K_SCAN,K_RSHSC,NC, K_SCAN,K_RSHSC,NC},
{K_ASTER,NC,NC, K_ASTER,NC,NC,  K_ASTER,NC,NC, 0x1b,K_ASTER,NC, 0x1b,0x4e,K_ASTER},
{K_SCAN,K_ALTSC,NC, K_SCAN,K_ALTSC,NC, K_SCAN,K_ALTSC,NC, K_SCAN,K_ALTSC,NC, K_SCAN,K_ALTSC,NC},
{K_SPACE,NC,NC, K_SPACE,NC,NC,  K_NUL,NC,NC,   0x1b,K_SPACE,NC, K_SPACE,NC,NC},
{K_SCAN,K_CLCKSC,NC, K_SCAN,K_CLCKSC,NC, K_SCAN,K_CLCKSC,NC, K_SCAN,K_CLCKSC,NC, K_SCAN,K_CLCKSC,NC},
{K_F1,  K_F1S,  K_F1,  K_F1A,  K_F1S},
{K_F2,  K_F2S,  K_F2,  K_F2A,  K_F2S},
{K_F3,  K_F3S,  K_F3,  K_F3A,  K_F3S},
{K_F4,  K_F4S,  K_F4,  K_F4A,  K_F4S},
{K_F5,  K_F5S,  K_F5,  K_F5A,  K_F5S},
{K_F6,  K_F6S,  K_F6,  K_F6A,  K_F6S},
{K_F7,  K_F7S,  K_F7,  K_F7A,  K_F7S},
{K_F8,  K_F8S,  K_F8,  K_F8A,  K_F8S},
{K_F9,  K_F9S,  K_F9,  K_F9A,  K_F9S},
{K_F10, K_F10S, K_F10, K_F10A, K_F10S},
{K_SCAN,K_NLCKSC,NC, K_SCAN,K_NLCKSC,NC, K_SCAN,K_NLCKSC,NC, K_SCAN,K_NLCKSC,NC, K_SCAN,K_NLCKSC,NC},
{K_SCRL,         K_NUL,NC,NC,    K_SCRL,        K_SCRL,      K_NUL,NC,NC},
{K_HOME,         K_SEVEN,NC,NC,  K_HOME,        K_HOME,      0x1b,0x4e,K_SEVEN},
{K_UA,           K_EIGHT,NC,NC,  K_UA,          K_UA,        0x1b,0x4e,K_EIGHT},
{K_PUP,          K_NINE,NC,NC,   K_PUP,         K_PUP,       0x1b,0x4e,K_NINE},
{0x1b,0x5b,0x53, K_MINUS,NC,NC,  0x1b,0x5b,0x53, 0x1b,0x5b,0x53, 0x1b,0x4e,0x2d},
{K_LA,           K_FOUR,NC,NC,   K_LA,          K_LA,        0x1b,0x4e,K_FOUR},
{0x1b,0x5b,0x47, K_FIVE,NC,NC,   0x1b,0x5b,0x47, 0x1b,0x5b,0x47, 0x1b,0x4e,0x35},
{K_RA,           K_SIX,NC,NC,    K_RA,          K_RA,        0x1b,0x4e,K_SIX},
{0x1b,0x5b,0x54, K_PLUS,NC,NC,   0x1b,0x5b,0x54, 0x1b,0x5b,0x54, 0x1b,0x4e,0x2b},
{K_END,          K_ONE,NC,NC,    K_END,         K_END,       0x1b,0x4e,K_ONE},
{K_DA,           K_TWO,NC,NC,    K_DA,          K_DA,        0x1b,0x4e,K_TWO},
{K_PDN,          K_THREE,NC,NC,  K_PDN,         K_PDN,       0x1b,0x4e,K_THREE},
{K_INS,          K_ZERO,NC,NC,   K_INS,         K_INS,       0x1b,0x4e,K_ZERO},
{0x1b,0x5b,0x39, K_PERIOD,NC,NC, K_DEL,NC,NC,   K_DEL,NC,NC, 0x1b,0x4e,K_PERIOD},
{NC,NC,NC,       NC,NC,NC,       NC,NC,NC,      NC,NC,NC,    NC,NC,NC},
{NC,NC,NC,       NC,NC,NC,       NC,NC,NC,      NC,NC,NC,    NC,NC,NC},
{NC,NC,NC,       NC,NC,NC,       NC,NC,NC,      NC,NC,NC,    NC,NC,NC},
{K_F11,          K_F11S,         K_F11,         K_F11A,      K_F11S},
{K_F12,          K_F12S,         K_F12,         K_F12A,      K_F12S}
};
short	kd_index_reg	= EGA_IDX_REG;
short	kd_io_reg	= EGA_IO_REG;
u_char	*font_start	= 0;
short	fb_width	= 0;
short	fb_height	= 0;
short	char_width	= 0;
short	char_height	= 0;
short	chars_in_font	= 0;
short	cursor_height	= 0;
u_char	char_black	= 0;
u_char	char_white	= 0xff;
short	xstart		= 0;
short	ystart		= 0;
short	char_byte_width	= 0;
short	fb_byte_width	= 0;
short	font_byte_width	= 0;
int	kd_pollc = 0;
#ifdef	DEBUG
static void
pause(void)
{
int i;
for (i = 0; i < 50000; ++i)
;
}
void
feep(void)
{
kd_bellon();
pause();
kd_belloff(NULL);
}
void
kd_debug_put(
int	loc,
char	c)
{
csrpos_t pos = ONE_PAGE - (loc+1) * ONE_SPACE;
(*kd_dput)(pos, c, KA_NORMAL);
}
#endif
extern boolean_t	mouse_in_use;
int			old_kb_mode;
void
cnpollc(boolean_t on)
{
if (mouse_in_use) {
if (on) {
old_kb_mode = kb_mode;
kb_mode = KB_ASCII;
X_kdb_enter();
kd_pollc++;
} else {
--kd_pollc;
X_kdb_exit();
kb_mode = old_kb_mode;
}
} else {
if (on) {
kd_pollc++;
} else {
--kd_pollc;
}
}
}
int
kdopen(
dev_t	 dev,
int	 flag,
io_req_t ior)
{
struct 	tty	*tp;
spl_t	o_pri;
tp = &kd_tty;
o_pri = simple_lock_irq(&tp->t_lock);
if (!(tp->t_state & (TS_ISOPEN|TS_WOPEN))) {
simple_unlock_nocheck(&tp->t_lock.slock);
ttychars(tp);
simple_lock_nocheck(&tp->t_lock.slock);
tp->t_oproc = kdstart;
tp->t_stop = kdstop;
tp->t_ospeed = tp->t_ispeed = B115200;
tp->t_flags = ODDP|EVENP|ECHO|CRMOD|XTABS|LITOUT;
kdinit();
}
tp->t_state |= TS_CARR_ON;
simple_unlock_irq(o_pri, &tp->t_lock);
return (char_open(dev, tp, flag, ior));
}
void
kdclose(dev_t dev, int flag)
{
struct	tty	*tp;
tp = &kd_tty;
{
spl_t s;
s = simple_lock_irq(&tp->t_lock);
ttyclose(tp);
simple_unlock_irq(s, &tp->t_lock);
}
return;
}
int
kdread(dev_t dev, io_req_t uio)
{
struct	tty	*tp;
tp = &kd_tty;
tp->t_state |= TS_CARR_ON;
return((*linesw[kd_tty.t_line].l_read)(tp, uio));
}
int
kdwrite(dev_t dev, io_req_t uio)
{
return((*linesw[kd_tty.t_line].l_write)(&kd_tty, uio));
}
vm_offset_t
kdmmap(dev_t dev, vm_offset_t off, vm_prot_t prot)
{
if (off >= (128*1024))
return(-1);
return(i386_btop(kd_bitmap_start+off));
}
int
kdportdeath(
dev_t		dev,
mach_port_t	port)
{
return (tty_portdeath(&kd_tty, (ipc_port_t)port));
}
io_return_t kdgetstat(
dev_t		dev,
dev_flavor_t	flavor,
dev_status_t	data,
mach_msg_type_number_t	*count)
{
io_return_t	result;
switch (flavor) {
case KDGSTATE:
if (*count < 1)
return (D_INVALID_OPERATION);
*data = kd_state;
*count = 1;
result = D_SUCCESS;
break;
case KDGKBENT:
result = kdgetkbent((struct kbentry *)data);
*count = sizeof(struct kbentry)/sizeof(int);
break;
default:
result = tty_get_status(&kd_tty, flavor, data, count);
break;
}
return (result);
}
io_return_t kdsetstat(
dev_t		dev,
dev_flavor_t	flavor,
dev_status_t	data,
mach_msg_type_number_t	count)
{
io_return_t	result;
switch (flavor) {
case KDSKBENT:
if (count < sizeof(struct kbentry)/sizeof(int)) {
return (D_INVALID_OPERATION);
}
result = kdsetkbent((struct kbentry *)data, 0);
break;
case KDSETBELL:
if (count < 1)
return (D_INVALID_OPERATION);
result = kdsetbell(*data, 0);
break;
default:
result = tty_set_status(&kd_tty, flavor, data, count);
}
return (result);
}
int
kdsetbell(
int	val,
int	flags)
{
int err = 0;
if (val == KD_BELLON)
kd_bellon();
else if (val == KD_BELLOFF)
kd_belloff(NULL);
else
err = D_INVALID_OPERATION;
return(err);
}
int
kdgetkbent(struct kbentry *kbent)
{
u_char *cp;
spl_t o_pri = SPLKD();
cp = &key_map[kbent->kb_index][CHARIDX(kbent->kb_state)];
kbent->kb_value[0] = *cp++;
kbent->kb_value[1] = *cp++;
kbent->kb_value[2] = *cp;
(void)splx(o_pri);
return(0);
}
int
kdsetkbent(
struct kbentry 	*kbent,
int		flags)
{
u_char *cp;
spl_t o_pri;
o_pri = SPLKD();
cp = &key_map[kbent->kb_index][CHARIDX(kbent->kb_state)];
*cp++ = kbent->kb_value[0];
*cp++ = kbent->kb_value[1];
*cp = kbent->kb_value[2];
(void)splx(o_pri);
return(0);
}
void
kdintr(int vec)
{
struct	tty	*tp;
unsigned char	c;
unsigned char	scancode;
unsigned int	char_idx;
boolean_t	up = FALSE;
if (kd_pollc)
return;
if (!kd_initialized)
return;
tp = &kd_tty;
#ifdef	old
while ((inb(K_STATUS) & K_OBUF_FUL) == 0)
;
#else
{
int safety = 1000;
while ((inb(K_STATUS) & K_OBUF_FUL) == 0)
if (!safety--) break;
}
#endif
if ((inb(K_STATUS) & 0x20) == 0x20) {
if (mouse_in_use) {
mouse_handle_byte((u_char)inb(K_RDWR));
return;
} else {
printf("M%xI", inb(K_RDWR));
return;
}
}
scancode = inb(K_RDWR);
if (scancode == K_EXTEND && kb_mode != KB_EVENT) {
kd_extended = TRUE;
goto done;
} else if (scancode == K_RESEND) {
kd_resend();
goto done;
} else if (scancode == K_ACKSC) {
kd_handle_ack();
goto done;
} else if (kd_kbd_mouse && kd_kbd_magic(scancode)) {
goto done;
} else if (kdcheckmagic(scancode)) {
goto done;
} else if (kb_mode == KB_EVENT) {
kd_enqsc(scancode);
goto done;
}
if (scancode & K_UP) {
up = TRUE;
scancode &= ~K_UP;
}
if (scancode < NUMKEYS) {
char_idx = kdstate2idx(kd_state, kd_extended);
c = key_map[scancode][char_idx];
if (c == K_SCAN) {
c = key_map[scancode][++char_idx];
set_kd_state(do_modifier(kd_state, c, up));
} else if (!up) {
unsigned int max;
max = char_idx + NUMOUTPUT;
char_idx++;
if (!kd_extended) {
if (kd_state&KS_CLKED) {
if (kd_isupper(c)) {
c += ('a' - 'A');
max = char_idx;
}
else if (kd_islower(c)) {
c -= ('a' - 'A');
max = char_idx;
}
}
if ((kd_state&KS_NLKED) &&
(((K_HOMESC) <= scancode) &&
(scancode <= (K_DELSC)))) {
char_idx = CHARIDX(SHIFT_STATE);
c = key_map[scancode][char_idx];
max = char_idx + NUMOUTPUT;
char_idx++;
}
}
for ( ; (c != K_DONE) && (char_idx <= max);
c = key_map[scancode][char_idx++]) {
(*linesw[tp->t_line].l_rint)(c, tp);
}
kd_extended = FALSE;
}
}
done:
return;
}
void
kd_handle_ack(void)
{
switch (kd_ack) {
case SET_LEDS:
kd_setleds2();
kd_ack = DATA_ACK;
break;
case DATA_ACK:
kd_ack = NOT_WAITING;
break;
case NOT_WAITING:
printf("unexpected ACK from keyboard\n");
break;
default:
panic("bogus kd_ack\n");
break;
}
}
void
kd_resend(void)
{
if (kd_ack == NOT_WAITING)
printf("unexpected RESEND from keyboard\n");
else
kd_senddata(last_sent);
}
int
do_modifier(
int		state,
Scancode	c,
boolean_t	up)
{
switch (c) {
case (K_ALTSC):
if (up)
state &= ~KS_ALTED;
else
state |= KS_ALTED;
kd_extended = FALSE;
break;
#ifndef	ORC
case (K_CLCKSC):
#endif
case (K_CTLSC):
if (up)
state &= ~KS_CTLED;
else
state |= KS_CTLED;
kd_extended = FALSE;
break;
#ifdef	ORC
case (K_CLCKSC):
if (!up)
state ^= KS_CLKED;
break;
#endif
case (K_NLCKSC):
if (!up)
state ^= KS_NLKED;
break;
case (K_LSHSC):
case (K_RSHSC):
if (up)
state &= ~KS_SHIFTED;
else
state |= KS_SHIFTED;
kd_extended = FALSE;
break;
}
return(state);
}
boolean_t
kdcheckmagic(Scancode scancode)
{
static int magic_state = KS_NORMAL;
boolean_t up = FALSE;
if (scancode == 0x46)
{
kd_kbd_mouse = !kd_kbd_mouse;
kd_kbd_magic_button = 0;
return(TRUE);
}
if (scancode & K_UP) {
up = TRUE;
scancode &= ~K_UP;
}
magic_state = do_modifier(magic_state, scancode, up);
if ((magic_state&(KS_CTLED|KS_ALTED)) == (KS_CTLED|KS_ALTED)) {
switch (scancode) {
#if	MACH_KDB
case K_dSC:
kdb_kintr();
(void)SPLKD();
magic_state = KS_NORMAL;
if (kb_mode == KB_ASCII)
kd_state = KS_NORMAL;
else {
kd_enqsc(K_ALTSC | K_UP);
kd_enqsc(K_CTLSC | K_UP);
kd_enqsc(K_dSC | K_UP);
}
return(TRUE);
break;
#endif
case K_DELSC:
if (rebootflag)
kdreboot();
break;
}
}
return(FALSE);
}
unsigned int
kdstate2idx(unsigned int	state,
boolean_t	extended)
{
int state_idx = NORM_STATE;
if ((!extended) && state != KS_NORMAL) {
if ((state&(KS_SHIFTED|KS_ALTED)) == (KS_SHIFTED|KS_ALTED))
state_idx = SHIFT_ALT;
else if (state&KS_CTLED)
state_idx = CTRL_STATE;
else if (state&KS_SHIFTED)
state_idx = SHIFT_STATE;
else if (state&KS_ALTED)
state_idx = ALT_STATE;
}
return (CHARIDX(state_idx));
}
void
kdstart(struct tty *tp)
{
spl_t	o_pri;
int	ch;
if (tp->t_state & TS_TTSTOP)
return;
for ( ; ; ) {
tp->t_state &= ~TS_BUSY;
if (tp->t_state & TS_TTSTOP)
break;
if ((tp->t_outq.c_cc <= 0) || (ch = getc(&tp->t_outq)) == -1)
break;
o_pri = splsoftclock();
kd_putc_esc(ch);
splx(o_pri);
}
if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
tt_write_wakeup(tp);
}
}
void
kdstop(
struct tty 	*tp,
int		flags)
{
}
void
kdinit(void)
{
unsigned char	k_comm;
if (kd_initialized)
return;
esc_spt = esc_seq;
kd_attr = KA_NORMAL;
kd_attrflags = 0;
kd_color = KA_NORMAL;
kd_xga_init();
if (inb(K_STATUS) & K_OBUF_FUL)
(void)inb(K_RDWR);
kd_sendcmd(KC_CMD_READ);
k_comm = kd_getdata();
k_comm &= ~K_CB_DISBLE;
k_comm |= K_CB_ENBLIRQ;
kd_sendcmd(KC_CMD_WRITE);
kd_senddata(k_comm);
unmask_irq(KBD_IRQ);
kd_initialized = TRUE;
#if	ENABLE_IMMEDIATE_CONSOLE
{
extern boolean_t immediate_console_enable;
immediate_console_enable = FALSE;
}
kd_setpos(ONE_PAGE - ONE_LINE); printf("\n");
#endif
cnsetleds(kd_state = KS_NORMAL);
ttychars(&kd_tty);
}
static boolean_t kd_bellstate = FALSE;
void
kd_belloff(void * param)
{
unsigned char status;
status = (inb(K_PORTB) & ~(K_SPKRDATA | K_ENABLETMR2));
outb(K_PORTB, status);
kd_bellstate = FALSE;
return;
}
void
kd_bellon(void)
{
unsigned char	status;
outb(K_TMRCTL, K_SELTMR2 | K_RDLDTWORD | K_TSQRWAVE | K_TBINARY);
outb(K_TMR2, 1500 & 0xff);
outb(K_TMR2, (int)1500 >> 8);
status = (inb(K_PORTB)| K_ENABLETMR2 | K_SPKRDATA);
outb(K_PORTB, status);
return;
}
void
kd_putc_esc(u_char c)
{
if (c == (K_ESC)) {
if (esc_spt == esc_seq) {
*(esc_spt++)=(K_ESC);
*(esc_spt) = '\0';
} else {
kd_putc((K_ESC));
esc_spt = esc_seq;
}
} else {
if (esc_spt - esc_seq) {
if (esc_spt - esc_seq > K_MAXESC - 1)
esc_spt = esc_seq;
else {
*(esc_spt++) = c;
*(esc_spt) = '\0';
kd_parseesc();
}
} else {
kd_putc(c);
}
}
}
int sit_for_0 = 1;
void
kd_putc(u_char ch)
{
if ((!ch) && sit_for_0)
return;
switch (ch) {
case ((K_LF)):
kd_down();
break;
case ((K_CR)):
kd_cr();
break;
case ((K_BS)):
kd_left();
break;
case ((K_HT)):
kd_tab();
break;
case ((K_BEL)):
if (!kd_bellstate)
{
kd_bellon();
timeout(kd_belloff, 0, hz/8 );
kd_bellstate = TRUE;
}
break;
default:
(*kd_dput)(kd_curpos, ch, kd_attr);
kd_right();
break;
}
return;
}
void
kd_setpos(csrpos_t newpos)
{
if (newpos > ONE_PAGE) {
kd_scrollup();
newpos = BOTTOM_LINE;
}
if (newpos < 0) {
kd_scrolldn();
newpos = 0;
}
(*kd_dsetcursor)(newpos);
}
void
kd_scrollup(void)
{
csrpos_t to;
csrpos_t from;
int	count;
to = 0;
from = ONE_LINE;
count = (ONE_PAGE - ONE_LINE)/ONE_SPACE;
(*kd_dmvup)(from, to, count);
to = BOTTOM_LINE;
count = ONE_LINE/ONE_SPACE;
(*kd_dclear)(to, count, kd_attr);
return;
}
void
kd_scrolldn(void)
{
csrpos_t to;
csrpos_t from;
int	count;
to 	= ONE_PAGE - ONE_SPACE;
from 	= ONE_PAGE - ONE_LINE - ONE_SPACE;
count 	= (ONE_PAGE - ONE_LINE) / ONE_SPACE;
(*kd_dmvdown)(from, to, count);
to	= 0;
count	= ONE_LINE/ONE_SPACE;
(*kd_dclear)(to, count, kd_attr);
return;
}
void
kd_parseesc(void)
{
u_char	*escp;
escp = esc_seq + 1;
switch(*(escp)) {
case 'c':
kd_cls();
kd_home();
esc_spt = esc_seq;
break;
case '[':
escp++;
kd_parserest(escp);
break;
case '\0':
break;
default:
kd_putc(*escp);
esc_spt = esc_seq;
break;
}
return;
}
#define reverse_video_char(a)       (((a) & 0x88) | ((((a) >> 4) | ((a) << 4)) & 0x77))
static void
kd_update_kd_attr(void)
{
kd_attr = kd_color;
if (kd_attrflags & KAX_UNDERLINE)
kd_attr = (kd_attr & 0xf0) | KAX_COL_UNDERLINE;
else if (kd_attrflags & KAX_DIM)
kd_attr = (kd_attr & 0xf0) | KAX_COL_DIM;
if (kd_attrflags & KAX_REVERSE)
kd_attr = reverse_video_char(kd_attr);
if (kd_attrflags & KAX_BLINK)
kd_attr ^= 0x80;
if (kd_attrflags & KAX_BOLD)
kd_attr ^= 0x08;
}
unsigned char color_table[] = { 0, 4, 2, 6, 1, 5, 3, 7,
8,12,10,14, 9,13,11,15 };
void
kd_parserest(u_char *cp)
{
int	number[16], npar = 0, i;
csrpos_t newpos;
boolean_t question = FALSE;
boolean_t angle = FALSE;
if (*cp == '?') {
question = TRUE;
cp++;
} else if (*cp == '<') {
angle = TRUE;
cp++;
}
for(i=0;i<=15;i++)
number[i] = MACH_ATOI_DEFAULT;
do {
cp += mach_atoi(cp, &number[npar]);
} while (*cp == ';' && ++npar <= 15 && cp++);
if (question) {
switch(*cp) {
case '\0':
break;
default:
if (*cp >= '@' && *cp <= '~')
{
}
else
{
kd_putc(*cp);
}
esc_spt = esc_seq;
break;
}
} else if (angle) {
switch(*cp) {
case '\0':
break;
default:
if (*cp >= '@' && *cp <= '~')
{
}
else
{
kd_putc(*cp);
}
esc_spt = esc_seq;
break;
}
} else {
switch(*cp) {
case 'm':
for (i=0;i<=npar;i++)
switch(number[i]) {
case MACH_ATOI_DEFAULT:
case 0:
kd_attrflags = 0;
kd_color = KA_NORMAL;
break;
case 1:
kd_attrflags |= KAX_BOLD;
kd_attrflags &= ~KAX_DIM;
break;
case 2:
kd_attrflags |= KAX_DIM;
kd_attrflags &= ~KAX_BOLD;
break;
case 4:
kd_attrflags |= KAX_UNDERLINE;
break;
case 5:
kd_attrflags |= KAX_BLINK;
break;
case 7:
kd_attrflags |= KAX_REVERSE;
break;
case 8:
kd_attrflags |= KAX_INVISIBLE;
break;
case 21:
case 22:
kd_attrflags &= ~(KAX_BOLD | KAX_DIM);
break;
case 24:
kd_attrflags &= ~KAX_UNDERLINE;
break;
case 25:
kd_attrflags &= ~KAX_BLINK;
break;
case 27:
kd_attrflags &= ~KAX_REVERSE;
break;
case 38:
kd_attrflags |= KAX_UNDERLINE;
kd_color = (kd_color & 0xf0) | (KA_NORMAL & 0x0f);
break;
case 39:
kd_attrflags &= ~KAX_UNDERLINE;
kd_color = (kd_color & 0xf0) | (KA_NORMAL & 0x0f);
break;
default:
if (number[i] >= 30 && number[i] <= 37) {
kd_color = (kd_color & 0xf0) | color_table[(number[i] - 30)];
} else if (number[i] >= 40 && number[i] <= 47) {
kd_color = (kd_color & 0x0f) | (color_table[(number[i] - 40)] << 4);
}
break;
}
kd_update_kd_attr();
esc_spt = esc_seq;
break;
case '@':
if (number[0] == MACH_ATOI_DEFAULT)
kd_insch(1);
else
kd_insch(number[0]);
esc_spt = esc_seq;
break;
case 'A':
if (number[0] == MACH_ATOI_DEFAULT)
kd_up();
else
while (number[0]--)
kd_up();
esc_spt = esc_seq;
break;
case 'B':
if (number[0] == MACH_ATOI_DEFAULT)
kd_down();
else
while (number[0]--)
kd_down();
esc_spt = esc_seq;
break;
case 'C':
if (number[0] == MACH_ATOI_DEFAULT)
kd_right();
else
while (number[0]--)
kd_right();
esc_spt = esc_seq;
break;
case 'D':
if (number[0] == MACH_ATOI_DEFAULT)
kd_left();
else
while (number[0]--)
kd_left();
esc_spt = esc_seq;
break;
case 'E':
kd_cr();
if (number[0] == MACH_ATOI_DEFAULT)
kd_down();
else
while (number[0]--)
kd_down();
esc_spt = esc_seq;
break;
case 'F':
kd_cr();
if (number[0] == MACH_ATOI_DEFAULT)
kd_up();
else
while (number[0]--)
kd_up();
esc_spt = esc_seq;
break;
case 'G':
if (number[0] == MACH_ATOI_DEFAULT)
number[0] = 0;
else
if (number[0] > 0)
--number[0];
kd_setpos(BEG_OF_LINE(kd_curpos) + number[0] * ONE_SPACE);
esc_spt = esc_seq;
break;
case 'f':
case 'H':
if (number[0] == MACH_ATOI_DEFAULT && number[1] == MACH_ATOI_DEFAULT)
{
kd_home();
esc_spt = esc_seq;
break;
}
if (number[0] == MACH_ATOI_DEFAULT)
number[0] = 0;
else if (number[0] > 0)
--number[0];
newpos = (number[0] * ONE_LINE);
if (number[1] == MACH_ATOI_DEFAULT)
number[1] = 0;
else if (number[1] > 0)
number[1]--;
newpos += (number[1] * ONE_SPACE);
if (newpos < 0)
newpos = 0;
if (newpos > ONE_PAGE)
newpos = (ONE_PAGE - ONE_SPACE);
kd_setpos(newpos);
esc_spt = esc_seq;
break;
case 'J':
switch(number[0]) {
case MACH_ATOI_DEFAULT:
case 0:
kd_cltobcur();
break;
case 1:
kd_cltopcur();
break;
case 2:
kd_cls();
break;
default:
break;
}
esc_spt = esc_seq;
break;
case 'K':
switch(number[0]) {
case MACH_ATOI_DEFAULT:
case 0:
kd_cltoecur();
break;
case 1:
kd_clfrbcur();
break;
case 2:
kd_eraseln();
break;
default:
break;
}
esc_spt = esc_seq;
break;
case 'L':
if (number[0] == MACH_ATOI_DEFAULT)
kd_insln(1);
else
kd_insln(number[0]);
esc_spt = esc_seq;
break;
case 'M':
if (number[0] == MACH_ATOI_DEFAULT)
kd_delln(1);
else
kd_delln(number[0]);
esc_spt = esc_seq;
break;
case 'P':
if (number[0] == MACH_ATOI_DEFAULT)
kd_delch(1);
else
kd_delch(number[0]);
esc_spt = esc_seq;
break;
case 'S':
if (number[0] == MACH_ATOI_DEFAULT)
kd_scrollup();
else
while (number[0]--)
kd_scrollup();
esc_spt = esc_seq;
break;
case 'T':
if (number[0] == MACH_ATOI_DEFAULT)
kd_scrolldn();
else
while (number[0]--)
kd_scrolldn();
esc_spt = esc_seq;
break;
case 'X':
if (number[0] == MACH_ATOI_DEFAULT)
kd_erase(1);
else
kd_erase(number[0]);
esc_spt = esc_seq;
break;
case '\0':
break;
default:
if (*cp >= '@' && *cp <= '~')
{
}
else
{
kd_putc(*cp);
}
esc_spt = esc_seq;
break;
}
}
return;
}
void
kd_tab(void)
{
int i;
for (i = 8 - (CURRENT_COLUMN(kd_curpos) % 8); i > 0; i--) {
kd_putc(' ');
}
}
void
kd_cls(void)
{
(*kd_dclear)(0, ONE_PAGE/ONE_SPACE, kd_attr);
return;
}
void
kd_home(void)
{
kd_setpos(0);
return;
}
void
kd_up(void)
{
if (kd_curpos < ONE_LINE)
kd_scrolldn();
else
kd_setpos(kd_curpos - ONE_LINE);
return;
}
void
kd_down(void)
{
if (kd_curpos >= (ONE_PAGE - ONE_LINE))
kd_scrollup();
else
kd_setpos(kd_curpos + ONE_LINE);
return;
}
void
kd_right(void)
{
if (kd_curpos < (ONE_PAGE - ONE_SPACE))
kd_setpos(kd_curpos + ONE_SPACE);
else {
kd_scrollup();
kd_setpos(BEG_OF_LINE(kd_curpos));
}
return;
}
void
kd_left(void)
{
if (0 < kd_curpos)
kd_setpos(kd_curpos - ONE_SPACE);
return;
}
void
kd_cr(void)
{
kd_setpos(BEG_OF_LINE(kd_curpos));
return;
}
void
kd_cltobcur(void)
{
csrpos_t start;
int	count;
start = kd_curpos;
count = (ONE_PAGE - kd_curpos)/ONE_SPACE;
(*kd_dclear)(start, count, kd_attr);
return;
}
void
kd_cltopcur(void)
{
int	count;
count = (kd_curpos + ONE_SPACE) / ONE_SPACE;
(*kd_dclear)(0, count, kd_attr);
return;
}
void
kd_cltoecur(void)
{
csrpos_t i;
csrpos_t hold;
hold = BEG_OF_LINE(kd_curpos) + ONE_LINE;
for (i = kd_curpos; i < hold; i += ONE_SPACE) {
(*kd_dput)(i, K_SPACE, kd_attr);
}
}
void
kd_clfrbcur(void)
{
csrpos_t i;
for (i = BEG_OF_LINE(kd_curpos); i <= kd_curpos; i += ONE_SPACE) {
(*kd_dput)(i, K_SPACE, kd_attr);
}
}
void
kd_delln(int number)
{
csrpos_t to;
csrpos_t from;
int	delbytes;
int	count;
if (number <= 0)
return;
delbytes = number * ONE_LINE;
to = BEG_OF_LINE(kd_curpos);
if (to + delbytes >= ONE_PAGE)
delbytes = ONE_PAGE - to;
if (to + delbytes < ONE_PAGE) {
from = to + delbytes;
count = (ONE_PAGE - from) / ONE_SPACE;
(*kd_dmvup)(from, to, count);
}
to = ONE_PAGE - delbytes;
count = delbytes / ONE_SPACE;
(*kd_dclear)(to, count, kd_attr);
return;
}
void
kd_insln(int number)
{
csrpos_t to;
csrpos_t from;
int	count;
csrpos_t top;
int	insbytes;
if (number <= 0)
return;
top = BEG_OF_LINE(kd_curpos);
insbytes = number * ONE_LINE;
if (top + insbytes > ONE_PAGE)
insbytes = ONE_PAGE - top;
to = ONE_PAGE - ONE_SPACE;
from = to - insbytes;
if (from > top) {
count = (from - top + ONE_SPACE) / ONE_SPACE;
(*kd_dmvdown)(from, to, count);
}
count = insbytes / ONE_SPACE;
(*kd_dclear)(top, count, kd_attr);
return;
}
void
kd_delch(int number)
{
int	 count;
int	 delbytes;
csrpos_t to;
csrpos_t from;
csrpos_t nextline;
if (number <= 0)
return;
nextline = BEG_OF_LINE(kd_curpos) + ONE_LINE;
delbytes = number * ONE_SPACE;
if (kd_curpos + delbytes > nextline)
delbytes = nextline - kd_curpos;
if (kd_curpos + delbytes < nextline) {
from = kd_curpos + delbytes;
to = kd_curpos;
count = (nextline - from) / ONE_SPACE;
(*kd_dmvup)(from, to, count);
}
to = nextline - delbytes;
count = delbytes / ONE_SPACE;
(*kd_dclear)(to, count, kd_attr);
return;
}
void
kd_erase(int number)
{
csrpos_t i;
csrpos_t stop;
stop = kd_curpos + (ONE_SPACE * number);
if (stop > BEG_OF_LINE(kd_curpos) + ONE_LINE)
stop = BEG_OF_LINE(kd_curpos) + ONE_LINE;
for (i = kd_curpos; i < stop; i += ONE_SPACE) {
(*kd_dput)(i, K_SPACE, kd_attr);
}
return;
}
void
kd_eraseln(void)
{
csrpos_t i;
csrpos_t stop;
stop = BEG_OF_LINE(kd_curpos) + ONE_LINE;
for (i = BEG_OF_LINE(kd_curpos); i < stop; i += ONE_SPACE) {
(*kd_dput)(i, K_SPACE, kd_attr);
}
return;
}
void
kd_insch(int number)
{
csrpos_t to;
csrpos_t from;
int	count;
csrpos_t nextline;
int	insbytes;
if (number <= 0)
return;
nextline = BEG_OF_LINE(kd_curpos) + ONE_LINE;
insbytes = number * ONE_SPACE;
if (kd_curpos + insbytes > nextline)
insbytes = nextline - kd_curpos;
to = nextline - ONE_SPACE;
from = to - insbytes;
if (from >= kd_curpos) {
count = (from - kd_curpos + ONE_SPACE) / ONE_SPACE;
(*kd_dmvdown)(from, to, count);
}
count = insbytes / ONE_SPACE;
(*kd_dclear)(kd_curpos, count, kd_attr);
return;
}
boolean_t
kd_isupper(u_char c)
{
if (('A' <= c) && (c <= 'Z'))
return(TRUE);
return(FALSE);
}
boolean_t
kd_islower(u_char c)
{
if (('a' <= c) && (c <= 'z'))
return(TRUE);
return(FALSE);
}
void
kd_senddata(unsigned char ch)
{
while (inb(K_STATUS) & K_IBUF_FUL)
;
outb(K_RDWR, ch);
last_sent = ch;
return;
}
void
kd_sendcmd(unsigned char ch)
{
while (inb(K_STATUS) & K_IBUF_FUL)
;
outb(K_CMD, ch);
return;
}
unsigned char
kd_getdata(void)
{
while ((inb(K_STATUS) & K_OBUF_FUL) == 0)
;
return(inb(K_RDWR));
}
void
kd_cmdreg_write(int val)
{
int ch=KC_CMD_WRITE;
while (inb(K_STATUS) & K_IBUF_FUL)
;
outb(K_CMD, ch);
while (inb(K_STATUS) & K_IBUF_FUL)
;
outb(K_RDWR, val);
}
void
kd_mouse_drain(void)
{
int i;
while(inb(K_STATUS) & K_IBUF_FUL)
;
while((i = inb(K_STATUS)) & K_OBUF_FUL)
printf("kbd: S = %x D = %x\n", i, inb(K_RDWR));
}
void
set_kd_state(int newstate)
{
kd_state = newstate;
kd_setleds1(state2leds(newstate));
}
u_char
state2leds(int state)
{
u_char result = 0;
if (state & KS_NLKED)
result |= K_LED_NUMLK;
if (state & KS_CLKED)
result |= K_LED_CAPSLK;
return(result);
}
void
kd_setleds1(u_char val)
{
if (kd_ack != NOT_WAITING) {
#ifdef MACH_KBD
printf("kd_setleds1: unexpected state (%d)\n", kd_ack);
#endif
return;
}
kd_ack = SET_LEDS;
kd_nextled = val;
kd_senddata(K_CMD_LEDS);
}
void
kd_setleds2(void)
{
kd_senddata(kd_nextled);
}
void
cnsetleds(u_char val)
{
kd_senddata(K_CMD_LEDS);
(void)kd_getdata();
kd_senddata(val);
(void)kd_getdata();
}
void
kdreboot(void)
{
(*kd_dreset)();
#ifndef BROKEN_KEYBOARD_RESET
kd_sendcmd(0xFE);
delay(1000000);
#endif
cpu_shutdown();
}
static int which_button[] = {0, MOUSE_LEFT, MOUSE_MIDDLE, MOUSE_RIGHT};
static struct mouse_motion moved;
int
kd_kbd_magic(int scancode)
{
int new_button = 0;
if (kd_kbd_mouse == 2)
printf("sc = %x\n", scancode);
switch (scancode) {
case 0x3d:
new_button++;
case 0x3c:
new_button++;
case 0x3b:
new_button++;
if (kd_kbd_magic_button && (new_button != kd_kbd_magic_button)) {
mouse_button(which_button[kd_kbd_magic_button], 1);
}
if (kd_kbd_magic_button == new_button) {
mouse_button(which_button[new_button], 1);
kd_kbd_magic_button = 0;
} else {
mouse_button(which_button[new_button], 0);
kd_kbd_magic_button = new_button;
}
break;
case 0x4d:
moved.mm_deltaX = kd_kbd_magic_scale;
moved.mm_deltaY = 0;
mouse_moved(moved);
break;
case 0x4b:
moved.mm_deltaX = -kd_kbd_magic_scale;
moved.mm_deltaY = 0;
mouse_moved(moved);
break;
case 0x48:
moved.mm_deltaX = 0;
moved.mm_deltaY = kd_kbd_magic_scale;
mouse_moved(moved);
break;
case 0x50:
moved.mm_deltaX = 0;
moved.mm_deltaY = -kd_kbd_magic_scale;
mouse_moved(moved);
break;
case 0x47:
moved.mm_deltaX = -2*kd_kbd_magic_scale;
moved.mm_deltaY = 2*kd_kbd_magic_scale;
mouse_moved(moved);
break;
case 0x49:
moved.mm_deltaX = 2*kd_kbd_magic_scale;
moved.mm_deltaY = 2*kd_kbd_magic_scale;
mouse_moved(moved);
break;
case 0x4f:
moved.mm_deltaX = -2*kd_kbd_magic_scale;
moved.mm_deltaY = -2*kd_kbd_magic_scale;
mouse_moved(moved);
break;
case 0x51:
moved.mm_deltaX = 2*kd_kbd_magic_scale;
moved.mm_deltaY = -2*kd_kbd_magic_scale;
mouse_moved(moved);
break;
default:
return 0;
}
return 1;
}
#define	SLAMBPW	2
static csrpos_t
xga_getpos(void)
{
unsigned char	low;
unsigned char	high;
short pos;
outb(kd_index_reg, C_HIGH);
high = inb(kd_io_reg);
outb(kd_index_reg, C_LOW);
low = inb(kd_io_reg);
pos = (0xff&low) + ((unsigned short)high<<8);
return(ONE_SPACE * (csrpos_t)pos);
}
void
kd_xga_init(void)
{
unsigned char	start, stop;
#if 0
unsigned char	screen;
outb(CMOS_ADDR, CMOS_EB);
screen = inb(CMOS_DATA) & CM_SCRMSK;
switch(screen) {
default:
printf("kd: unknown screen type, defaulting to EGA\n");
case CM_EGA_VGA:
#endif
vid_start = (u_char *)phystokv(EGA_START);
kd_index_reg = EGA_IDX_REG;
kd_io_reg = EGA_IO_REG;
kd_lines = 25;
kd_cols = 80;
kd_bitmap_start = 0xa0000;
{
char *addr = (char *)phystokv(kd_bitmap_start);
int i;
for (i = 0; i < 200; i++)
addr[i] = 0x00;
}
#if 0
break;
case CM_CGA_40:
vid_start = (u_char *)phystokv(CGA_START);
kd_index_reg = CGA_IDX_REG;
kd_io_reg = CGA_IO_REG;
kd_lines = 25;
kd_cols = 40;
break;
case CM_CGA_80:
vid_start = (u_char *)phystokv(CGA_START);
kd_index_reg = CGA_IDX_REG;
kd_io_reg = CGA_IO_REG;
kd_lines = 25;
kd_cols = 80;
break;
case CM_MONO_80:
vid_start = (u_char *)phystokv(MONO_START);
kd_index_reg = MONO_IDX_REG;
kd_io_reg = MONO_IO_REG;
kd_lines = 25;
kd_cols = 80;
break;
}
#endif
outb(kd_index_reg, C_START);
start = inb(kd_io_reg);
start &= ~0x20;
outb(kd_io_reg, start);
outb(kd_index_reg, C_STOP);
stop = inb(kd_io_reg);
if (!start && !stop)
{
outb(kd_index_reg, C_START);
outb(kd_io_reg, 14);
outb(kd_index_reg, C_STOP);
outb(kd_io_reg, 15);
}
kd_setpos(xga_getpos());
}
static void
charput(csrpos_t pos, char ch, char chattr)
{
*(vid_start + pos) = ch;
*(vid_start + pos + 1) = chattr;
}
static void
charsetcursor(csrpos_t newpos)
{
short curpos;
curpos = newpos / ONE_SPACE;
outb(kd_index_reg, C_HIGH);
outb(kd_io_reg, (u_char)(curpos>>8));
outb(kd_index_reg, C_LOW);
outb(kd_io_reg, (u_char)(curpos&0xff));
kd_curpos = newpos;
}
static void
charmvup(csrpos_t from, csrpos_t to, int count)
{
kd_slmscu(vid_start+from, vid_start+to, count);
}
static void
charmvdown(csrpos_t from, csrpos_t to, int count)
{
kd_slmscd(vid_start+from, vid_start+to, count);
}
static void
charclear(csrpos_t to, int count, char chattr)
{
kd_slmwd(vid_start+to, count, ((unsigned short)chattr<<8)+K_SPACE);
}
static void
kd_noopreset(void)
{
}
void
bmpput(
csrpos_t pos,
char	 ch,
char	 chattr)
{
short xbit, ybit;
u_char *to, *from;
short i, j;
u_char mask = (chattr == KA_REVERSE ? 0xff : 0);
if ((u_char)ch >= chars_in_font)
ch = K_QUES;
bmpch2bit(pos, &xbit, &ybit);
to = bit2fbptr(xbit, ybit);
from = font_start + ch * char_byte_width;
for (i = 0; i < char_height; ++i) {
for (j = 0; j < char_byte_width; ++j)
*(to+j) = *(from+j) ^ mask;
to += fb_byte_width;
from += font_byte_width;
}
}
static void
bmpcp1char(
csrpos_t from,
csrpos_t to)
{
short from_xbit, from_ybit;
short to_xbit, to_ybit;
u_char *tp, *fp;
short i, j;
bmpch2bit(from, &from_xbit, &from_ybit);
bmpch2bit(to, &to_xbit, &to_ybit);
tp = bit2fbptr(to_xbit, to_ybit);
fp = bit2fbptr(from_xbit, from_ybit);
for (i = 0; i < char_height; ++i) {
for (j = 0; j < char_byte_width; ++j)
*(tp+j) = *(fp+j);
tp += fb_byte_width;
fp += fb_byte_width;
}
}
void
bmpmvup(
csrpos_t 	from,
csrpos_t	to,
int		count)
{
short from_xbit, from_ybit;
short to_xbit, to_ybit;
short i;
bmpch2bit(from, &from_xbit, &from_ybit);
bmpch2bit(to, &to_xbit, &to_ybit);
if (from_xbit == xstart && to_xbit == xstart && count%kd_cols == 0) {
from_xbit = to_xbit = 0;
bmppaintcsr(kd_curpos, char_black);
count /= kd_cols;
count *= fb_byte_width * (char_height+cursor_height);
kd_slmscu(bit2fbptr(from_xbit, from_ybit),
bit2fbptr(to_xbit, to_ybit),
count/SLAMBPW);
bmppaintcsr(kd_curpos, char_white);
} else {
for (i=0; i < count; ++i) {
bmpcp1char(from, to);
from += ONE_SPACE;
to += ONE_SPACE;
}
}
}
void
bmpmvdown(
csrpos_t 	from,
csrpos_t	to,
int		count)
{
short from_xbit, from_ybit;
short to_xbit, to_ybit;
short i;
bmpch2bit(from, &from_xbit, &from_ybit);
bmpch2bit(to, &to_xbit, &to_ybit);
if (from_xbit == xstart + (kd_cols - 1) * char_width
&& to_xbit == xstart + (kd_cols - 1) * char_width
&& count%kd_cols == 0) {
from_xbit = to_xbit = 8 * (fb_byte_width - 1);
bmppaintcsr(kd_curpos, char_black);
count /= kd_cols;
count *= fb_byte_width * (char_height+cursor_height);
kd_slmscd(bit2fbptr(from_xbit, from_ybit),
bit2fbptr(to_xbit, to_ybit),
count/SLAMBPW);
bmppaintcsr(kd_curpos, char_white);
} else {
for (i=0; i < count; ++i) {
bmpcp1char(from, to);
from -= ONE_SPACE;
to -= ONE_SPACE;
}
}
}
void
bmpclear(
csrpos_t 	to,
int		count,
char		chattr)
{
short i;
u_short clearval;
u_short clearbyte = (chattr == KA_REVERSE ? char_white : char_black);
clearval = (u_short)(clearbyte<<8) + clearbyte;
if (to == 0 && count >= kd_lines * kd_cols) {
kd_slmwd(vid_start, (fb_byte_width * fb_height)/SLAMBPW,
clearval);
} else
for (i = 0; i < count; ++i) {
bmpput(to, K_SPACE, chattr);
to += ONE_SPACE;
}
}
void
bmpsetcursor(csrpos_t pos)
{
bmppaintcsr(kd_curpos, char_black);
bmppaintcsr(pos, char_white);
kd_curpos = pos;
}
void
bmppaintcsr(
csrpos_t 	pos,
u_char		val)
{
short xbit, ybit;
u_char *cp;
short line, byte;
bmpch2bit(pos, &xbit, &ybit);
ybit += char_height;
cp = bit2fbptr(xbit, ybit);
for (line = 0; line < cursor_height; ++line) {
for (byte = 0; byte < char_byte_width; ++byte)
*(cp+byte) = val;
cp += fb_byte_width;
}
}
void
bmpch2bit(
csrpos_t 	pos,
short		*xb,
short		*yb)
{
short xch, ych;
xch = (pos / ONE_SPACE) % kd_cols;
ych = pos / (ONE_SPACE * kd_cols);
*xb = xstart + xch * char_width;
*yb = ystart + ych * (char_height + cursor_height);
}
u_char *
bit2fbptr(
short	xb,
short	yb)
{
return(vid_start + yb * fb_byte_width + xb/8);
}
int
kdcnprobe(struct consdev *cp)
{
int maj, unit, pri;
maj = 0;
unit = 0;
pri = CN_INTERNAL;
cp->cn_dev = makedev(maj, unit);
cp->cn_pri = pri;
return 0;
}
int
kdcninit(struct consdev *cp)
{
kdinit();
return 0;
}
int
kdcngetc(dev_t dev, int wait)
{
if (wait) {
int c;
while ((c = kdcnmaygetc()) < 0)
continue;
return c;
}
else
return kdcnmaygetc();
}
int
kdcnputc(dev_t dev, int c)
{
if (!kd_initialized)
return -1;
if (c == '\n')
kd_putc('\r');
kd_putc_esc(c);
return 0;
}
int
kdcnmaygetc(void)
{
unsigned char	c;
unsigned char	scancode;
unsigned int	char_idx;
#ifdef	notdef
spl_t	o_pri;
#endif
boolean_t	up;
if (! kd_initialized)
return -1;
kd_extended = FALSE;
#ifdef	notdef
o_pri = splhi();
#endif
for ( ; ; ) {
if (!(inb(K_STATUS) & K_OBUF_FUL))
return -1;
up = FALSE;
if ((inb(K_STATUS) & 0x20) == 0x20) {
printf("M%xP", inb(K_RDWR));
continue;
}
scancode = inb(K_RDWR);
if (scancode == K_EXTEND) {
kd_extended = TRUE;
continue;
} else if (scancode == K_RESEND) {
printf("cngetc: resend");
kd_resend();
continue;
} else if (scancode == K_ACKSC) {
printf("cngetc: handle_ack");
kd_handle_ack();
continue;
}
if (scancode & K_UP) {
up = TRUE;
scancode &= ~K_UP;
}
if (kd_kbd_mouse)
kd_kbd_magic(scancode);
if (scancode < NUMKEYS) {
char_idx = kdstate2idx(kd_state, kd_extended);
c = key_map[scancode][char_idx];
if (c == K_SCAN) {
c = key_map[scancode][++char_idx];
kd_state = do_modifier(kd_state, c, up);
#ifdef notdef
cnsetleds(state2leds(kd_state));
#endif
} else if (! up
&& c == K_ESC
&& key_map[scancode][char_idx+1] == 0x5b) {
c = key_map[scancode][char_idx+2];
switch (c) {
#define _MAP(A,B,C)	(C)
#define MAP(T)		_MAP(T)
#define	CTRL(c)		((c) & 0x1f)
case MAP(K_HOME):	c = CTRL('a'); break;
case MAP(K_UA):		c = CTRL('p'); break;
case MAP(K_LA):		c = CTRL('b'); break;
case MAP(K_RA):		c = CTRL('f'); break;
case MAP(K_DA):		c = CTRL('n'); break;
case MAP(K_END):	c = CTRL('e'); break;
case 0x39:		c = CTRL('d'); break;
#undef CTRL
#undef MAP
#undef _MAP
default:
c = K_ESC;
}
return(c);
} else if (!up) {
if (c == K_CR)
c = K_LF;
#ifdef	notdef
splx(o_pri);
#endif
return(c & 0177);
}
}
}
}