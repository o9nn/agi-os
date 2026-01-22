#ifndef _KD_H_
#define _KD_H_
#include <device/input.h>
#include <mach/boolean.h>
#include <sys/types.h>
#include <device/cons.h>
#include <device/io_req.h>
#include <device/buf.h>
#include <device/input.h>
#include <device/tty.h>
#include <i386at/kdsoft.h>
#define EGA_START 0x0b8000
#define CGA_START 0x0b8000
#define MONO_START 0x0b0000
#define K_TMR0 0x40
#define K_TMR1 0x41
#define K_TMR2 0x42
#define K_TMRCTL 0x43
#define K_RDWR 0x60
#define K_PORTB 0x61
#define K_STATUS 0x64
#define K_CMD 0x64
#define EGA_IDX_REG 0x3d4
#define EGA_IO_REG 0x3d5
#define CGA_IDX_REG 0x3d4
#define CGA_IO_REG 0x3d5
#define MONO_IDX_REG 0x3b4
#define MONO_IO_REG 0x3b5
#define C_START 0x0a
#define C_STOP 0x0b
#define C_LOW 0x0f
#define C_HIGH 0x0e
#define K_OBUF_FUL 0x01
#define K_IBUF_FUL 0x02
#define K_SYSFLAG 0x04
#define K_CMD_DATA 0x08
#define K_KBD_INHBT 0x10
#define KC_CMD_READ 0x20
#define KC_CMD_WRITE 0x60
#define KC_CMD_TEST 0xab
#define KC_CMD_DUMP 0xac
#define KC_CMD_DISBLE 0xad
#define KC_CMD_ENBLE 0xae
#define KC_CMD_RDKBD 0xc4
#define KC_CMD_ECHO 0xee
#define K_CMD_LEDS 0xed
#define K_CB_ENBLIRQ 0x01
#define K_CB_SETSYSF 0x04
#define K_CB_INHBOVR 0x08
#define K_CB_DISBLE 0x10
#define K_LED_SCRLLK 0x1
#define K_LED_NUMLK 0x2
#define K_LED_CAPSLK 0x4
#define K_ENABLETMR2 0x01
#define K_SPKRDATA 0x02
#define K_ENABLEPRTB 0x04
#define K_EIOPRTB 0x08
#define K_REFRESHB 0x10
#define K_OUT2B 0x20
#define K_ICKB 0x40
#define K_SELTMRMASK 0xc0
#define K_SELTMR0 0x00
#define K_SELTMR1 0x40
#define K_SELTMR2 0x80
#define K_RDLDTMRMASK 0x30
#define K_HOLDTMR 0x00
#define K_RDLDTLSB 0x10
#define K_RDLDTMSB 0x20
#define K_RDLDTWORD 0x30
#define K_TMDCTLMASK 0x0e
#define K_TCOUNTINTR 0x00
#define K_TONESHOT 0x02
#define K_TRATEGEN 0x04
#define K_TSQRWAVE 0x06
#define K_TSOFTSTRB 0x08
#define K_THARDSTRB 0x0a
#define K_TCNTMDMASK 0x01
#define K_TBINARY 0x00
#define K_TBCD 0x01
#define KA_NORMAL 0x07
#define KA_REVERSE 0x70
#define KAX_REVERSE 0x01
#define KAX_UNDERLINE 0x02
#define KAX_BLINK 0x04
#define KAX_BOLD 0x08
#define KAX_DIM 0x10
#define KAX_INVISIBLE 0x20
#define KAX_COL_UNDERLINE 0x0f
#define KAX_COL_DIM 0x08
#define ONE_SPACE 2
#define BOTTOM_LINE 3840
#define ONE_PAGE 4000
#define ONE_LINE 160
#define BEG_OF_LINE(pos) ((pos) - (pos)%ONE_LINE)
#define CURRENT_COLUMN(pos) (((pos) % ONE_LINE) / ONE_SPACE)
#define NUMKEYS 89
#define NUMSTATES 5
#define NUMOUTPUT 3
#define WIDTH_KMAP (NUMSTATES * NUMOUTPUT)
#define NORM_STATE 0
#define SHIFT_STATE 1
#define CTRL_STATE 2
#define ALT_STATE 3
#define SHIFT_ALT 4
#define CHARIDX(sidx) ((sidx) * NUMOUTPUT)
#define KS_NORMAL 0x00
#define KS_SLKED 0x01
#define KS_NLKED 0x02
#define KS_CLKED 0x04
#define KS_ALTED 0x08
#define KS_SHIFTED 0x10
#define KS_CTLED 0x20
#define K_UP 0x80
#define K_EXTEND 0xe0
#define K_ACKSC 0xfa
#define K_RESEND 0xfe
#define K_CTLSC 0x1d
#define K_LSHSC 0x2a
#define K_RSHSC 0x36
#define K_ALTSC 0x38
#define K_CLCKSC 0x3a
#define K_NLCKSC 0x45
#define K_BSSC 0x0e
#define K_TABSC 0x0f
#define K_RETSC 0x1c
#define K_SPSC 0x39
#define K_ESCSC 0x01
#define K_qSC 0x10
#define K_wSC 0x11
#define K_eSC 0x12
#define K_rSC 0x13
#define K_tSC 0x14
#define K_ySC 0x15
#define K_uSC 0x16
#define K_iSC 0x17
#define K_oSC 0x18
#define K_pSC 0x19
#define K_aSC 0x1e
#define K_sSC 0x1f
#define K_dSC 0x20
#define K_fSC 0x21
#define K_gSC 0x22
#define K_hSC 0x23
#define K_jSC 0x24
#define K_kSC 0x25
#define K_lSC 0x26
#define K_zSC 0x2c
#define K_xSC 0x2d
#define K_cSC 0x2e
#define K_vSC 0x2f
#define K_bSC 0x30
#define K_nSC 0x31
#define K_mSC 0x32
#define K_ONESC 0x02
#define K_TWOSC 0x03
#define K_THREESC 0x04
#define K_FOURSC 0x05
#define K_FIVESC 0x06
#define K_SIXSC 0x07
#define K_SEVENSC 0x08
#define K_EIGHTSC 0x09
#define K_NINESC 0x0a
#define K_ZEROSC 0x0b
#define K_MINUSSC 0x0c
#define K_EQLSC 0x0d
#define K_LBRKTSC 0x1a
#define K_RBRKTSC 0x1b
#define K_SEMISC 0x27
#define K_SQUOTESC 0x28
#define K_GRAVSC 0x29
#define K_BSLSHSC 0x2b
#define K_COMMASC 0x33
#define K_PERIODSC 0x34
#define K_SLASHSC 0x35
#define K_HOMESC 0x47
#define K_DELSC 0x53
#define K_DONE 0xffu
#define NC 0xffu
#define K_SCAN 0xfeu
#define K_NUL 0x00
#define K_SOH 0x01
#define K_STX 0x02
#define K_ETX 0x03
#define K_EOT 0x04
#define K_ENQ 0x05
#define K_ACK 0x06
#define K_BEL 0x07
#define K_BS 0x08
#define K_HT 0x09
#define K_LF 0x0a
#define K_VT 0x0b
#define K_FF 0x0c
#define K_CR 0x0d
#define K_SO 0x0e
#define K_SI 0x0f
#define K_DLE 0x10
#define K_DC1 0x11
#define K_DC2 0x12
#define K_DC3 0x13
#define K_DC4 0x14
#define K_NAK 0x15
#define K_SYN 0x16
#define K_ETB 0x17
#define K_CAN 0x18
#define K_EM 0x19
#define K_SUB 0x1a
#define K_ESC 0x1b
#define K_FS 0x1c
#define K_GS 0x1d
#define K_RS 0x1e
#define K_US 0x1f
#define K_SPACE 0x20
#define K_BANG 0x21
#define K_DQUOTE 0x22
#define K_POUND 0x23
#define K_DOLLAR 0x24
#define K_PERC 0x25
#define K_AMPER 0x26
#define K_SQUOTE 0x27
#define K_LPAREN 0x28
#define K_RPAREN 0x29
#define K_ASTER 0x2a
#define K_PLUS 0x2b
#define K_COMMA 0x2c
#define K_MINUS 0x2d
#define K_PERIOD 0x2e
#define K_SLASH 0x2f
#define K_ZERO 0x30
#define K_ONE 0x31
#define K_TWO 0x32
#define K_THREE 0x33
#define K_FOUR 0x34
#define K_FIVE 0x35
#define K_SIX 0x36
#define K_SEVEN 0x37
#define K_EIGHT 0x38
#define K_NINE 0x39
#define K_COLON 0x3a
#define K_SEMI 0x3b
#define K_LTHN 0x3c
#define K_EQL 0x3d
#define K_GTHN 0x3e
#define K_QUES 0x3f
#define K_ATSN 0x40
#define K_A 0x41
#define K_B 0x42
#define K_C 0x43
#define K_D 0x44
#define K_E 0x45
#define K_F 0x46
#define K_G 0x47
#define K_H 0x48
#define K_I 0x49
#define K_J 0x4a
#define K_K 0x4b
#define K_L 0x4c
#define K_M 0x4d
#define K_N 0x4e
#define K_O 0x4f
#define K_P 0x50
#define K_Q 0x51
#define K_R 0x52
#define K_S 0x53
#define K_T 0x54
#define K_U 0x55
#define K_V 0x56
#define K_W 0x57
#define K_X 0x58
#define K_Y 0x59
#define K_Z 0x5a
#define K_LBRKT 0x5b
#define K_BSLSH 0x5c
#define K_RBRKT 0x5d
#define K_CARET 0x5e
#define K_UNDSC 0x5f
#define K_GRAV 0x60
#define K_a 0x61
#define K_b 0x62
#define K_c 0x63
#define K_d 0x64
#define K_e 0x65
#define K_f 0x66
#define K_g 0x67
#define K_h 0x68
#define K_i 0x69
#define K_j 0x6a
#define K_k 0x6b
#define K_l 0x6c
#define K_m 0x6d
#define K_n 0x6e
#define K_o 0x6f
#define K_p 0x70
#define K_q 0x71
#define K_r 0x72
#define K_s 0x73
#define K_t 0x74
#define K_u 0x75
#define K_v 0x76
#define K_w 0x77
#define K_x 0x78
#define K_y 0x79
#define K_z 0x7a
#define K_LBRACE 0x7b
#define K_PIPE 0x7c
#define K_RBRACE 0x7d
#define K_TILDE 0x7e
#define K_DEL 0x7f
#define K_F1 0x1b,0x4f,0x50
#define K_F1S 0x1b,0x4f,0x70
#define K_F2 0x1b,0x4f,0x51
#define K_F2S 0x1b,0x4f,0x71
#define K_F3 0x1b,0x4f,0x52
#define K_F3S 0x1b,0x4f,0x72
#define K_F4 0x1b,0x4f,0x53
#define K_F4S 0x1b,0x4f,0x73
#define K_F5 0x1b,0x4f,0x54
#define K_F5S 0x1b,0x4f,0x74
#define K_F6 0x1b,0x4f,0x55
#define K_F6S 0x1b,0x4f,0x75
#define K_F7 0x1b,0x4f,0x56
#define K_F7S 0x1b,0x4f,0x76
#define K_F8 0x1b,0x4f,0x57
#define K_F8S 0x1b,0x4f,0x77
#define K_F9 0x1b,0x4f,0x58
#define K_F9S 0x1b,0x4f,0x78
#define K_F10 0x1b,0x4f,0x59
#define K_F10S 0x1b,0x4f,0x79
#define K_F11 0x1b,0x4f,0x5a
#define K_F11S 0x1b,0x4f,0x7a
#define K_F12 0x1b,0x4f,0x41
#define K_F12S 0x1b,0x4f,0x61
#define K_F1A 0x1b,0x4f,0x30
#define K_F2A 0x1b,0x4f,0x31
#define K_F3A 0x1b,0x4f,0x32
#define K_F4A 0x1b,0x4f,0x33
#define K_F5A 0x1b,0x4f,0x34
#define K_F6A 0x1b,0x4f,0x35
#define K_F7A 0x1b,0x4f,0x36
#define K_F8A 0x1b,0x4f,0x37
#define K_F9A 0x1b,0x4f,0x38
#define K_F10A 0x1b,0x4f,0x39
#define K_F11A 0x1b,0x4f,0x3a
#define K_F12A 0x1b,0x4f,0x3b
#define K_SCRL 0x1b,0x5b,0x4d
#define K_HOME 0x1b,0x5b,0x48
#define K_UA 0x1b,0x5b,0x41
#define K_PUP 0x1b,0x5b,0x56
#define K_LA 0x1b,0x5b,0x44
#define K_RA 0x1b,0x5b,0x43
#define K_END 0x1b,0x5b,0x59
#define K_DA 0x1b,0x5b,0x42
#define K_PDN 0x1b,0x5b,0x55
#define K_INS 0x1b,0x5b,0x40
#define KBD_IRQ 1
#ifdef KERNEL
extern u_char key_map[NUMKEYS][WIDTH_KMAP];
#endif
#ifdef KERNEL
#include <i386/spl.h>
#define SPLKD spltty
#endif
#define KDGKBENT _IOWR('k', 1, struct kbentry)
#define KDSKBENT _IOW('k', 2, struct kbentry)
#define KDGSTATE _IOR('k', 3, int)
#define KDSETBELL _IOW('k', 4, int)
# define KD_BELLON 1
# define KD_BELLOFF 0
struct kbentry {
u_char kb_state;
u_char kb_index;
u_char kb_value[NUMOUTPUT];
};
#ifdef KERNEL
extern int kb_mode;
#endif
struct X_kdb {
u_int *ptr;
u_int size;
};
#define K_X_KDB_ENTER _IOW('K', 16, struct X_kdb)
#define K_X_KDB_EXIT _IOW('K', 17, struct X_kdb)
#define K_X_IN 0x01000000
#define K_X_OUT 0x02000000
#define K_X_BYTE 0x00010000
#define K_X_WORD 0x00020000
#define K_X_LONG 0x00040000
#define K_X_TYPE 0x03070000
#define K_X_PORT 0x0000ffff
extern boolean_t kd_isupper (u_char);
extern boolean_t kd_islower (u_char);
extern void kd_senddata (unsigned char);
extern void kd_sendcmd (unsigned char);
extern void kd_cmdreg_write (int);
extern void kd_mouse_drain (void);
extern void set_kd_state (int);
extern void kd_setleds1 (u_char);
extern void kd_setleds2 (void);
extern void cnsetleds (u_char);
extern void kdreboot (void);
extern void kd_putc_esc (u_char);
extern void kd_putc (u_char);
extern void kd_parseesc (void);
extern void kd_down (void);
extern void kd_up (void);
extern void kd_cr (void);
extern void kd_tab (void);
extern void kd_left (void);
extern void kd_right (void);
extern void kd_scrollup (void);
extern void kd_scrolldn (void);
extern void kd_cls (void);
extern void kd_home (void);
extern void kd_insch (int number);
extern void kd_cltobcur (void);
extern void kd_cltopcur (void);
extern void kd_cltoecur (void);
extern void kd_clfrbcur (void);
extern void kd_eraseln (void);
extern void kd_insln (int);
extern void kd_delln (int);
extern void kd_delch (int);
extern void kd_erase (int);
extern void kd_bellon (void);
extern void kd_belloff (void *param);
extern void kdinit (void);
extern int kdsetkbent (struct kbentry *, int);
extern int kdgetkbent (struct kbentry *);
extern int kdsetbell (int, int);
extern void kd_resend (void);
extern void kd_handle_ack (void);
extern int kd_kbd_magic (int);
extern unsigned int kdstate2idx (unsigned int, boolean_t);
extern void kd_parserest (u_char *);
extern int kdcnprobe(struct consdev *cp);
extern int kdcninit(struct consdev *cp);
extern int kdcngetc(dev_t dev, int wait);
extern int kdcnmaygetc (void);
extern int kdcnputc(dev_t dev, int c);
extern void kd_setpos(csrpos_t newpos);
extern void kd_slmwd (void *start, int count, int value);
extern void kd_slmscu (void *from, void *to, int count);
extern void kd_slmscd (void *from, void *to, int count);
extern void kdintr(int vec);
#if MACH_KDB
#include <ddb/db_input.h>
#endif
extern int kdopen(dev_t dev, int flag, io_req_t ior);
extern void kdclose(dev_t dev, int flag);
extern int kdread(dev_t dev, io_req_t uio);
extern int kdwrite(dev_t dev, io_req_t uio);
extern io_return_t kdgetstat(
dev_t dev,
dev_flavor_t flavor,
dev_status_t data,
mach_msg_type_number_t *count);
extern io_return_t kdsetstat(
dev_t dev,
dev_flavor_t flavor,
dev_status_t data,
mach_msg_type_number_t count);
extern int kdportdeath(dev_t dev, mach_port_t port);
extern vm_offset_t kdmmap(dev_t dev, vm_offset_t off, vm_prot_t prot);
boolean_t kdcheckmagic(Scancode scancode);
int do_modifier(int state, Scancode c, boolean_t up);
void bmpch2bit(csrpos_t pos, short *xb, short *yb);
void bmppaintcsr(csrpos_t pos, u_char val);
u_char *bit2fbptr(short xb, short yb);
unsigned char kd_getdata(void);
unsigned char state2leds(int state);
void kdstart(struct tty *tp);
void kdstop(struct tty *tp, int flags);
void kd_xga_init(void);
#endif