#ifndef _KDSOFT_H_
#define _KDSOFT_H_
typedef	short	csrpos_t;
extern u_char 	*vid_start;
extern csrpos_t kd_curpos;
extern short	kd_lines;
extern short	kd_cols;
extern char	kd_attr;
extern void bmpput(csrpos_t, char, char);
extern void bmpmvup(csrpos_t, csrpos_t, int);
extern void bmpmvdown(csrpos_t, csrpos_t, int);
extern void bmpclear(csrpos_t, int, char);
extern void bmpsetcursor(csrpos_t);
extern void	(*kd_dput)(csrpos_t, char, char);
extern void	(*kd_dmvup)(csrpos_t, csrpos_t, int);
extern void	(*kd_dmvdown)(csrpos_t, csrpos_t, int);
extern void	(*kd_dclear)(csrpos_t, int, char);
extern void	(*kd_dsetcursor)(csrpos_t);
extern void	(*kd_dreset)(void);
extern u_char	*font_start;
extern short	fb_width;
extern short	fb_height;
extern short	char_width;
extern short	char_height;
extern short	chars_in_font;
extern short	cursor_height;
extern u_char	char_black;
extern u_char	char_white;
extern short	xstart, ystart;
extern short	char_byte_width;
extern short	fb_byte_width;
extern short	font_byte_width;
#endif