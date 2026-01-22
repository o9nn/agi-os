#ifndef dwtext_INCLUDED
# define dwtext_INCLUDED
#ifdef _WINDOWS
#define _Windows
#endif
typedef struct TEXTWINDOW_S {
const char *Title;
HICON hIcon;
BYTE *ScreenBuffer;
POINT ScreenSize;
char *DragPre;
char *DragPost;
int nCmdShow;
HWND hwnd;
BYTE *KeyBuf;
BYTE *KeyBufIn;
BYTE *KeyBufOut;
unsigned int KeyBufSize;
BOOL quitnow;
char line_buf[256];
int line_end;
int line_start;
BOOL line_complete;
BOOL line_eof;
BOOL bFocus;
BOOL bGetCh;
char *fontname;
int fontsize;
HFONT hfont;
int CharAscent;
int CaretHeight;
int CursorFlag;
POINT CursorPos;
POINT ClientSize;
POINT CharSize;
POINT ScrollPos;
POINT ScrollMax;
int x, y, cx, cy;
} TW;
TW *text_new(void);
void text_destroy(TW *tw);
int text_kbhit(TW *tw);
int getch(void);
int text_gets(TW *tw, char *buf, int len);
int text_read_line(TW *tw, char *buf, int len);
int text_putch(TW *tw, int ch);
void text_write_buf(TW *tw, const char *buf, int cnt);
void text_puts(TW *tw, const char *str);
void text_to_cursor(TW *tw);
int text_register_class(TW *tw, HICON hicon);
int text_create(TW *tw, const char *title, int cmdShow);
void text_font(TW *tw, const char *fontname, int fontsize);
void text_size(TW *tw, int width, int height);
void text_setpos(TW *tw, int x, int y, int cx, int cy);
int text_getpos(TW *tw, int *px, int *py, int *pcx, int *pcy);
void text_drag(TW *tw, const char *pre_drag, const char *post_drag);
HWND text_get_handle(TW *tw);
#endif