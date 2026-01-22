#ifndef _VGA_DYNACOLOR_H_
#define _VGA_DYNACOLOR_H_ 1
#define DYNACOLOR_COMPONENT_MAX 0x63
#define DYNACOLOR_COMPONENTS 3
#define DYNACOLOR_COLORS 8
struct dynacolor
{
int ref[8];
signed char col[16];
};
typedef struct dynacolor dynacolor_t;
#define DYNACOLOR_INIT_8 { { 1, 0, 0, 0, 0, 0, 0, 0 }, \
{ 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 } }
#define DYNACOLOR_INIT_16 { { -1 } }
extern dynacolor_t dynacolor_init_8;
extern dynacolor_t dynacolor_init_16;
void dynacolor_init (void);
void dynacolor_fini (void);
void dynacolor_activate (dynacolor_t *dc);
signed char dynacolor_allocate (dynacolor_t *dc, unsigned char col);
#define dynacolor_lookup(dc,c) \
((dc).ref[0] < 0 ? (c) : \
((dc).col[(c)] >= 0 ? (dc).ref[(dc).col[(c)]]++, (dc).col[(c)] : \
dynacolor_allocate (&(dc), (c))))
#define dynacolor_add_ref(dc,p) \
do { \
if ((dc).ref[0] >= 0) \
(dc).ref[p]++; \
} while (0)
#define dynacolor_release(dc,p) \
do { \
if ((dc).ref[0] >= 0) \
(dc).ref[p]--; \
} while (0)
void dynacolor_replace_colors (dynacolor_t *dc,
signed char fgcol, signed char bgcol,
signed char *r_fgcol, signed char *r_bgcol);
#endif