#pragma src "/home/runner/work/echo.kern/echo.kern/vb9"
#pragma lib "libvb9.a"
typedef struct Rectangle Rectangle;
typedef struct Point Point;
typedef struct Control Control;
typedef struct VB9Form VB9Form;
typedef struct VB9Event VB9Event;
struct Point {
int x;
int y;
};
struct Rectangle {
Point min;
Point max;
};
enum {
VB9_BUTTON = 1,
VB9_TEXTBOX,
VB9_LABEL,
VB9_LISTBOX,
VB9_MAX_CONTROLS = 128
};
struct VB9Event {
char *name;
char *handler;
int (*execute)(Control*);
};
struct Control {
int type;
char name[64];
Rectangle rect;
char *text;
char *filepath;
VB9Event events[8];
void (*draw)(Control*);
void (*click)(Control*);
void (*change)(Control*);
};
struct VB9Form {
char name[64];
char *basepath;
int drawfd;
int ncontrols;
Control controls[VB9_MAX_CONTROLS];
void (*load)(VB9Form*);
void (*unload)(VB9Form*);
void (*refresh)(VB9Form*);
};
struct VB9Runtime {
VB9Form *forms;
int nforms;
int totalsize;
};
VB9Form* vb9_createform(char *name);
Control* vb9_addcontrol(VB9Form *form, int type, char *name, Rectangle rect);
void vb9_settext(Control *ctrl, char *text);
char* vb9_gettext(Control *ctrl);
void vb9_setevent(Control *ctrl, char *event, char *handler);
void vb9_show(VB9Form *form);
void vb9_compile(VB9Form *form, char *output);
void vb9_render(VB9Form *form);
void vb9_execute(VB9Form *form);
void vb9_exportfs(VB9Form *form);
void vb9_importfs(char *path);
void vb9_draw_button(Control *ctrl);
void vb9_draw_textbox(Control *ctrl);
void vb9_draw_label(Control *ctrl);
void vb9_draw_listbox(Control *ctrl);
void vb9_click_button(Control *ctrl);
void vb9_change_textbox(Control *ctrl);
void vb9_click_listbox(Control *ctrl);
char* readfile(char *path);
char* vb9_typename(int type);
enum {
VB9_MAX_FORMS = 16,
VB9_MAX_FILESIZE = 1400000,
VB9_MAX_PATH = 256
};