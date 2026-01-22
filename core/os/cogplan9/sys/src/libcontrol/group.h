typedef struct Group Group;
struct Group {
Control;
int lastbut;
int border;
int mansize;
int separation;
int selected;
int lastkid;
CImage *bordercolor;
CImage *image;
int nkids;
Control **kids;
Rectangle *separators;
int nseparators;
};