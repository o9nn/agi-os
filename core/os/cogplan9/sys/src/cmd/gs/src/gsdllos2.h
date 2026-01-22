#ifndef gsdllos2_INCLUDED
# define gsdllos2_INCLUDED
unsigned long gsdll_get_bitmap(unsigned char *device, unsigned char **pbitmap);
typedef long (*GSDLLAPI PFN_gsdll_get_bitmap) (unsigned char *, unsigned char **);
#endif