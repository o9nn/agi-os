#ifndef gsdllwin_INCLUDED
# define gsdllwin_INCLUDED
GSDLLEXPORT HGLOBAL GSDLLAPI gsdll_copy_dib(unsigned char * device);
GSDLLEXPORT HPALETTE GSDLLAPI gsdll_copy_palette(unsigned char * device);
GSDLLEXPORT void GSDLLAPI gsdll_draw(unsigned char * device, HDC hdc, LPRECT dest,
LPRECT src);
GSDLLEXPORT int GSDLLAPI gsdll_get_bitmap_row(unsigned char *device,
LPBITMAPINFOHEADER pbmih,
LPRGBQUAD prgbquad, LPBYTE * ppbyte,
unsigned int row);
typedef HGLOBAL (GSDLLAPI * PFN_gsdll_copy_dib)(unsigned char *);
typedef HPALETTE (GSDLLAPI * PFN_gsdll_copy_palette)(unsigned char *);
typedef void (GSDLLAPI * PFN_gsdll_draw) (unsigned char *, HDC, LPRECT,
LPRECT);
typedef int (GSDLLAPI * PFN_gsdll_get_bitmap_row)
(unsigned char *device, LPBITMAPINFOHEADER pbmih, LPRGBQUAD prgbquad,
LPBYTE * ppbyte, unsigned int row);
#endif