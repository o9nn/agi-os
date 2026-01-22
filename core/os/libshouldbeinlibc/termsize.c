#include <sys/ioctl.h>
int
deduce_term_size (int fd, char *type, int *width, int *height)
{
int w = 0, h = 0;
struct winsize ws;
if (fd >= 0 && ioctl (fd, TIOCGWINSZ, &ws) == 0)
{
w = ws.ws_col;
h = ws.ws_row;
}
if (((width && !w) || (height && !h)) && type)
{
}
if (width)
*width = w;
if (height)
*height = h;
return (!width || w) && (!height && h);
}