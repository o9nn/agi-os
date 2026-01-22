#include <string.h>
#include <kern/debug.h>
#include <sys/types.h>
#include <device/conf.h>
#include <mach/boolean.h>
#include <device/cons.h>
#ifdef MACH_KMSG
#include <device/io_req.h>
#include <device/kmsg.h>
#endif
static	boolean_t cn_inited = FALSE;
static	struct consdev *cn_tab = 0;
int	(*romgetc)(char c) = 0;
void	(*romputc)(char c) = 0;
#if CONSBUFSIZE > 0
static	char consbuf[CONSBUFSIZE] = { 0 };
static	char *consbp = consbuf;
static	boolean_t consbufused = FALSE;
#endif
void
cninit(void)
{
struct consdev *cp;
dev_ops_t cn_ops;
int x;
if (cn_inited)
return;
for (cp = constab; cp->cn_probe; cp++) {
(*cp->cn_probe)(cp);
if (cp->cn_pri > CN_DEAD &&
(cn_tab == NULL || cp->cn_pri > cn_tab->cn_pri))
cn_tab = cp;
}
if ((cp = cn_tab)) {
(*cp->cn_init)(cp);
if (dev_name_lookup(cp->cn_name, &cn_ops, &x) == FALSE)
panic("cninit: dev_name_lookup failed");
dev_set_indirection("console", cn_ops, minor(cp->cn_dev));
#if CONSBUFSIZE > 0
if (consbufused) {
char *cbp = consbp;
do {
if (*cbp)
cnputc(*cbp);
if (++cbp == &consbuf[CONSBUFSIZE])
cbp = consbuf;
} while (cbp != consbp);
consbufused = FALSE;
}
#endif
cn_inited = TRUE;
return;
}
panic("can't find a console device");
}
int
cngetc(void)
{
if (cn_tab)
return ((*cn_tab->cn_getc)(cn_tab->cn_dev, 1));
if (romgetc)
return ((*romgetc)(1));
return (0);
}
int
cnmaygetc(void)
{
if (cn_tab)
return((*cn_tab->cn_getc)(cn_tab->cn_dev, 0));
if (romgetc)
return ((*romgetc)(0));
return (0);
}
void
cnputc(char c)
{
if (c == 0)
return;
#ifdef MACH_KMSG
kmsg_putchar (c);
#endif
#if defined(MACH_HYP) && 0
{
unsigned char d = c;
hyp_console_write(&d, 1);
}
#endif
if (cn_tab) {
(*cn_tab->cn_putc)(cn_tab->cn_dev, c);
if (c == '\n')
(*cn_tab->cn_putc)(cn_tab->cn_dev, '\r');
} else if (romputc) {
(*romputc)(c);
if (c == '\n')
(*romputc)('\r');
}
#if CONSBUFSIZE > 0
else {
if (consbufused == FALSE) {
consbp = consbuf;
consbufused = TRUE;
memset(consbuf, 0, CONSBUFSIZE);
}
*consbp++ = c;
if (consbp >= &consbuf[CONSBUFSIZE])
consbp = consbuf;
}
#endif
}