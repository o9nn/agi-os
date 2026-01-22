#include <openssl/rand.h>
#include "rand_lcl.h"
#if defined(OPENSSL_SYS_VMS)
# include <descrip.h>
# include <jpidef.h>
# include <ssdef.h>
# include <starlet.h>
# ifdef __DECC
# pragma message disable DOLLARID
# endif
# if __INITIAL_POINTER_SIZE == 64
# define PTR_T __void_ptr64
# pragma pointer_size save
# pragma pointer_size 32
# else
# define PTR_T void *
# endif
static struct items_data_st {
short length, code;
} items_data[] = {
{
4, JPI$_BUFIO
},
{
4, JPI$_CPUTIM
},
{
4, JPI$_DIRIO
},
{
8, JPI$_LOGINTIM
},
{
4, JPI$_PAGEFLTS
},
{
4, JPI$_PID
},
{
4, JPI$_WSSIZE
},
{
0, 0
}
};
int RAND_poll(void)
{
long pid, iosb[2];
int status = 0;
struct {
short length, code;
long *buffer;
int *retlen;
} item[32], *pitem;
unsigned char data_buffer[256];
short total_length = 0;
struct items_data_st *pitems_data;
pitems_data = items_data;
pitem = item;
while (pitems_data->length && (total_length + pitems_data->length <= 256)) {
pitem->length = pitems_data->length;
pitem->code = pitems_data->code;
pitem->buffer = (long *)&data_buffer[total_length];
pitem->retlen = 0;
total_length += pitems_data->length;
pitems_data++;
pitem ++;
}
pitem->length = pitem->code = 0;
pid = -1;
while ((status = sys$getjpiw(0, &pid, 0, item, iosb, 0, 0))
!= SS$_NOMOREPROC) {
if (status == SS$_NORMAL) {
RAND_add((PTR_T) data_buffer, total_length, total_length / 2);
}
}
sys$gettim(iosb);
RAND_add((PTR_T) iosb, sizeof(iosb), sizeof(iosb) / 2);
return 1;
}
#endif