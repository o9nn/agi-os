#include "fshelp.h"
int
fshelp_translated (struct transbox *box)
{
return (box->active != MACH_PORT_NULL);
}