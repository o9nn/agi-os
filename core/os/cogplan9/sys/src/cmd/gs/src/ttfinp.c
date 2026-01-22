#include "ttmisc.h"
#include "ttfoutl.h"
#include "ttfsfnt.h"
#include "ttfinp.h"
unsigned char ttfReader__Byte(ttfReader *r)
{   unsigned char b;
r->Read(r, &b, 1);
return b;
}
signed char ttfReader__SignedByte(ttfReader *r)
{   signed char b;
r->Read(r, &b, 1);
return b;
}
signed short ttfReader__Short(ttfReader *r)
{   unsigned char buf[2];
r->Read(r, buf, 2);
return ((int16)buf[0] << 8) | (int16)buf[1];
}
unsigned short ttfReader__UShort(ttfReader *r)
{   unsigned char buf[2];
r->Read(r, buf, 2);
return ((uint16)buf[0] << 8) | (uint16)buf[1];
}
unsigned int ttfReader__UInt(ttfReader *r)
{   unsigned char buf[4];
r->Read(r, buf, 4);
return ((int32)buf[0] << 24) | ((int32)buf[1] << 16) |
((int32)buf[2] <<  8) |  (int32)buf[3];
}
signed int ttfReader__Int(ttfReader *r)
{
return (int)ttfReader__UInt(r);
}