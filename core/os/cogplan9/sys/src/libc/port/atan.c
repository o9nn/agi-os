#include <u.h>
#include <libc.h>
#define sq2p1 2.414213562373095048802e0
#define sq2m1 .414213562373095048802e0
#define p4 .161536412982230228262e2
#define p3 .26842548195503973794141e3
#define p2 .11530293515404850115428136e4
#define p1 .178040631643319697105464587e4
#define p0 .89678597403663861959987488e3
#define q4 .5895697050844462222791e2
#define q3 .536265374031215315104235e3
#define q2 .16667838148816337184521798e4
#define q1 .207933497444540981287275926e4
#define q0 .89678597403663861962481162e3
static
double
xatan(double arg)
{
double argsq, value;
argsq = arg*arg;
value = ((((p4*argsq + p3)*argsq + p2)*argsq + p1)*argsq + p0);
value = value/(((((argsq + q4)*argsq + q3)*argsq + q2)*argsq + q1)*argsq + q0);
return value*arg;
}
static
double
satan(double arg)
{
if(arg < sq2m1)
return xatan(arg);
if(arg > sq2p1)
return PIO2 - xatan(1/arg);
return PIO2/2 + xatan((arg-1)/(arg+1));
}
double
atan(double arg)
{
if(arg > 0)
return satan(arg);
return -satan(-arg);
}