#include <u.h>
#include <libc.h>
#include <draw.h>
#include <geometry.h>
Point3 xformpoint(Point3 p, Space *to, Space *from){
Point3 q, r;
register double *m;
if(from){
m=&from->t[0][0];
q.x=*m++*p.x; q.x+=*m++*p.y; q.x+=*m++*p.z; q.x+=*m++*p.w;
q.y=*m++*p.x; q.y+=*m++*p.y; q.y+=*m++*p.z; q.y+=*m++*p.w;
q.z=*m++*p.x; q.z+=*m++*p.y; q.z+=*m++*p.z; q.z+=*m++*p.w;
q.w=*m++*p.x; q.w+=*m++*p.y; q.w+=*m++*p.z; q.w+=*m  *p.w;
}
else
q=p;
if(to){
m=&to->tinv[0][0];
r.x=*m++*q.x; r.x+=*m++*q.y; r.x+=*m++*q.z; r.x+=*m++*q.w;
r.y=*m++*q.x; r.y+=*m++*q.y; r.y+=*m++*q.z; r.y+=*m++*q.w;
r.z=*m++*q.x; r.z+=*m++*q.y; r.z+=*m++*q.z; r.z+=*m++*q.w;
r.w=*m++*q.x; r.w+=*m++*q.y; r.w+=*m++*q.z; r.w+=*m  *q.w;
}
else
r=q;
return r;
}
Point3 xformpointd(Point3 p, Space *to, Space *from){
p=xformpoint(p, to, from);
if(p.w!=0){
p.x/=p.w;
p.y/=p.w;
p.z/=p.w;
p.w=1;
}
return p;
}
Point3 xformplane(Point3 p, Space *to, Space *from){
Point3 q, r;
register double *m;
if(from){
m=&from->tinv[0][0];
q.x =*m++*p.x; q.y =*m++*p.x; q.z =*m++*p.x; q.w =*m++*p.x;
q.x+=*m++*p.y; q.y+=*m++*p.y; q.z+=*m++*p.y; q.w+=*m++*p.y;
q.x+=*m++*p.z; q.y+=*m++*p.z; q.z+=*m++*p.z; q.w+=*m++*p.z;
q.x+=*m++*p.w; q.y+=*m++*p.w; q.z+=*m++*p.w; q.w+=*m  *p.w;
}
else
q=p;
if(to){
m=&to->t[0][0];
r.x =*m++*q.x; r.y =*m++*q.x; r.z =*m++*q.x; r.w =*m++*q.x;
r.x+=*m++*q.y; r.y+=*m++*q.y; r.z+=*m++*q.y; r.w+=*m++*q.y;
r.x+=*m++*q.z; r.y+=*m++*q.z; r.z+=*m++*q.z; r.w+=*m++*q.z;
r.x+=*m++*q.w; r.y+=*m++*q.w; r.z+=*m++*q.w; r.w+=*m  *q.w;
}
else
r=q;
return r;
}