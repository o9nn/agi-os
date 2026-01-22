#include <u.h>
#include <libc.h>
#include <draw.h>
#include <geometry.h>
Space *pushmat(Space *t){
Space *v;
v=malloc(sizeof(Space));
if(t==0){
ident(v->t);
ident(v->tinv);
}
else
*v=*t;
v->next=t;
return v;
}
Space *popmat(Space *t){
Space *v;
if(t==0) return 0;
v=t->next;
free(t);
return v;
}
void rot(Space *t, double theta, int axis){
double s=sin(radians(theta)), c=cos(radians(theta));
Matrix m, inv;
register i=(axis+1)%3, j=(axis+2)%3;
ident(m);
m[i][i] = c;
m[i][j] = -s;
m[j][i] = s;
m[j][j] = c;
ident(inv);
inv[i][i] = c;
inv[i][j] = s;
inv[j][i] = -s;
inv[j][j] = c;
ixform(t, m, inv);
}
void qrot(Space *t, Quaternion q){
Matrix m, inv;
int i, j;
qtom(m, q);
for(i=0;i!=4;i++) for(j=0;j!=4;j++) inv[i][j]=m[j][i];
ixform(t, m, inv);
}
void scale(Space *t, double x, double y, double z){
Matrix m, inv;
ident(m);
m[0][0]=x;
m[1][1]=y;
m[2][2]=z;
ident(inv);
inv[0][0]=1/x;
inv[1][1]=1/y;
inv[2][2]=1/z;
ixform(t, m, inv);
}
void move(Space *t, double x, double y, double z){
Matrix m, inv;
ident(m);
m[0][3]=x;
m[1][3]=y;
m[2][3]=z;
ident(inv);
inv[0][3]=-x;
inv[1][3]=-y;
inv[2][3]=-z;
ixform(t, m, inv);
}
void xform(Space *t, Matrix m){
Matrix inv;
if(invertmat(m, inv)==0) return;
ixform(t, m, inv);
}
void ixform(Space *t, Matrix m, Matrix inv){
matmul(t->t, m);
matmulr(t->tinv, inv);
}
void look(Space *t, Point3 e, Point3 l, Point3 u){
Matrix m, inv;
Point3 r;
l=unit3(sub3(l, e));
u=unit3(vrem3(sub3(u, e), l));
r=cross3(l, u);
ident(m);
m[0][0]=r.x; m[0][1]=r.y; m[0][2]=r.z;
m[1][0]=l.x; m[1][1]=l.y; m[1][2]=l.z;
m[2][0]=u.x; m[2][1]=u.y; m[2][2]=u.z;
ident(inv);
inv[0][0]=r.x; inv[0][1]=l.x; inv[0][2]=u.x;
inv[1][0]=r.y; inv[1][1]=l.y; inv[1][2]=u.y;
inv[2][0]=r.z; inv[2][1]=l.z; inv[2][2]=u.z;
ixform(t, m, inv);
move(t, -e.x, -e.y, -e.z);
}
int persp(Space *t, double fov, double n, double f){
Matrix m;
double z;
if(n<=0 || f<=n || fov<=0 || 180<=fov)
return -1;
z=1/tan(radians(fov)/2);
m[0][0]=z; m[0][1]=0; m[0][2]=0; m[0][3]=0;
m[1][0]=0; m[1][1]=(f+n)/(f-n); m[1][2]=0; m[1][3]=f*(1-m[1][1]);
m[2][0]=0; m[2][1]=0; m[2][2]=z; m[2][3]=0;
m[3][0]=0; m[3][1]=1; m[3][2]=0; m[3][3]=0;
xform(t, m);
return 0;
}
void viewport(Space *t, Rectangle r, double aspect){
Matrix m;
double xc, yc, wid, hgt, scale;
xc=.5*(r.min.x+r.max.x);
yc=.5*(r.min.y+r.max.y);
wid=(r.max.x-r.min.x)*aspect;
hgt=r.max.y-r.min.y;
scale=.5*(wid<hgt?wid:hgt);
ident(m);
m[0][0]=scale;
m[0][3]=xc;
m[1][1]=0;
m[1][2]=-scale;
m[1][3]=yc;
m[2][1]=1;
m[2][2]=0;
xform(t, m);
}