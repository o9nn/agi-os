#define _RESEARCH_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <fcntl.h>
#include <math.h>
#include "comments.h"
#include "gen.h"
#include "ext.h"
typedef struct bbox {
int	set;
double	llx, lly;
double	urx, ury;
} Bbox;
Bbox	bbox = {FALSE, 0.0, 0.0, 0.0, 0.0};
Bbox	docbbox = {FALSE, 0.0, 0.0, 0.0, 0.0};
double	ctm[6] = {1.0, 0.0, 0.0, 1.0, 0.0, 0.0};
double	matrix1[6], matrix2[6];
void	concat(double []);
void	resetbbox(int);
void	rotate(double);
void	scale(double, double);
void	translate(double, double);
void	writebbox(FILE *, char *, int);
void
cover(x, y)
double	x, y;
{
if ( bbox.set == FALSE ) {
bbox.llx = bbox.urx = x;
bbox.lly = bbox.ury = y;
bbox.set = TRUE;
} else {
if ( x < bbox.llx )
bbox.llx = x;
if ( y < bbox.lly )
bbox.lly = y;
if ( x > bbox.urx )
bbox.urx = x;
if ( y > bbox.ury )
bbox.ury = y;
}
}
void
writebbox(fp, keyword, slop)
FILE	*fp;
char	*keyword;
int		slop;
{
Bbox	ubbox;
double	x, y;
if ( strcmp(keyword, BOUNDINGBOX) == 0 )
bbox = docbbox;
if ( bbox.set == TRUE ) {
ubbox = bbox;
bbox.set = FALSE;
x = ctm[0] * ubbox.llx + ctm[2] * ubbox.lly + ctm[4];
y = ctm[1] * ubbox.llx + ctm[3] * ubbox.lly + ctm[5];
cover(x, y);
x = ctm[0] * ubbox.llx + ctm[2] * ubbox.ury + ctm[4];
y = ctm[1] * ubbox.llx + ctm[3] * ubbox.ury + ctm[5];
cover(x, y);
x = ctm[0] * ubbox.urx + ctm[2] * ubbox.ury + ctm[4];
y = ctm[1] * ubbox.urx + ctm[3] * ubbox.ury + ctm[5];
cover(x, y);
x = ctm[0] * ubbox.urx + ctm[2] * ubbox.lly + ctm[4];
y = ctm[1] * ubbox.urx + ctm[3] * ubbox.lly + ctm[5];
cover(x, y);
bbox.llx -= slop + 0.5;
bbox.lly -= slop + 0.5;
bbox.urx += slop + 0.5;
bbox.ury += slop + 0.5;
fprintf(fp, "%s %d %d %d %d\n", keyword, (int)bbox.llx, (int)bbox.lly,
(int)bbox.urx, (int)bbox.ury);
bbox = ubbox;
}
resetbbox(fp == stdout);
}
void
resetbbox(int output)
{
if ( docbbox.set == TRUE ) {
cover(docbbox.llx, docbbox.lly);
cover(docbbox.urx, docbbox.ury);
}
if ( output == TRUE ) {
docbbox = bbox;
docbbox.set = TRUE;
}
bbox.set = FALSE;
}
void
scale(double sx, double sy)
{
matrix1[0] = sx;
matrix1[1] = 0;
matrix1[2] = 0;
matrix1[3] = sy;
matrix1[4] = 0;
matrix1[5] = 0;
concat(matrix1);
}
void
translate(double tx, double ty)
{
matrix1[0] = 1.0;
matrix1[1] = 0.0;
matrix1[2] = 0.0;
matrix1[3] = 1.0;
matrix1[4] = tx;
matrix1[5] = ty;
concat(matrix1);
}
void
rotate(double angle)
{
angle *= M_PI / 180;
matrix1[0] = matrix1[3] = cos(angle);
matrix1[1] = sin(angle);
matrix1[2] = -matrix1[1];
matrix1[4] = 0.0;
matrix1[5] = 0.0;
concat(matrix1);
}
void
concat(double m1[])
{
double	m2[6];
m2[0] = ctm[0];
m2[1] = ctm[1];
m2[2] = ctm[2];
m2[3] = ctm[3];
m2[4] = ctm[4];
m2[5] = ctm[5];
ctm[0] = m1[0] * m2[0] + m1[1] * m2[2];
ctm[1] = m1[0] * m2[1] + m1[1] * m2[3];
ctm[2] = m1[2] * m2[0] + m1[3] * m2[2];
ctm[3] = m1[2] * m2[1] + m1[3] * m2[3];
ctm[4] = m1[4] * m2[0] + m1[5] * m2[2] + m2[4];
ctm[5] = m1[4] * m2[1] + m1[5] * m2[3] + m2[5];
}