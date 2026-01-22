#ifndef gsrect_INCLUDED
#  define gsrect_INCLUDED
#include "gxfixed.h"
#define rect_within(inner, outer)\
((inner).q.y <= (outer).q.y && (inner).q.x <= (outer).q.x &&\
(inner).p.y >= (outer).p.y && (inner).p.x >= (outer).p.x)
#define rect_intersect(to, from)\
BEGIN\
if ((from).p.x > (to).p.x) (to).p.x = (from).p.x;\
if ((from).q.x < (to).q.x) (to).q.x = (from).q.x;\
if ((from).p.y > (to).p.y) (to).p.y = (from).p.y;\
if ((from).q.y < (to).q.y) (to).q.y = (from).q.y;\
END
#define rect_merge(to, from)\
BEGIN\
if ((from).p.x < (to).p.x) (to).p.x = (from).p.x;\
if ((from).q.x > (to).q.x) (to).q.x = (from).q.x;\
if ((from).p.y < (to).p.y) (to).p.y = (from).p.y;\
if ((from).q.y > (to).q.y) (to).q.y = (from).q.y;\
END
int int_rect_difference(gs_int_rect * outer, const gs_int_rect * inner,
gs_int_rect * diffs  );
#define PARALLELOGRAM_IS_RECT(ax, ay, bx, by)\
( ((ax) | (by)) == 0 || ((bx) | (ay)) == 0 )
#define INT_RECT_FROM_PARALLELOGRAM(prect, px, py, ax, ay, bx, by)\
BEGIN\
int px_ = fixed2int_pixround(px);\
int py_ = fixed2int_pixround(py);\
int qx_ = fixed2int_pixround((px) + (ax) + (bx));  \
int qy_ = fixed2int_pixround((py) + (ay) + (by));  \
\
if (qx_ < px_)\
(prect)->p.x = qx_, (prect)->q.x = px_;\
else\
(prect)->p.x = px_, (prect)->q.x = qx_;\
if (qy_ < py_)\
(prect)->p.y = qy_, (prect)->q.y = py_;\
else\
(prect)->p.y = py_, (prect)->q.y = qy_;\
END
#endif